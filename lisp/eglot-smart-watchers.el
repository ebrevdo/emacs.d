;;; eglot-smart-watchers.el --- Smarter, incremental file-watches for Pyright  -*- lexical-binding:t; -*-
;;; Commentary:

(require 'cl-lib)
(require 'seq)
(require 'eglot)                     ;needed at compile time

;; ------------------------------------------------------------------
;; Compatibility with Eglot ≥ 1.17  (eglot--maybe-watch was removed)
;; ------------------------------------------------------------------
(eval-when-compile (require 'eglot))          ; silence byte-compiler

(defun nuance--recreate-watchers (server &rest _ignore)  ; <-- changed
  "Re-install :workspace/didChangeWatchedFiles watches for SERVER."
  ;; 1. Drop every low-level file-notify watch that is still active.
  (maphash (lambda (_dir watch+ids)
             (file-notify-rm-watch (car watch+ids)))
           (eglot--file-watches server))
  (clrhash (eglot--file-watches server))

  ;; 2. Register the (already-tweaked) watcher list again.
  (let ((id (format "nuance-%s" (float-time)))) ; fresh registration id
    (eglot-register-capability
     server 'workspace/didChangeWatchedFiles id
     :watchers (nuance--tweak-watchers nil))))

(unless (fboundp 'eglot--maybe-watch)
  (defalias 'eglot--maybe-watch #'nuance--recreate-watchers))

;; If the helper is gone in the version of Eglot we are running,
;; publish the compatibility shim under that name, so the rest of
;; this file continues to work unmodified.
(unless (fboundp 'eglot--maybe-watch)
  (defalias 'eglot--maybe-watch #'nuance--recreate-watchers))

(use-package eglot
  :config

  ;; ------------------------------------------------------------
  ;; Helpers
  ;; ------------------------------------------------------------
  (defun nuance--pyright-server-p (server)
    "Return non-nil if SERVER is a Pyright instance."
    (seq-some (lambda (elt)
                (string-match-p "\\bpyright\\>" elt))
              (process-command (jsonrpc--process server))))

  (defun nuance--managed-file-list (server)
    (seq-filter #'identity
                (mapcar #'buffer-file-name (eglot--managed-buffers server))))

  (defun nuance--project-root (server)
    (car (project-roots (eglot--project server))))

  (defun nuance--dirs-to-watch (server)
    "Directories that must be watched for SERVER, deduped."
    (let* ((root (file-truename
                  (file-name-as-directory (nuance--project-root server))))
           (seen (make-hash-table :test #'equal))
           dirs)
      (dolist (file (nuance--managed-file-list server))
        (let ((dir (file-name-directory file)))
          (while (and dir (string-prefix-p root dir))
            (unless (gethash dir seen)
              (push dir dirs)
              (puthash dir t seen))
            (setq dir (unless (string= dir root)
                        (file-name-directory
                         (directory-file-name dir)))))))
      dirs))

  ;; ------------------------------------------------------------
  ;; Restrict project-files to already-open buffers for Pyright
  ;; ------------------------------------------------------------
  (defvar-local nuance--current-pyright-server nil)

  (defun nuance--project-files-open-only (orig project &rest args)
    (if (and nuance--current-pyright-server
             (nuance--pyright-server-p nuance--current-pyright-server))
        (let ((open (mapcar #'file-truename
                            (nuance--managed-file-list
                             nuance--current-pyright-server))))
          (seq-intersection (apply orig project args) open #'string-equal))
      (apply orig project args)))

  ;; ------------------------------------------------------------
  ;; Replace Pyright’s own watch request
  ;; ------------------------------------------------------------
  (defun nuance--tweak-watchers (_orig)
    (vconcat
     (list '(:globPattern "*.py"              :kind 7)
           '(:globPattern "pyrightconfig.json" :kind 7))))

  (defun nuance--limit-pyright-watches (orig server method id &rest args)
    (if (and (eq method 'workspace/didChangeWatchedFiles)
             (nuance--pyright-server-p server))
        (let* ((nuance--current-pyright-server server)
               (args (plist-put (copy-sequence args)
                                :watchers (nuance--tweak-watchers
                                           (plist-get args :watchers))))
               (dirs (nuance--dirs-to-watch server)))
          (advice-add 'project-files :around #'nuance--project-files-open-only)
          (cl-letf* ((orig-project-files (symbol-function 'project-files))
                     ((symbol-function 'project-files)
                      (lambda (project &rest rest)
                        (seq-remove
                         (lambda (f)
                           (not (member (file-name-directory f) dirs)))
                         (apply orig-project-files project rest)))))
            (unwind-protect
                (apply orig server method id args)
              (advice-remove 'project-files
                             #'nuance--project-files-open-only))))
      (apply orig server method id args)))

  (advice-add 'eglot-register-capability :around #'nuance--limit-pyright-watches)

  ;; ------------------------------------------------------------
  ;; Incremental refresh of file-notify watches
  ;; ------------------------------------------------------------
  (defvar nuance--pyright-dirs-cache (make-hash-table :test #'eq)
    "SERVER → last directory list we installed.")

  (defun nuance--pyright-current-dirs (server)
    (sort (nuance--dirs-to-watch server) #'string<))

  (defun nuance--python-buffer-p ()
    (derived-mode-p 'python-mode))

  (defun nuance--pyright-refresh-watchers (&rest _ignore)
    "Refresh watches for the current Pyright SERVER, if needed."
    (when-let* ((server (eglot-current-server))
                ((nuance--pyright-server-p server)))
      (let* ((new (nuance--pyright-current-dirs server))
             (old (gethash server nuance--pyright-dirs-cache)))
        (unless (equal new old)
          (puthash server new nuance--pyright-dirs-cache)
          (let ((dirs new)
                (nuance--current-pyright-server server))
            (cl-letf* ((orig-project-files (symbol-function 'project-files))
                       ((symbol-function 'project-files)
                        (lambda (project &rest rest)
                          (seq-remove
                           (lambda (f)
                             (not (member (file-name-directory f) dirs)))
                           (apply orig-project-files project rest)))))
              (eglot--maybe-watch server t))))))
    nil)

  ;; Trigger refresh only for Python buffers
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (nuance--python-buffer-p)
                (nuance--pyright-refresh-watchers))))

  (add-hook 'kill-buffer-hook
            (lambda ()
              (when (nuance--python-buffer-p)
                (nuance--pyright-refresh-watchers)))))

(provide 'eglot-smart-watchers)
;;; eglot-smart-watchers.el ends here
