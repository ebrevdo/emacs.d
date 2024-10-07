;;; ci-failures.el --- Display Buildkite CI failures -*- lexical-binding: t; -*-

;; Author: Your Name <your.email@example.com>
;; Version: 1.6
;; Package-Requires: ((emacs "24.4"))
;; Keywords: tools, ci, buildkite
;; URL: https://github.com/yourusername/ci-failures

;;; Commentary:

;; This package provides a command `ci-failures-show` which runs the `ci_failures`
;; script and displays the failure logs incrementally in a compilation-mode buffer.
;; Job sections can be collapsed and expanded using custom folding overlays.
;; Pressing RET or TAB on a Job header toggles the section; pressing RET on an error jumps to it.
;; M-n and M-p navigate to the next or previous error or Job header.

;;; Code:

(require 'ansi-color)
(require 'compile)
(require 'cl-lib)  ;; Required for cl-lib functions like cl-find-if

(defvar ci-failures-process nil
  "Process handle for the `ci_failures` command.")

(defvar ci-failures-output-buffer "*CI Failures*"
  "Buffer name for displaying CI failures.")

(defvar-local ci-failures-fold-overlays nil
  "List of overlays used for folding in ci-failures buffer.")

(defconst ci-failures-invisible 'ci-failures-hidden
  "Symbol used for making text invisible in ci-failures buffer.")

(defun ci-failures-show ()
  "Run the `ci_failures` command asynchronously and display its output incrementally."
  (interactive)
  ;; Kill any existing process
  (when (and ci-failures-process (process-live-p ci-failures-process))
    (kill-process ci-failures-process))
  ;; Create or clear the output buffer
  (with-current-buffer (get-buffer-create ci-failures-output-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (compilation-mode)
      ;; Set up custom folding
      (ci-failures-setup-folding)
      ;; Override RET and TAB keys to handle folding
      (define-key (current-local-map) (kbd "RET") 'ci-failures-return)
      (define-key (current-local-map) (kbd "TAB") 'ci-failures-tab)
      ;; Override M-n and M-p for custom navigation
      (define-key (current-local-map) (kbd "M-n") 'ci-failures-next-error-or-header)
      (define-key (current-local-map) (kbd "M-p") 'ci-failures-previous-error-or-header)))
  ;; Start the asynchronous process
  (let ((process-connection-type nil))  ; Use a pipe, not a pty
    (setq ci-failures-process
          (start-process "ci-failures-process" ci-failures-output-buffer
                         "ci_failures" "--format" "flat")))
  ;; Set up the process filter
  (set-process-filter ci-failures-process 'ci-failures-process-filter)
  ;; Set up the process sentinel to handle process exit
  (set-process-sentinel ci-failures-process 'ci-failures-process-sentinel)
  ;; Display the output buffer
  (pop-to-buffer ci-failures-output-buffer)
  ;; Inform the user
  (message "Running ci_failures..."))

(defun ci-failures-setup-folding ()
  "Set up custom folding in the CI failures buffer."
  ;; Initialize the list of overlays
  (setq ci-failures-fold-overlays nil)
  ;; Add symbol to invisibility spec
  (add-to-invisibility-spec ci-failures-invisible)
  ;; Add hook to process new output
  (add-hook 'compilation-filter-hook #'ci-failures-folding-hook nil t)
  ;; Debug message
  (message "Debug: Folding setup complete"))

(defun ci-failures-folding-hook ()
  "Process buffer to create folding overlays."
  (save-excursion
    (let ((inhibit-read-only t)
          (start (or compilation-filter-start (point-min)))
          (end (point-max)))
      ;; Debug message
      (message "Debug: Processing folding from %d to %d" start end)
      (goto-char start)
      ;; Process new Job headers
      (while (re-search-forward "^Job:.*" end t)
        (let ((header-start (line-beginning-position))
              (header-end (line-end-position)))
          ;; Debug message
          (message "Debug: Found Job header at %d" header-start)
          ;; Optionally, make the Job header bold
          (add-text-properties header-start header-end '(face bold))
          ;; Add properties for detection
          (put-text-property header-start header-end 'ci-failures-header t)
          ;; Create the overlay for folding
          (let ((content-start (min (1+ header-end) end))
                (content-end (save-excursion
                               (if (re-search-forward "^Job:.*" end t)
                                   (match-beginning 0)
                                 end))))
            ;; Debug message
            (message "Debug: Creating overlay from %d to %d" content-start content-end)
            (when (< content-start content-end)
              (let ((ov (make-overlay content-start content-end)))
                (overlay-put ov 'invisible ci-failures-invisible)
                (overlay-put ov 'ci-failures-overlay t)
                (overlay-put ov 'ci-failures-header-pos header-start)
                (push ov ci-failures-fold-overlays)
                ;; Debug message
                (message "Debug: Overlay created from %d to %d" content-start content-end)))))))))

(defun ci-failures-toggle-section ()
  "Toggle the folding of the current Job section."
  (interactive)
  (let ((header-pos (line-beginning-position))
        (inhibit-read-only t))
    (if (get-text-property header-pos 'ci-failures-header)
        (let ((ov (cl-find-if (lambda (o)
                                (and (overlay-get o 'ci-failures-overlay)
                                     (= (overlay-get o 'ci-failures-header-pos) header-pos)))
                              ci-failures-fold-overlays)))
          (if ov
              (progn
                ;; Debug message
                (message "Debug: Toggling overlay from %d to %d" (overlay-start ov) (overlay-end ov))
                (overlay-put ov 'invisible (not (overlay-get ov 'invisible))))
            (message "No overlay found for this section")))
      (message "Not on a Job header line"))))

(defun ci-failures-return ()
  "Handle RET key in ci-failures buffer.
If on a Job header line, toggle folding.
Otherwise, call `compile-goto-error`."
  (interactive)
  (if (get-text-property (line-beginning-position) 'ci-failures-header)
      (ci-failures-toggle-section)
    (compile-goto-error)))

(defun ci-failures-tab ()
  "Handle TAB key in ci-failures buffer.
If on a Job header line, toggle folding.
Otherwise, call the default TAB action."
  (interactive)
  (if (get-text-property (line-beginning-position) 'ci-failures-header)
      (ci-failures-toggle-section)
    (call-interactively 'indent-for-tab-command)))

(defun ci-failures-next-error-or-header ()
  "Jump to the next error or Job header, whichever is closer."
  (interactive)
  (let ((next-error-pos (save-excursion
                          (compilation-next-error 1)
                          (point)))
        (next-header-pos (let ((pos (next-single-property-change (point) 'ci-failures-header)))
                           (when pos
                             pos))))
    (cond
     ((and next-error-pos next-header-pos)
      (if (< next-error-pos next-header-pos)
          (goto-char next-error-pos)
        (goto-char next-header-pos)))
     (next-error-pos (goto-char next-error-pos))
     (next-header-pos (goto-char next-header-pos))
     (t (message "No further errors or headers")))))

(defun ci-failures-previous-error-or-header ()
  "Jump to the previous error or Job header, whichever is closer."
  (interactive)
  (let ((prev-error-pos (save-excursion
                          (compilation-previous-error 1)
                          (point)))
        (prev-header-pos (let ((pos (previous-single-property-change (point) 'ci-failures-header)))
                           (when pos
                             pos))))
    (cond
     ((and prev-error-pos prev-header-pos)
      (if (> prev-error-pos prev-header-pos)
          (goto-char prev-error-pos)
        (goto-char prev-header-pos)))
     (prev-error-pos (goto-char prev-error-pos))
     (prev-header-pos (goto-char prev-header-pos))
     (t (message "No previous errors or headers")))))

(defun ci-failures-process-filter (proc output)
  "Process filter for handling output from `ci_failures` command.
PROC is the process, and OUTPUT is the string output."
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t)
          (moving (= (point) (process-mark proc)))
          (compilation-filter-start (marker-position (process-mark proc))))
      ;; Insert the new output
      (goto-char (process-mark proc))
      (insert output)
      (set-marker (process-mark proc) (point))
      ;; Apply ANSI color codes
      (ansi-color-apply-on-region compilation-filter-start (point))
      ;; Keep the buffer at the end if it was already there
      (when moving
        (goto-char (process-mark proc))))))

(defun ci-failures-process-sentinel (proc event)
  "Handle the process PROC state change, with EVENT description."
  (when (memq (process-status proc) '(exit signal))
    (let ((exit-code (process-exit-status proc)))
      (if (= exit-code 0)
          (message "ci_failures command completed successfully.")
        (message "ci_failures command failed with exit code %d" exit-code)))))

(provide 'ci-failures)

;;; ci-failures.el ends here
