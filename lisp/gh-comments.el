;;; gh-comments.el --- Display GitHub PR comments in Emacs -*- lexical-binding: t; -*-

;; Version: 1.6
;; Package-Requires: ((emacs "24.4"))
;; Keywords: tools, github
;; URL: https://github.com/ebrevdo/gh-comments

;;; Commentary:

;; This package provides functionality to display GitHub pull request
;; comments in an Emacs buffer, allowing easy navigation to the
;; associated lines in the source code. It fetches comments using the
;; GitHub REST API via the `gh` CLI, grouping comments by conversation,
;; filtering out resolved conversations, and formatting entries to match
;; `compilation-mode` requirements.

;;; Code:

(require 'compile)
(require 'json)
(require 'diff-mode)
(require 'seq)
(require 'cl-lib)
(require 'subr-x)  ;; For string-join and string-trim
(require 'let-alist)

(defgroup gh-comments nil
  "GitHub PR comments display and navigation."
  :group 'tools)

(defcustom gh-comments-gh-executable "gh"
  "Path to the GitHub CLI executable."
  :type 'string
  :group 'gh-comments)

(defcustom gh-comments-git-executable "git"
  "Path to the Git executable."
  :type 'string
  :group 'gh-comments)

(defface gh-comments-metadata-face
  '((t :inherit font-lock-comment-face))
  "Face for comment metadata (user and date)."
  :group 'gh-comments)

(defface gh-comments-body-face
  '((t :inherit font-lock-doc-face))
  "Face for comment body text."
  :group 'gh-comments)

(defface gh-comments-diff-added
  '((t :foreground "green"))
  "Face for added lines in diffs."
  :group 'gh-comments)

(defface gh-comments-diff-removed
  '((t :foreground "red"))
  "Face for removed lines in diffs."
  :group 'gh-comments)

(defun gh-comments-get-root-dir ()
  "Get the root directory of the current Git repository."
  (or (vc-root-dir)
      (error "Not inside a Git repository")))

(defun gh-comments-get-repo-info ()
  "Get the current repository owner and name."
  (let* ((default-directory (gh-comments-get-root-dir))
         (remote-url (string-trim
                      (shell-command-to-string
                       (format "%s config --get remote.origin.url"
                               gh-comments-git-executable))))
         (repo-parts (split-string
                      (replace-regexp-in-string "^.+github\\.com[:/]" ""
                                                (replace-regexp-in-string "\\.git$" "" remote-url))
                      "/")))
    (cons (car repo-parts) (cadr repo-parts))))

(defun gh-comments-get-current-pr ()
  "Get the current PR number, or nil if not in a PR."
  (let ((default-directory (gh-comments-get-root-dir)))
    (with-temp-buffer
      (let ((exit-code (call-process gh-comments-gh-executable
                                     nil (list t nil) nil
                                     "pr" "view" "--json" "number" "--jq" ".number")))
        (if (eq exit-code 0)
            (string-trim (buffer-string))
          nil)))))

(defun gh-comments-get-comments (owner repo pr-number)
  "Get review comments for the specified PR."
  (let* ((default-directory (gh-comments-get-root-dir))
         (pr-number (if (numberp pr-number)
                        pr-number
                      (string-to-number pr-number)))
         (api-endpoint (format "repos/%s/%s/pulls/%d/comments"
                               owner repo pr-number))
         (command (list gh-comments-gh-executable "api" api-endpoint
                        "--paginate" "--cache" "30m")))
    ;; Execute the command and capture output and errors
    (with-temp-buffer
      (let* ((exit-code (apply #'call-process
                               (car command) nil (list t t) nil (cdr command)))
             (output (buffer-string)))
        (if (/= exit-code 0)
            ;; On error, include the command and output in the error message
            (let ((error-message
                   (format "Command: %s\n\nError Output:\n%s"
                           (string-join command " ")
                           output)))
              (error error-message))
          (let ((json-string output))
            ;; Parse the JSON
            (condition-case err
                (json-parse-string json-string
                                   :object-type 'alist
                                   :array-type 'list)
              (json-parse-error
               ;; Include JSON string in error message
               (let ((error-message
                      (format "Failed to parse JSON.\n\nCommand: %s\n\nJSON String:\n%s\n\nError:\n%s"
                              (string-join command " ")
                              json-string
                              (error-message-string err))))
                 (error error-message))))))))))

(defun gh-comments-build-threads (comments)
  "Group comments into threads based on `in_reply_to_id`."
  (let ((comment-id-map (make-hash-table :test 'equal))
        (threads (make-hash-table :test 'equal)))
    ;; First, map comment IDs to comments
    (dolist (comment comments)
      (puthash (alist-get 'id comment) comment comment-id-map))
    ;; Build threads
    (dolist (comment comments)
      (let ((in-reply-to (alist-get 'in_reply_to_id comment)))
        (if in-reply-to
            (let* ((parent-comment (gethash in-reply-to comment-id-map))
                   (thread-id (alist-get 'id (or parent-comment comment))))
              (puthash thread-id
                       (cons comment (gethash thread-id threads))
                       threads))
          ;; Top-level comment, starts a new thread
          (let ((thread-id (alist-get 'id comment)))
            (puthash thread-id (list comment) threads)))))
    ;; Return threads as a list
    (hash-table-values threads)))

(defun gh-comments-filter-resolved-threads (threads)
  "Filter out resolved threads based on the presence of line numbers."
  (seq-filter
   (lambda (thread)
     (let* ((first-comment (car (last thread)))  ;; First comment is at the end after build
            (raw-line-number (alist-get 'line first-comment))
            ;; Handle :null values
            (line-number (if (eq raw-line-number :null) nil raw-line-number)))
       ;; Include thread if the line-number is a number
       (numberp line-number)
       ))
   threads))

(defun gh-comments-colorize-diff-line (line)
  "Return the diff line as-is; coloring is handled by font-lock."
  line)

(defun gh-comments-colorize-diff-hunk (hunk)
  "Prepare diff hunk for display."
  hunk)

(defun gh-comments-format-thread (thread)
  "Format a single thread for display."
  ;; THREAD is a list of comments in reverse order (newest first)
  (let* ((sorted-comments (reverse thread))
         (first-comment (car sorted-comments))
         (relative-path (or (alist-get 'path first-comment) "[unknown file]"))
         (absolute-path (expand-file-name relative-path (gh-comments-get-root-dir)))
         (raw-line-number (or (alist-get 'line first-comment)
                              (alist-get 'original_line first-comment)))
         ;; Handle :null values by converting them to nil
         (line-number (if (eq raw-line-number :null) nil raw-line-number))
         (line-number-str (if (numberp line-number)
                              (number-to-string line-number)
                            ""))
         (diff-hunk (or (alist-get 'diff_hunk first-comment) "[No diff hunk available]"))
         ;; No need to colorize diff hunk here
         (formatted-diff diff-hunk)
         (formatted-comments
          (mapconcat
           (lambda (comment)
             (let-alist comment
               (format "%s %s\n%s\n"
                       (propertize (or (alist-get 'login .user) "[unknown user]")
                                   'font-lock-face 'gh-comments-metadata-face)
                       (propertize (or .created_at "[unknown date]")
                                   'font-lock-face 'gh-comments-metadata-face)
                       (propertize (or .body "[empty comment]")
                                   'font-lock-face 'gh-comments-body-face))))
           sorted-comments
           "\n")))
    ;; Format according to compilation-mode requirements
    (format "%s:%s:\n%s\n%s\n\n"
            absolute-path
            line-number-str
            formatted-diff
            formatted-comments)))

(defun gh-comments-get-buffer-name (owner repo pr-number)
  "Generate the buffer name for PR comments."
  (format "*GitHub PR Comments: %s/%s PR#%s*" owner repo pr-number))

(defun gh-comments-display-comments (owner repo pr-number threads)
  "Display the threads in a compilation-mode buffer."
  (let ((buffer-name (gh-comments-get-buffer-name owner repo pr-number)))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (compilation-mode)
        ;; Add custom font-lock keywords for diff lines
        (font-lock-add-keywords nil
          '(("^\\(+.*\\)$" 1 'gh-comments-diff-added)
            ("^\\(-.*\\)$" 1 'gh-comments-diff-removed)
            ("^\\(@@.*@@\\)$" 1 'diff-hunk-header)))
        ;; Re-initialize font-lock to apply the new keywords
        (font-lock-flush)
        ;; Insert the comments
        (insert (format "GitHub Pull Request Comments for %s/%s PR#%s:\n\n"
                        owner repo pr-number))
        (dolist (thread threads)
          (insert (gh-comments-format-thread thread)))
        (goto-char (point-min))))
    (display-buffer buffer-name)))

(defun gh-comments-display-error (error-message)
  "Display ERROR-MESSAGE in a dedicated buffer."
  (let ((buffer-name "*gh-comments Errors*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert error-message)
        (special-mode)))
    (display-buffer buffer-name)))

;;;###autoload
(defun gh-comments-show (&optional pr-number)
  "Show GitHub PR comments for the current repository and PR.

If PR-NUMBER is not provided, attempt to determine it automatically."
  (interactive)
  (let* ((repo-info (gh-comments-get-repo-info))
         (owner (car repo-info))
         (repo (cdr repo-info))
         (pr-number (or pr-number (gh-comments-get-current-pr))))
    (unless pr-number
      (setq pr-number (read-string "Enter PR number: ")))
    (if (string-match-p "^[0-9]+$" pr-number)
        (condition-case err
            (let* ((comments (gh-comments-get-comments owner repo (string-to-number pr-number)))
                   (threads (gh-comments-build-threads comments))
                   (filtered-threads (gh-comments-filter-resolved-threads threads)))
              (if filtered-threads
                  (gh-comments-display-comments owner repo pr-number filtered-threads)
                (message "No unresolved comments found for PR #%s." pr-number)))
          (error
           (gh-comments-display-error (error-message-string err))))
      (message "Invalid PR number: %s" pr-number))))

(provide 'gh-comments)

;;; gh-comments.el ends here
