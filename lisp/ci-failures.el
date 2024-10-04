;;; ci-failures.el --- Display Buildkite CI failures -*- lexical-binding: t; -*-

;; Author: Your Name <your.email@example.com>
;; Version: 0.8
;; Package-Requires: ((emacs "24.4"))
;; Keywords: tools, ci, buildkite
;; URL: https://github.com/yourusername/ci-failures

;;; Commentary:

;; This package provides a command `show-ci-failures` which runs the `ci_failures`
;; script (now with `--format flat` argument) and displays the failure logs incrementally
;; in a compilation-mode buffer.

;; Since the Python script now handles the formatting and cleaning, the Elisp code
;; simply runs the command and displays its output, applying ANSI color processing.

;;; Code:

(require 'ansi-color)
(require 'compile)

(defvar ci-failures-process nil
  "Process handle for the `ci_failures` command.")

(defvar ci-failures-output-buffer "*CI Failures*"
  "Buffer name for displaying CI failures.")

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
      (compilation-mode)))
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

(defun ci-failures-process-filter (proc output)
  "Process filter for handling output from `ci_failures` command.
PROC is the process, and OUTPUT is the string output."
  (with-current-buffer ci-failures-output-buffer
    (let ((inhibit-read-only t)
          (moving (= (point) (process-mark proc))))
      ;; Insert the new output
      (goto-char (process-mark proc))
      (insert output)
      (set-marker (process-mark proc) (point))
      ;; Apply ANSI color codes
      (ansi-color-apply-on-region (point-min) (point-max))
      ;; Keep the buffer at the end if it was already there
      (when moving
        (goto-char (process-mark proc))))))

(defun ci-failures-process-sentinel (proc event)
  "Handle the process PROC state change, with EVENT description."
  (when (memq (process-status proc) '(exit signal))
    (let ((exit-code (process-exit-status proc)))
      (if (= exit-code 0)
          ;; Success; move to the min-point in buffer.
          (progn
            (with-current-buffer ci-failures-output-buffer
              (goto-char (point-min)))
            (message "ci_failures command completed successfully."))
        (message "ci_failures command failed with exit code %d" exit-code)))))

(provide 'ci-failures)

;;; ci-failures.el ends here
