(require 'package)
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
  (add-to-list 'package-archives '("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/"))
)

;; Disable menu bar
(menu-bar-mode -1)

;; Enable xterm-mouse-mode
;; (xterm-mouse-mode 1)

;; This allows us to then call `use-package-report` to profile startup.


;; Move eln-cache to $HOME/.emacs-eln-cache
(setq native-comp-eln-load-path (list (expand-file-name "~/.eln-cache-emacs/")))

;; Add all subdirectories of "~/.emacs.d/lisp" to the load path
(add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aibo:model "gpt-4o")
 '(custom-safe-themes
   '("061cf8206a054f0fd0ecd747e226608302953edf9f24663b10e6056ab783419f"
     "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" default))
 '(custom-theme-directory "~/.emacs.d/lisp/themes")
 '(package-selected-packages nil))

;; Ensure all selected packages are actually installed.  It's not sufficient to just refresh contents!
(unless (cl-every #'package-installed-p package-selected-packages)
  (package-install-selected-packages))

;; Stop asking if it's OK to kill processes on exit.
(setq confirm-kill-processes nil)

;; Always follow symlinks into git-controlled files.
(setq vc-follow-symlinks t)

;; Simple "y" or "n" to answer yes/no prompts.
(defalias 'yes-or-no-p 'y-or-n-p)
;; Even at exit when modified buffers exist.
;;(setq confirm-kill-emacs 'y-or-n-p)
;; Don't ask for exit confirmation unless there are unsaved changes.
(setq confirm-kill-emacs nil)

;; Don't use TABS for indentations.
(setq-default indent-tabs-mode nil)
;; Set the number to the number of columns to use.
(setq-default fill-column 100)
;; Add Autofill mode to mode hooks.
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; Show line number in the mode line.
(line-number-mode 1)
;; Show column number in the mode line.
(column-number-mode 1)
;; Highlight current row in all buffers.
(global-hl-line-mode 1)
;; Show matching parentheses.
(show-paren-mode 1)

;; Highlight the fill column
(require 'hl-fill-column)
(global-hl-fill-column-mode)

;; Share clipboard with system using super+{c,v,x}
;; Enable simpleclip
(use-package simpleclip
  :ensure t
  :custom
  ;; Don't set any keybindings for simpleclip
  (simpleclip-cut-keystrokes '())
  (simpleclip-copy-keystrokes '())
  (simpleclip-paste-keystrokes '())
  :config
  ;; Initialize simpleclip
  (simpleclip-mode 1)

  ;; Advice for `kill-new` to copy to system clipboard
  (defun my-kill-new-to-clipboard (string &optional replace)
    "Copy STRING to the system clipboard using simpleclip."
    (when (and string (stringp string)) ;; Ensure string is valid
      (simpleclip-set-contents string))) ;; Copy to system clipboard
  (advice-add 'kill-new :after #'my-kill-new-to-clipboard)

  (defun my-kill-region-to-clipboard (beg end)
    "Copy the most recent kill from the kill ring to the clipboard."
    (let ((killed-text (current-kill 0 t)))  ;; Retrieve the latest kill without moving the mark
      (when (and killed-text (stringp killed-text))
        (simpleclip-set-contents killed-text))))
  (advice-add 'kill-region :after #'my-kill-region-to-clipboard)

  ;; Advice for `yank` to pull text from the system clipboard
  (defun my-yank-from-clipboard (&rest _args)
    "Ensure yank also integrates with the system clipboard via simpleclip."
    (let ((clipboard-text (simpleclip-get-contents)))
      (when (and clipboard-text (stringp clipboard-text)) ;; Ensure text is valid
        ;; make sure that clipboard-text does not match the last kill
        (unless (string= clipboard-text (car kill-ring))
          (kill-new clipboard-text))))) ;; Sync clipboard content to kill ring
  (advice-add 'yank :before #'my-yank-from-clipboard))

;; whitespace-mode
;; free of trailing whitespace and to use 100-column width, standard indentation
(setq whitespace-style '(trailing lines space-before-tab indentation space-after-tab lines-tail)
      whitespace-line-column 100)
(global-whitespace-mode 1)

;; Always delete empty whitespace at end of lines when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Load copilot-emacsd
(load-file "~/.emacs.d/copilot-init.el")

;; Load openai chatbot
;; (add-to-list 'load-path "~/.emacs.d/lisp/aibo/elisp")
;; (require 'aibo)
;; ; Keybindings
;; (global-set-key (kbd "C-M-h") 'aibo:homepage)
;; (global-set-key (kbd "C-M-s") 'aibo:message-search)
;; ; Control+Meta+/ to create a new conversation
;; (global-set-key (kbd "C-M-/") 'aibo:create-conversation)
;; ; M-I to create a new conversation
;; (global-set-key (kbd "M-I") 'aibo:create-conversation)
;; ;; Hide `aibo` buffers from Ivy by adding the following:
;; (add-to-list 'ivy-ignore-buffers "\\*Aibo")
;; ;; Start openai chatbot server
;; (aibo:start-server)

;; Load paredit in lisp/elisp configurations
(add-hook 'emacs-lisp-mode-hook
          #'paredit-mode)

;; Load pyvenv and eglot, ruff, black, etc.
;; To install associated python LSP server:
;;   pip install --upgrade 'python-lsp-server[all]' python-lsp-black python-lsp-ruff
;; Enable LSP support by default in programming buffers
(add-hook 'prog-mode-hook #'eglot-ensure)
;; Create a memorable alias for `eglot-ensure'.
(defalias 'start-lsp-server #'eglot)
;; Enabled inline static analysis
(add-hook 'prog-mode-hook #'flymake-mode)
(require 'pyvenv)
(add-hook 'python-mode-hook 'pyvenv-mode)
(add-hook 'eglot-managed-mode-hook 'pyvenv-mode)
(add-hook 'eglot-managed-mode-hook
	  (lambda ()
	    (add-hook 'flymake-diagnostic-functions #'python-flymake t t))
	  nil t)

;; Autoformat on save
;; Load blacken and set all the settings in one go
(require 'blacken)
(add-hook 'python-mode-hook 'blacken-mode)
;; Set blacken to 100 char line limit
(setq blacken-line-length 100)
(setq blacken-use-pyproject-toml t)
;; Set blacken to only run in a repo with a pyproject.toml
(setq blacken-only-if-project-is-blackened t)

;; Also ruff fix on save
(require 'ruff-fix)
(require 'python-isort)

(defun absolute-default-directory ()
  "Return the absolute path of the default directory.  Replace ~ with homedir."
  (expand-file-name default-directory))

;; set python-isort-arguments; extract line-length from
(setq python-isort-arguments
      (append
       (list (concat "--line-length=" (number-to-string blacken-line-length))
             (if (locate-dominating-file default-directory ".isort.cfg")
                 (concat
                  "--settings-path="
                  (expand-file-name (locate-dominating-file default-directory ".isort.cfg"))
                  ".isort.cfg")
               (if (locate-dominating-file default-directory "openai/.isort.cfg")
                   (concat
                    "--settings-path="
                    (expand-file-name (locate-dominating-file default-directory "openai/.isort.cfg"))
                    "openai/.isort.cfg")
                 "")))
       ;; Add the rest of the arguments
       '("--profile=black" "--stdout" "-")))

(defun python-format-buffer-on-save ()
  "Run `blacken-buffer` and `ruff-fix-before-save` as `before-save-hook`."
  ;; First run blacken-buffer
  (add-hook 'before-save-hook #'blacken-buffer -10 t)
  ;; Then run isort-buffer
  (add-hook 'before-save-hook #'python-isort-buffer -9 t)
  ;; Then run ruff-fix-before-save
  (add-hook 'before-save-hook #'ruff-fix-before-save -8 t))
(add-hook 'python-mode-hook #'python-format-buffer-on-save)


;; Use python-pytest
(require 'python-pytest)
(setq python-pytest-executable "python -m pytest -vv")

;; Configure company mode
(use-package company
  :ensure t
  :init (global-company-mode)
  :bind ("M-/" . company-complete-common) ; Use Meta+/ to perform completion
  )

;; Configure consult mode.  Note this doesn't work with python-lsp-server as of writing,
;; and that pyright lsp server doesn't seem to work with eglot.
(require 'consult-eglot)


;; Enable which-func-mode
(which-function-mode 1)
;; Enable which-func-mode
(setq which-func-unknown "?")
;; Remove the which-function-mode from the mode-line-misc-info; since it's now in
;; buffer-identification.
(setq-default
 mode-line-misc-info
 (assq-delete-all 'which-function-mode mode-line-misc-info))
;; Modify mode-line format: add which-function-mode just after the line/column info.
;; First find the line/column (position) location, then insert
(let ((position (memq 'mode-line-position mode-line-format)))
  (setcdr position
          (cons 'which-func-format
                (cdr position))))


;; fuzzy completion in minibuffer, etc
;; Enable vertico

;; Function like vertico-next, but cycle to beginning if at end.
(defun vertico-next-cycle ()
  "Move to the next candidate, or cycle to the first candidate."
  (interactive)
  (if (= vertico--index (1- (length vertico--candidates)))
      (vertico--goto 0)
    (vertico-next)))

;; Function that performs vertico-insert if there's just one candidate,
;; otherwise it performs vertico-next.  However, if my last keyboard command was
;; was vertico-insert, then it performs vertico-next.

(defun vertico-insert-or-next ()
  "Insert the candidate if there's only one, otherwise go to the next candidate."
  (interactive)
  (if (eq last-command 'vertico-insert)
      (vertico-next-cycle)
    (let ((required-candidates 1))
      (if (<= (length vertico--candidates) required-candidates)
          (vertico-insert)
        (vertico-next-cycle))
      )))

;; Function for when pressing ENTER in vertico directory mode.  If the candidate is a directory,
;; then insert it instead of opening it.
(defun vertico-directory-insert-or-open ()
  "Insert the candidate if it's a directory, otherwise open it."
  (interactive)
  (if (file-directory-p (vertico--candidate))
      (vertico-insert)
    (vertico-exit)))

(use-package vertico
  :init
  (vertico-mode)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)
  ;; Show more candidates
  ;; (setq vertico-count 5)
  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)

  ;; Vertico tab iterates through candidates
  (keymap-set vertico-map "TAB" #'vertico-insert-or-next)
  ;; Vertico shift+tab iterates backwards through candidates
  ;; (keymap-set vertico-map "<backtab>" #'previous-line)
  )

(use-package vertico-directory
  :after vertico
  (vertico-directory-mode))

  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (use-package savehist
    :init
    (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(substring orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Color theme loading
(require 'kaolin-themes)
; Current fav theme: haki.
(load-theme 'haki t)
(set-face-attribute 'haki-region nil :background "#2e8b57" :foreground "#ffffff")

;; Diminish all minor modes
(defun diminish-most-modes (unused)
  (dolist (mode minor-mode-list) (diminish mode)))

(add-to-list 'after-load-functions 'diminish-most-modes)

;; Allow opening jupyter notebooks
;; Need to install jupyterlab:
;;    pip install jupyterlab
;; Also need some packages for compilation:
;;    brew install autoconf automake libtool pkg-config zeromq
(require 'jupyter)


;; Github etc code review
;; Requires first:
;;    brew install gpg
;;    gpg --full-generate-key
;; Then:
;;  Follow instructions at https://github.com/wandersoncferreira/code-review/blob/master/docs/github.md
;; to create .authinfo.gpg file.
;;
;; NOTE: DISABLED, this increases file load time by 2x inside git repos.
;; (add-hook 'code-review-mode-hook #'emojify-mode)
;; (setq code-review-fill-column 100)
;; (require 'code-review)

;; Additional custom keybindings
;; Use C-c g or C-c C-g to goto line
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c C-g") 'goto-line)
; Toggle comment region using C-c C-c in additional to M-;
(global-set-key (kbd "C-c C-c") 'comment-dwim)

; After loading magit, configure it
(require 'magit)
;; Set the default git executable to "git"
(setq magit-diff-expansion-threshold 3)

;; Make paths "clickable" in shell mode
(require 'compile)
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)
;; Make a `compilation-error-regexp-alist-alist` entry for pyright output, which has the format:
;; "\s*/path/to/file.py:line:col - [error message]"
(eval-after-load 'compile
  '(progn
     (add-to-list 'compilation-error-regexp-alist-alist
                  '(pyright "^\s*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\) - error: \\(.*\\)$" 1 2 3 (4)))
     (add-to-list 'compilation-error-regexp-alist 'pyright)))

;; Add a "cs" command that requests a string calls the shell command "cs" with that string as an argument.
;; The window is open in grep output mode.
(defun cs-string (string)
  "Run the shell command 'cs' with the given string as an argument."
  (let (
        ;; Wrap the string in double quotes and escape any existing double quotes.
        (string (concat "\"" string "\""))
        (buffer-name (concat "*cs " string "*")))
    (shell-command (concat "cs " string) buffer-name)
    (switch-to-buffer-other-window buffer-name)
    (grep-mode)
    (goto-char (point-min)))
  )

(defun cs ()
  "Run the shell command 'cs' with a string argument."
  (interactive)
  (let ((string (read-string "cs: ")))
    (cs-string string))
  )

;; Add a "cs-at-point" command that gets the token at the cursor and runs "cs" with that token.
(defun cs-at-point ()
  "Run the shell command 'cs' with the token at the cursor as an argument."
  (interactive)
  (let ((string (thing-at-point 'symbol)))
    (cs-string string)
))

;; Always use file locks, and store them somewhere in ~/.emacs.d/
;; Furthermore, create file locks the moment a file is opened, not just when it's edited.
;; In fact, create the lock files before the file is even being edited.
(setq lock-file-name-transform
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/locks/\\1" t)))
(setq create-lockfiles t)
;; Also set up auto-save-directory and backup-directory-alist
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
;; Enable backups and auto-save
(setq make-backup-files t)
(setq auto-save-default t)


;; Respect file-locks when loading a file and ask for confirmation if user wants to open it.
;; This is useful when opening a file that is already open in another emacs instance.  Use y-n prompt.
(defun check-file-lock-and-confirm-internal (filename wildcards)
  "Check if the file is locked and ask for confirmation if user wants to open it."
  (let (
        (file-loaded-in-buffers (find-buffer-visiting filename))
        (lock-file-name (file-locked-p filename)))
    (if (and (not file-loaded-in-buffers) lock-file-name)
        (progn
          (if (y-or-n-p (format "File is locked by %s.  Open anyway? " lock-file-name))
              t
            nil))
      t)
    ))

;; TODO: use filelock.el to create the lock files when applying orig-fun.
(defun check-file-lock-and-confirm (orig-fun &rest args)
  "Check if the file is locked and ask for confirmation if user wants to open it."
  (let ((filename (car args))
        (wildcards (cadr args)))
    (if (file-exists-p filename)
        (if (check-file-lock-and-confirm-internal filename wildcards)
            (apply orig-fun args))
      (apply orig-fun args)))
  )
;; find-file always uses the above function to first confirm
(advice-add 'find-file :around #'check-file-lock-and-confirm)

;; C-x C-f for regular find-file
;; C-x f for find-file-in-repository (which is slower)
(require 'find-file-in-repository)
(global-set-key (kbd "C-x f") 'find-file-in-repository)


;; Better M-., M-, and M-? keybindings
(require 'smart-jump)
(smart-jump-setup-default-registers)


;; To be able to use `gh-comments-show`.
(require 'gh-comments)

;; To be able to use `ci-failures-show`.
(require 'ci-failures)

;; Define a handler function for etags
(defun etags-jump-fn-for-smart-jump ()
  "Jump to definition using etags."
  (let ((tags-file (locate-dominating-file default-directory "TAGS")))
    (if tags-file
        (visit-tags-table tags-file))
    (find-tag (thing-at-point 'symbol t))))

(defun etags-refs-fn-for-smart-jump ()
  "Find references using etags."
  (let ((tags-file (locate-dominating-file default-directory "TAGS")))
    (if tags-file
        (visit-tags-table tags-file))
    (tags-apropos (concat "\\b" (thing-at-point 'symbol t)))))

;; Register the handler for the jump-to-definition action
;; register for all emacs prog and text modes
;; NOTE: NOT CURRENTLY WORKING
(smart-jump-register :modes '(emacs-lisp-mode
                              python-mode
                              rust-mode
                              go-mode
                              text-mode
                              org-mode
                              markdown-mode
                              eglot-mode
                              )
                     :jump-fn 'etags-jump-fn-for-smart-jump
                     :pop-fn 'pop-tag-mark
                     :refs-fn 'etags-refs-fn-for-smart-jump
                     :should-jump t
                     :heuristic 'error
                     :async nil)

;; Add a command "pyright" which simply runs in a shell "pyright" with the current buffer's file.
;; It also sets the new buffer's mode to compilation-mode.  While it's running, we
;; should be able to see the output in the new buffer, and switch to other buffers.
(defun pyright ()
  "Run pyright on the current buffer's file."
  (interactive)
  (let (
        (original-filename (eshell-quote-argument (buffer-file-name)))
        ;; Use the buffer's hint name to create a new buffer name.
        (buffer-name (concat "*pyright " (buffer-name) "*")))
    (if original-filename
        (progn
          (compilation-start (concat "pyright " original-filename "\n") 'compilation-mode
                             (lambda (mode) (concat "*pyright: " buffer-name "*")))
          )
      (message "No file associated with buffer."))))


(require 'rg)
(rg-enable-default-bindings)


;; Make 'rg' by default use the current token at the cursor as the default value.
(defun rg-with-default-symbol (orig-fun &rest args)
  "Advice to pre-fill the rg prompt with the symbol at point."
  (let ((string (thing-at-point 'symbol)))
    (minibuffer-with-setup-hook
        (lambda () (when string (insert (regexp-quote string))))
      (apply orig-fun args))))

(advice-add 'rg :around #'rg-with-default-symbol)

;; Add a "rg repo" command that runs "rg" prompt, with the default directory as the repo directory
(defun rg-repo ()
  "Run the interactive 'rg' command with the default directory set to the repository root."
  (interactive)
  (let ((repo-root (magit-toplevel)))
    (if repo-root
        (let ((default-directory repo-root))
          (call-interactively 'rg))
      (message "Not inside a version-controlled repository."))))

(require 'gptel)
(setq gptel-model "ChatGPT:o1-mini")
(setq gptel-stream t)

;; Set up markdown mode to use pandoc, properly toggle math mode, and use visual-line-mode
(use-package markdown-mode
  :ensure
  :init
  ;; support latex for pandoc
  (setq markdown-command "pandoc -f markdown -t html5 --mathjax --highlight-style pygments")
  (setq markdown-header-scaling t)
  (setq markdown-enable-math t)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-wiki-links t)
)


(provide 'init)
;;; init.el ends here
