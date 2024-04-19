(with-eval-after-load 'package
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
  (add-to-list 'package-archives '("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/"))
  )

;; This allows us to then call `use-package-report` to profile startup.
(setq use-package-compute-statistics t)

;; Add all subdirectories of "~/.emacs.d/lisp" to the load path
(add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Save all backup files in ~/.emacs.d/backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aibo:model "gpt4-turbo")
 '(custom-safe-themes
   '("061cf8206a054f0fd0ecd747e226608302953edf9f24663b10e6056ab783419f" "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" default))
 '(custom-theme-directory "~/.emacs.d/lisp/themes")
 '(package-selected-packages
   '(go-mode rust-mode org org-journal markdown-mode flymake solarized-theme magit orderless vertico eglot paredit editorconfig jsonrpc)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
(add-to-list 'load-path "~/.emacs.d/lisp/aibo/elisp")
(require 'aibo)
; Keybindings
(global-set-key (kbd "C-M-h") 'aibo:homepage)
(global-set-key (kbd "C-M-s") 'aibo:message-search)
; Control+Meta+/ to create a new conversation
(global-set-key (kbd "C-M-/") 'aibo:create-conversation)
; M-I to create a new conversation
(global-set-key (kbd "M-I") 'aibo:create-conversation)
;; Hide `aibo` buffers from Ivy by adding the following:
(add-to-list 'ivy-ignore-buffers "\\*Aibo")
;; Start openai chatbot server
(aibo:start-server)

;; Load paredit in lisp/elisp configurations
(add-hook 'emacs-lisp-mode-hook
          #'paredit-mode)

;; Load pyvenv and eglot, ruff, black, etc.
;; To install associated python LSP server:
;;   pip install 'python-lsp-server[all]' python-lsp-black python-lsp-ruff
;; Enable LSP support by default in programming buffers
(add-hook 'prog-mode-hook #'eglot-ensure)
;; Create a memorable alias for `eglot-ensure'.
(defalias 'start-lsp-server #'eglot)
;; Enabled inline static analysis
(add-hook 'prog-mode-hook #'flymake-mode)
(require 'pyvenv)
(add-hook 'python-mode-hook 'pyvenv-mode)
(add-hook 'eglot-managed-mode-hook 'pyvenv-mode)
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
(defun eglot-format-buffer-on-save ()
  "Run `eglot-format-buffer` as a before-save hook."
  (add-hook 'before-save-hook #'ruff-fix-before-save -10 t))
(add-hook 'eglot-managed-mode-hook #'eglot-format-buffer-on-save)
;; Also isort on save
(require 'python-isort)
(add-hook 'python-mode-hook 'python-isort-on-save-mode)
;; Configure company mode
(use-package company
  :ensure t
  :init (global-company-mode)
  :bind ("M-/" . company-complete-common) ; Use Meta+/ to perform completion
  )
;; Configure consult mode.  Note this doesn't work with python-lsp-server as of writing,
;; and that pyright lsp server doesn't seem to work with eglot.
(require 'consult-eglot)

;; fuzzy completion in minibuffer, etc
;; Enable vertico
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
  ;; (setq vertico-cycle t)
  )

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
; Open up magit status using C-x g or C-x C-g

;; Add a "cs" command that requests a string calls the shell command "cs" with that string as an argument.
;; The window is open in grep output mode.
(defun cs-string (string)
  "Run the shell command 'cs' with the given string as an argument."
  (let (
        ;; First, any non-quoted spans surrounded by forward slashes: first escape the
        ;; forward slashes
        (string (replace-regexp-in-string "/" "\\\\/" string))
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



(provide 'init)
;;; init.el ends here
