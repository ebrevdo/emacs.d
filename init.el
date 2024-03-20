(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")) ;; installed by default
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")) ;

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/s.el")
(add-to-list 'load-path "~/.emacs.d/lisp/f.el")
(add-to-list 'load-path "~/.emacs.d/lisp/ht.el")
(add-to-list 'load-path "~/.emacs.d/lisp/ts.el")
(add-to-list 'load-path "~/.emacs.d/lisp/uuidgen-el")
(add-to-list 'load-path "~/.emacs.d/lisp/pyvenv")
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-web-server")
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-zmq")
(add-to-list 'load-path "~/.emacs.d/lisp/hl-fill-column")
(add-to-list 'load-path "~/.emacs.d/lisp/jupyter")
(add-to-list 'load-path "~/.emacs.d/lisp/themes/emacs-kaolin-themes")

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
   '(markdown-mode flymake solarized-theme magit orderless vertico eglot paredit editorconfig jsonrpc)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Stop asking if it's OK to kill processes on exit.
(setq confirm-kill-processes nil)

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

;; Load copilot-emacsd
(load-file "~/.emacs.d/copilot-init.el")

;; Load openai chatbot
(add-to-list 'load-path "~/.emacs.d/lisp/aibo/elisp")
(require 'aibo)

; Keybindings
(global-set-key (kbd "C-M-h") 'aibo:homepage)
(global-set-key (kbd "C-M-s") 'aibo:message-search)
(global-set-key (kbd "M-/") 'aibo:create-conversation)
; Hide `aibo` buffers from Ivy by adding the following:
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


;; fuzzy completion in minibuffer, etc
;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  (vertico-flat-mode)
  
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
