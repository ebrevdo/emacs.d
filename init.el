(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")) ;; installed by default
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")) ;

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/s.el")
(add-to-list 'load-path "~/.emacs.d/lisp/f.el")
(add-to-list 'load-path "~/.emacs.d/lisp/ht.el")
(add-to-list 'load-path "~/.emacs.d/lisp/ts.el")
(add-to-list 'load-path "~/.emacs.d/lisp/uuidgen-el")
(add-to-list 'load-path "~/.emacs.d/lisp/pyvenv")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aibo:model "gpt4-turbo")
 '(package-selected-packages '(paredit editorconfig jsonrpc)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;; Load pyvenv
(require 'pyvenv)

