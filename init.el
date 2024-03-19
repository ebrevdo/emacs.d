(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")) ;; installed by default
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")) ;

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/s.el")
(add-to-list 'load-path "~/.emacs.d/lisp/f.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(paredit editorconfig jsonrpc)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Load copilot-emacsd
(load-file "~/.emacs.d/copilot-init.el")
