(package-initialize)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")) ;; installed by default
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")) ;; installed by default from Emacs 28 onwards
(add-to-list 'load-path "~/.emacs.d/copilot-emacsd/")
(add-to-list 'load-path "~/.emacs.d/copilot-emacsd/copilot.el/")

;; s is only available on melpa, so we download the file manually from github.
(use-package s "~/.emacs.d/s.el")

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
(load-file "~/.emacs.d/init-copilot.el")
