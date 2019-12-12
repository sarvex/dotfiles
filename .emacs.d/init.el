;;  ____ _____ 
;; |  _ \_   _|  Derek Taylor (DistroTube)
;; | | | || |  	 http://www.youtube.com/c/DistroTube
;; | |_| || |  	 http://www.gitlab.com/dwt1/ 
;; |____/ |_|  	
;;        
;; A customized config for Emacs (https://www.gnu.org/software/emacs/)     
;; Modified by Derek Taylor (http://www.gitlab.com/dwt1/)

(require 'package)
(require 'erc)
(add-to-list 'load-path "~/.emacs.d/evil")
(setq evil-want-integration t) ;; This is optional since it's already set to t by default.
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)
(add-to-list 'load-path "~/emacs-libvterm")
(require 'vterm)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-delay 0.5)
 '(blink-cursor-interval 0.5)
 '(blink-cursor-mode t)
 '(display-line-numbers-type (quote relative))
 '(global-display-line-numbers-mode t)
 '(menu-bar-mode nil)
 '(minimap-window-location (quote right))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (ipython-shell-send evil-collection goto-chg multi-term org-bullets undo-tree rainbow-mode pdf-tools emojify minimap lua-mode haskell-mode ##)))
 '(scroll-bar-mode nil)
 '(send-mail-function (quote mailclient-send-it))
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(package-initialize)
(add-to-list 'load-path "/home/dt/.emacs.d/elpa/which-key-3.3.0/which-key.el")
(require 'which-key)
(which-key-mode)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)

;; Fields that are auto-filled in email
(setq
   user-mail-address "derek@distrotube.com"
   user-full-name  "Derek Taylor"
   mu4e-compose-signature
    (concat
      "Derek Taylor\n"
      "http://www.youtube.com/DistroTube\n"))

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.1and1.com" 587 nil nil))
   smtpmail-auth-credentials
     '(("smtp.1and1.com" 587 "derek@distrotube.com" nil))
   smtpmail-default-smtp-server "smtp.1and1.com"
   smtpmail-smtp-server "smtp.1and1.com"
   smtpmail-smtp-service 587)
;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(require 'rainbow-mode)
  (use-package rainbow-mode
    :ensure t
    :config
    (setq rainbow-x-colors nil)
    (add-hook 'prog-mode-hook 'rainbow-mode))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)
(add-hook 'after-init-hook #'global-emojify-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Mononoki Nerd Font Mono" :foundry "UKWN" :slant normal :weight normal :height 120 :width normal)))))
(when (require 'evil-collection nil t)
  (evil-collection-init))
(require 'org-bullets)
;; (setq org-bullets-bullet-list '("☯" "○" "✸" "✿" "~"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; minimal rendering by default in eww
(setq-default shr-inhibit-images t)`
(setq-default shr-use-fonts nil)

;;; Keybindings
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop) ; Indent selection by one tab length
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)  ; De-indent selection by one tab length
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
