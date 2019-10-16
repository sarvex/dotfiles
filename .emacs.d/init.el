;;  ____ _____ 
;; |  _ \_   _|  Derek Taylor (DistroTube)
;; | | | || |  	 http://www.youtube.com/c/DistroTube
;; | |_| || |  	 http://www.gitlab.com/dwt1/ 
;; |____/ |_|  	
;;        
;; A customized config.py for Qtile window manager (http://www.qtile.org)     
;; Modified by Derek Taylor (http://www.gitlab.com/dwt1/ )

(require 'package)
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
    (rainbow-mode pdf-tools emojify minimap lua-mode haskell-mode ##)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(package-initialize)
(add-to-list 'load-path "/home/dt/.emacs.d/elpa/which-key-3.3.0/which-key.el")
(require 'which-key)
(which-key-mode)
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
 '(default ((t (:family "mononoki Nerd Font Mono" :foundry "UKWN" :slant normal :weight normal :height 120 :width normal)))))

;;; Keybindings
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop) ; Indent selection by one tab length
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)  ; De-indent selection by one tab length
