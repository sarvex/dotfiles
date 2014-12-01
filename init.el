(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)
(setq read-file-name-completion-ignore-case 't)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq sentence-end-double-space nil)
(setq truncate-partial-width-windows nil)
(setq-default indicate-empty-lines t)
(setq next-line-add-newlines t)
(setq require-final-newline 't)
(setq blink-matching-paren-distance nil)
(setq show-paren-style 'expression)
(setq disabled-command-hook nil)
(setq undo-limit 100000)
(setq default-major-mode 'text-mode)
(setq pop-up-frame t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(global-whitespace-mode t)
(setq ring-bell-function (lambda () (message "*beep*")))

(require 'powerline)
(powerline-default-theme)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(cursor-type (quote bar))
 '(delete-selection-mode t)
 '(ecb-options-version "2.40")
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-python-command "/usr/local/bin/python3")
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(initial-frame-alist
   (quote
    ((vertical-scroll-bars)
     (width . 90)
     (height . 35)
     m)))
 '(make-backup-files nil)
 '(menu-bar-mode t)
 '(mouse-wheel-mode t)
 '(put (quote narrow-to-region) t)
 '(python-check-command "/usr/local/bin/pyflakes")
 '(python-shell-interpreter "/usr/local/bin/python3")
 '(require-final-newline t)
 '(show-paren-mode t)
 '(speedbat t)
 '(windmove-default-keybindings nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq py-python-command "/usr/local/bin/python3")
(elpy-enable)

(exec-path-from-shell-initialize)

(setq load-path (cons "/usr/local/opt/erlang/lib/erlang/lib/tools-2.7/emacs" load-path))
(setq erlang-root-dir "/usr/local/opt/erlang/lib/erlang/lib")
(setq exec-path (cons "/usr/local/opt/erlang/lib/erlang/bin" exec-path))
(require 'erlang-start)

(load-theme 'moe-dark t)

(require 'whitespace)
(setq whitespace-line-column 120)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

