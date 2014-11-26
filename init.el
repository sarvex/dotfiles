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
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-python-command "/usr/local/bin/python3")
 '(initial-frame-alist (quote ((vertical-scroll-bars) (width . 80) (height . 30))))
 '(make-backup-files nil)
 '(menu-bar-mode t)
 '(python-check-command "/usr/local/bin/pyflakes")
 '(python-shell-interpreter "/usr/local/bin/python3")
 '(require-final-newline t))

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

(load-theme 'moe-light t)

