(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(setq package-list '(starter-kit-bindings starter-kit starter-kit-eshell starter-kit-js starter-kit-lisp starter-kit-ruby golden-ratio elpy smart-mode-line moe-theme smart-mode-line-powerline-theme exec-path-from-shell ggtags edts))

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)
(setq read-file-name-completion-ignore-case 't)

(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(global-whitespace-mode t)
(setq ring-bell-function (lambda () (message "*beep*")))

(require 'powerline)
(powerline-default-theme)

(require 'golden-ratio)
(golden-ratio-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(tab-width 4)
 '(sentence-end-double-space nil)
 '(truncate-partial-width-windows nil)
 '(next-line-add-newlines t)
 '(require-final-newline t)
 '(blink-matching-paren-distance nil)
 '(show-paren-style 'expression)
 '(disabled-command-hook nil)
 '(undo-limit 100000)
 '(default-major-mode 'text-mode)
 '(pop-up-frame t)
 '(cursor-type (quote bar))
 '(delete-selection-mode t)
 '(ecb-options-version "2.40")
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-python-command "/usr/local/bin/python3")
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(make-backup-files nil)
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

(require 'moe-theme)
(load-theme 'moe-dark t)

(require 'whitespace)
(setq whitespace-line-column 120)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; Show the current function name in the header line
(which-function-mode)
(setq which-func-unknown "n/a")
(setq-default header-line-format '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info (assq-delete-all 'which-func-mode mode-line-misc-info))


