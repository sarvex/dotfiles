(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(setq package-list '(starter-kit-bindings starter-kit starter-kit-eshell starter-kit-js starter-kit-lisp starter-kit-ruby golden-ratio helm elpy smart-mode-line moe-theme smart-mode-line-powerline-theme exec-path-from-shell ggtags edts))

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
 '(blink-matching-paren-distance nil)
 '(cursor-type (quote bar))
 '(default-major-mode (quote text-mode) t)
 '(delete-selection-mode t)
 '(disabled-command-function nil t)
 '(ecb-options-version "2.40")
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-python-command "/usr/local/bin/python3")
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(jde-jdk-registry (quote (("1.8.0" . "/etc/alternatives/java_sdk"))))
 '(make-backup-files nil)
 '(mouse-wheel-mode t)
 '(next-line-add-newlines t)
 '(pop-up-frame t)
 '(put (quote narrow-to-region) t)
 '(python-check-command "/usr/local/bin/pyflakes")
 '(python-shell-interpreter "/usr/local/bin/python3")
 '(require-final-newline t)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(speedbat t)
 '(tab-width 4)
 '(truncate-partial-width-windows nil)
 '(undo-limit 100000)
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

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)


(add-to-list 'load-path "~/.emacs.d/jdee-2.4.1/lisp")
(load "jde")

