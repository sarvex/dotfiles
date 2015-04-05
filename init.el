;;; init.el --- Emacs configuration of Sarvex Jatasra -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2015
;;
;; Author: Sarvex Jatasra <sarvex.jatasra@gmail.com>
;; URL: sarvex.github.io
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:

;(byte-recompile-directory (expand-file-name "~/.emacs.d/elpa") 0)

(setq user-full-name "Sarvex Jatasra")
(setq user-mail-address "sarvex.jatasra@gmail.com")

;;; Code:
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(prefer-coding-system 'utf-8)

(fset 'yes-or-no-p 'y-or-n-p)

;;; Debugging
(setq message-log-max 10240)


;;; Package management

(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;; Requires

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(require 'subr-x)
(require 'rx)
(require 'time-date)


;;; Key binding
(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v l" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)
(bind-key [remap just-one-space] #'cycle-spacing)
(bind-key "C-c T d" #'toggle-debug-on-error)
(bind-key "C-c h b" #'describe-personal-keybindings)
(bind-key "C-c t v" #'variable-pitch-mode)


;;; Mac Environment fixup
(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (progn
    (when (string-match-p "/zsh$" (getenv "SHELL"))
      (setq exec-path-from-shell-arguments '("-l")))

    (dolist (var '("PYTHONPATH" "INFOPATH"))
      (add-to-list 'exec-path-from-shell-variables var))

    (exec-path-from-shell-initialize)

    (with-eval-after-load 'info
      (dolist (dir (parse-colon-path (getenv "INFOPATH")))
    (when dir
      (add-to-list 'Info-directory-list dir))))))

(use-package ns-win                     ; OS X window support
  :defer t
  :if (eq system-type 'darwin)
  :config
  (setq ns-pop-up-frames nil            ; Don't pop up new frames from the
    ; workspace
    mac-option-modifier 'meta       ; Option is simply the natural Meta
    mac-command-modifier 'meta      ; But command is a lot easier to hit
    mac-right-command-modifier 'left
    mac-right-option-modifier 'none ; Keep right option for accented input
    ;; Just in case we ever need these keys
    mac-function-modifier 'hyper))


;;; Customization interface
(defconst my-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
  :defer t
  :config
  (setq custom-file my-custom-file
    custom-buffer-done-kill nil            ; Kill when existing
    custom-buffer-verbose-help nil         ; Remove redundant help text
    custom-unlispify-tag-names nil
    custom-unlispify-menu-entries nil)
  :init (load my-custom-file 'no-error 'no-message))


;;; User interface

(use-package diminish
  :ensure diminish)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

(use-package dynamic-fonts              ; Select best available font
  :ensure t
  :config
  (progn
    (setq dynamic-fonts-preferred-monospace-fonts
      '(
    "Source Code Pro"
    "Anonymous Pro"
    "Inconsolata"
    "Consolas"
    "Fira Mono"
    "Menlo"
    "DejaVu Sans Mono"
    "Bitstream Vera Mono"
    "Courier New")
      dynamic-fonts-preferred-monospace-point-size
    (pcase system-type (`darwin 13) (`windows-nt 9))
      dynamic-fonts-preferred-proportional-fonts
      '(
    "Fira Sans"
    "Helvetica"
    "Segoe UI"
    "DejaVu Sans"
    "Bitstream Vera"
    "Tahoma"
    "Verdana"
    "Arial Unicode MS"
    "Arial")
      dynamic-fonts-preferred-proportional-point-size
    (pcase system-type (`darwin 13) (`windows-nt 9)))

    (dynamic-fonts-setup)))

(use-package unicode-fonts              ; Map Unicode blocks to fonts
  :ensure t
  :disabled t
  :init (unicode-fonts-setup))

(use-package moe-theme
  :ensure t
  :defer t
  :init (require 'moe-theme) (load-theme 'moe-dark 'no-confirm))

(use-package solarized                  ; My colour theme
  :ensure solarized-theme
  :disabled t
  :defer t
  :init (load-theme 'solarized-light 'no-confirm)
  :config
  (setq solarized-use-variable-pitch nil
    solarized-emphasize-indicators nil
    solarized-height-minus-1 1.0
    solarized-height-plus-1 1.0
    solarized-height-plus-2 1.0
    solarized-height-plus-3 1.0
    solarized-height-plus-4 1.0))

(use-package zenburn
  :disabled t
  :ensure zenburn-theme
  :defer t
  :init (load-theme 'zenburn 'no-confirm))


;;; The mode line
(use-package powerline
  :ensure t
  :init (require 'powerline)
  :config (powerline-default-theme)

  (use-package stickyfunc-enhance
    :ensure t
    :defer t
    :init
    (progn
      (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
      (require 'stickyfunc-enhance)
      (semantic-mode t))))

(use-package which-func                 ; Current function name in header line
  :init (which-function-mode)
  :config
  (setq which-func-unknown "⊥"
    which-func-format
    `((:propertize (" ➤ " which-func-current)
       local-map ,which-func-keymap
       face which-func
       mouse-face mode-line-highlight))))

(use-package fancy-battery
  :ensure t
  :defer t
  :init (fancy-battery-mode))

(use-package anzu
  :ensure t
  :init (global-anzu-mode)
  :config (setq anzu-cons-mode-line-p nil)
  :diminish anzu-mode)

(use-package autopair
  :ensure t
  :config (autopair-global-mode)
  :diminish autopair-mode)

(use-package golden-ratio
  :ensure t
  :config (golden-ratio-mode t)
  :diminish golden-ratio-mode)

(use-package god-mode
  :ensure t
  :init (require 'god-mode))

(use-package deft
  :ensure t
  :init (require 'deft)
  :config
  (progn
    (setq deft-extension "org")
    (setq deft-text-mode 'org-mode)
    (setq deft-use-filename-as-title t)))


;;; The minibuffer
(use-package savehist
  :init (savehist-mode t)
  :config (setq savehist-save-minibuffer-history t
    savehist-autosave-interval 180))

(use-package ido
  :init (progn (ido-mode t)
       (ido-everywhere))
  :config
  (setq ido-enable-flex-matching t
    ido-create-new-buffer 'always
    ido-use-filename-at-point 'guess
    ido-default-file-method 'selected-window
    ido-default-buffer-method 'selected-window
    ido-use-virtual-buffers t
    ido-use-faces nil))

(use-package ido-ubiquitous
  :ensure t
  :init (ido-ubiquitous-mode))

(use-package ido-at-point
  :ensure t
  :defer t
  :init (require 'ido-at-point)
  :config (ido-at-point-mode))

(use-package ido-complete-space-or-hyphen
  :ensure t
  :defer t
  :init (require 'ido-complete-space-or-hyphen))

(use-package flx-ido
  :ensure t
  :init (flx-ido-mode))

(use-package flx-isearch
  :ensure t
  :defer t
  :diminish flex-isearch-mode
  :bind (("C-M-s" . flex-isearch-forward)
     ("C-M-r" . flex-isearch-backward)))

(use-package smex
  :ensure t
  :bind (([remap execute-extended-command] . smex)
     ("M-X" . smex-major-mode-commands)))


;;; Buffer, Windows and Frames

(setq frame-resize-pixelwise t
      frame-title-format
      '(:eval (if (buffer-file-name)
      (abbreviate-file-name (buffer-file-name)) "%b")))

(use-package uniquify                   ; Make buffer names unique
  :config (setq uniquify-buffer-name-style 'forward))

(use-package ibuffer                    ; Better buffer list
  :bind (([remap list-buffers] . ibuffer))
  :config (setq ibuffer-formats
    '((mark modified read-only vc-status-mini " "
    (name 18 18 :left :elide)
    " "
    (size 9 -1 :right)
    " "
    (mode 16 16 :left :elide)
    " "
    (vc-status 16 16 :left)
    " "
    filename-and-process)
      (mark modified read-only " "
    (name 18 18 :left :elide)
    " "
    (size 9 -1 :right)
    " "
    (mode 16 16 :left :elide)
    " " filename-and-process)
      (mark " "
    (name 16 -1)
    " " filename))))

(use-package ibuffer-vc                 ; Group buffers by VC project and status
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook
      (lambda ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-projectile         ; Group buffers by Projectile project
  :ensure t
  :defer t)

(use-package windmove                   ; Move between windows with Shift+Arrow
  :bind (("S-<left>"  . windmove-left)
     ("S-<right>" . windmove-right)
     ("S-<up>"    . windmove-up)
     ("S-<down>"  . windmove-down)))

(use-package winner                     ; Undo and redo window configurations
  :init (winner-mode))

(use-package ediff-wind
  :defer t
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
    ediff-split-window-function #'split-window-horizontally))

(use-package writeroom-mode
  :ensure t
  :bind (("C-c T R" . writeroom-mode)))

(use-package fringe-current-line
  :ensure t
  :defer t
  :init (require 'fringe-current-line)
  :config (global-fringe-current-line-mode t))

(use-package git-gutter-fringe+
  :ensure t
  :defer t
  :init (require 'git-gutter-fringe+)
  :config (setq git-gutter-fr+-side 'right-fringe))


;;; File handling

(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq delete-by-moving-to-trash
      (or (not (eq system-type 'darwin)) ; Trash is well supported on other
    ; systems
      (fboundp 'system-move-file-to-trash)))

(use-package tramp                      ; Access remote files
  :defer t
  :config
  (setq tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save")))

(use-package dired                      ; Edit directories
  :defer t
  :config
  (progn
    (require 'dired-x)
    (setq dired-auto-revert-buffer t    ; Revert on re-visiting
      dired-listing-switches "-alhF"
      dired-ls-F-marks-symlinks t   ; -F marks links with @
      dired-recursive-copies 'always)
    (when (or (memq system-type '(gnu gnu/linux))
      (string= (file-name-nondirectory insert-directory-program) "gls"))
      (setq dired-listing-switches
    (concat dired-listing-switches " --group-directories-first -v")))))

(use-package dired-x                    ; Additional tools for Dired
  :bind (("C-x C-j" . dired-jump))
  :config
  (progn
    (setq dired-omit-verbose nil)        ; Shut up, dired
    (when (eq system-type 'darwin)
      (setq dired-guess-shell-gnutar "tar"))))

(use-package copyright                  ; Deal with copyright notices
  :defer t
  :bind (("C-c u C" . copyright-update))
  :init (add-hook 'find-file-hook #'copyright-update)
  :config (setq copyright-year-ranges t
    copyright-names-regexp (regexp-quote user-full-name)))

(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :ensure t
  :init (ignoramus-setup))

(use-package hardhat                    ; Protect user-writable files
  :ensure t
  :init (global-hardhat-mode t)
  :config (setq hardhat-mode-lighter nil))

(use-package bookmark                   ; Bookmarks for Emacs buffers
  :bind (("C-c l b" . list-bookmarks))
  :config (setq bookmark-save-flag 1))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(use-package recentf
  :init (recentf-mode t)
  :bind ("C-x C-r" . ido-recentf-open)
  :config
  (setq recentf-max-saved-items 512
    recentf-max-menu-items 64
    recentf-auto-cleanup 300
    recentf-exclude (list "/\\.git/.*\\'" ; Git contents
      "/elpa/.*\\'" ; Package files
      "/itsalltext/" ; It's all text temp files
      #'ignoramus-boring-p)))

(use-package saveplace                  ; Save point position in files
  :config (setq-default save-place t))

(use-package autorevert
  :init
  (progn
    (global-auto-revert-mode)
    (setq auto-revert-check-vc-info t)))

(use-package image-file
  :init (auto-image-file-mode))

(use-package launch
  :ensure t
  :defer t
  :init (global-launch-mode))


;;; Basic editing

(use-package electric
  :init (electric-layout-mode))

(use-package elec-pair
  :init (electric-pair-mode))

(setq indicate-empty-lines t
      require-final-newline t)

(setq kill-ring-max 200                 ; More killed items
      save-interprogram-paste-before-kill t)

(use-package subword                    ; Subword/superword editing
  :defer t
  :diminish subword-mode)

(use-package adaptive-wrap
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package visual-fill-column
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package visual-regexp              ; Regexp replace with in-buffer display
  :ensure t
  :bind (("C-c r" . vr/query-replace)
     ("C-c R" . vr/replace)))

(use-package browse-kill-ring           ; Browse kill ring interactively
  :ensure t
  :bind (("C-c y" . browse-kill-ring)))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-to-char)
     ("M-Z" . zop-up-to-char)))

(use-package easy-kill                  ; Easy killing and marking on C-w
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
     ([remap mark-sexp]      . easy-mark)))

(use-package align                      ; Align text in buffers
  :bind (("C-c A a" . align)
     ("C-c A c" . align-current)
     ("C-c A r" . align-regexp)))

(use-package multiple-cursors           ; Edit text with multiple cursors
  :ensure t
  :bind (("C-c m e"   . mc/mark-more-like-this-extended)
     ("C-c m h"   . mc/mark-all-like-this-dwim)
     ("C-c m l"   . mc/edit-lines)
     ("C-c m n"   . mc/mark-next-like-this)
     ("C-c m p"   . mc/mark-previous-like-this)
     ("C-c m r"   . vr/mc-mark)
     ("C-c m C-a" . mc/edit-beginnings-of-lines)
     ("C-c m C-e" . mc/edit-ends-of-lines)
     ("C-c m C-s" . mc/mark-all-in-region))
  :config
  (setq mc/mode-line
    '(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
      face font-lock-warning-face)))

(use-package expand-region              ; Expand region by semantic units
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package undo-tree                  ; Branching undo
  :ensure t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package nlinum                     ; Line numbers in display margin
  :ensure t
  :bind (("C-c T l" . nlinum-mode)))

(use-package server                     ; The server of `emacsclient'
  :defer t
  :init (server-start))

(bind-key [remap just-one-space] #'cycle-spacing)


;;; Navigation and scrolling
(setq scroll-margin 0
      scroll-conservatively 1000
      scroll-error-top-bottom t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)
     ("C-c j"   . ace-jump-mode)
     ("C-c J"   . ace-jump-mode-pop-mark))
  :config
  (ace-jump-mode-enable-mark-sync))

(use-package ace-jump-buffer
  :ensure t
  :defer t)

(use-package ace-jump-zap
  :ensure t
  :defer t)

(use-package ace-window
  :ensure t
  :defer t
  :bind ("M-p" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package wiki-nav
  :ensure t
  :defer t
  :init (require 'wiki-nav)
  :config (global-wiki-nav-mode t)
  :diminish wiki-nav-mode button-lock-mode)

(use-package page-break-lines           ; Turn page breaks into lines
  :ensure t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package outline                    ; Navigate outlines in buffers
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
      (add-hook hook #'outline-minor-mode))
  :diminish outline-minor-mode)

(use-package imenu-anywhere             ; IDO-based imenu across open buffers
  :ensure t
  :bind (("C-c i" . imenu-anywhere)))


;;; Search
(use-package ag                         ; Search code in files/projects
  :ensure t
  :bind (("C-c a a" . ag-regexp)
     ("C-c a A" . ag)
     ("C-c a d" . ag-dired-regexp)
     ("C-c a D" . ag-dired)
     ("C-c a f" . ag-files)
     ("C-c a k" . ag-kill-other-buffers)
     ("C-c a K" . ag-kill-buffers))
  :config
  (setq ag-reuse-buffers t            ; Don't spam buffer list with ag buffers
    ag-highlight-search t         ; A little fanciness
    ag-project-root-function (lambda (d) (let ((default-directory d))
       (projectile-project-root)))))

(use-package wgrep                      ; Edit grep/occur/ag results in-place
  :ensure t
  :defer t)

(use-package wgrep-ag                   ; Wgrep for ag
  :ensure t
  :defer t)


;;; Highlights
(use-package whitespace                 ; Highlight bad whitespace
  :config
  (progn
    (setq whitespace-action '(auto-cleanup))
    (setq whitespace-style '(face tab-mark empty trailing lines-tail space-before-tab indentation empty space-after-tab))
    (setq whitespace-line-column nil)
    (global-whitespace-mode t))
  :diminish global-whitespace-mode)

(use-package hl-line                    ; Highlight the current line
  :init (global-hl-line-mode 1))

(use-package paren                      ; Highlight paired delimiters
  :init (show-paren-mode)
  :config (setq show-paren-when-point-inside-paren t
    show-paren-when-point-in-periphery t))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
      (add-hook hook #'rainbow-delimiters-mode)))

(use-package rainbow-mode
  :ensure t
  :bind (("C-c T r" . rainbow-mode))
  :config (add-hook 'css-mode-hook #'rainbow-mode))

(use-package rainbow-blocks
  :ensure t
  :init (require 'rainbow-blocks)
  :config
  (dolist (hook '(eval-expression-minibuffer-setup-hook
      emacs-lisp-mode-hook
      inferior-emacs-lisp-mode-hook
      clojure-mode-hook))
    (add-hook hook #'rainbow-blocks-mode))
  :diminish rainbow-blocks-mode)

(use-package rainbow-identifiers
  :ensure t
  :defer t
  :config (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

(use-package color-moccur
  :ensure t
  :defer t
  :init (require 'color-moccur))

(use-package color-identifiers-mode
  :ensure t
  :defer t
  :config (global-color-indentifiers-mode)
  :diminish color-identifiers-mode)

(use-package hi-lock                    ; Custom regexp highlights
  :init (global-hi-lock-mode))


;;; Skeletons, completion and expansion

(setq completion-cycle-threshold 5)

(use-package hippie-exp                 ; Powerful expansion and completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (setq hippie-expand-try-functions-list
    '(try-expand-dabbrev
      try-expand-dabbrev-all-buffers
      try-expand-dabbrev-from-kill
      try-complete-file-name-partially
      try-complete-file-name
      try-expand-all-abbrevs
      try-expand-list
      try-complete-lisp-symbol-partially
      try-complete-lisp-symbol)))

(use-package company                    ; Graphical (auto-)completion
  :ensure t
  :init (global-company-mode)
  :config
  (progn
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)
    (setq company-tooltip-align-annotations t
      company-show-numbers t))
  :diminish company-mode)

(use-package company-quickhelp          ; Documentation popups for Company
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

(use-package company-math               ; Completion for Math symbols
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-math-symbols-unicode)
      (add-to-list 'company-backends 'company-math-symbols-latex)))


;;; Spelling and syntax checking
(use-package ispell                     ; Spell checking
  :defer t
  :if (eq system-type 'darwin)
  :config
  (progn
    (setq ispell-program-name (if (eq system-type 'darwin)
      (executable-find "aspell")
    (executable-find "hunspell"))
      ispell-dictionary "en_GB"     ; Default dictionnary
      ispell-silently-savep t       ; Don't ask when saving the private dict
      ispell-choices-win-default-height 5)

    (unless ispell-program-name
      (warn "No spell checker available.  Install Hunspell or ASpell for OS X."))))

(use-package flyspell                   ; On-the-fly spell checking
  :bind (("C-c T s" . flyspell-mode))
  :if (eq system-type 'darwin)
  :init
  (progn
    (dolist (hook '(text-mode-hook message-mode-hook))
      (add-hook hook 'turn-on-flyspell))
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (progn
    (setq flyspell-use-meta-tab nil
      flyspell-issue-welcome-flag nil
      flyspell-issue-message-flag nil)
    (define-key flyspell-mode-map "\M-\t" nil))
  :diminish flyspell-mode)

(use-package flycheck                   ; On-the-fly syntax checking
  :ensure flycheck
  :defer t
  :bind (("C-c l e" . list-flycheck-errors)
     ("C-c T f" . flycheck-mode))
  :init (global-flycheck-mode t)
  :config
  (progn
    (setq flycheck-completion-system 'ido)
    (set-face-attribute 'flycheck-error-list-checker-name nil :inherit 'italic)
    (use-package flycheck-color-mode-line
      :ensure flycheck-color-mode-line
      :init (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))
  :diminish flycheck-mode)

(use-package flycheck-pos-tip           ; Show Flycheck messages in popups
  :ensure t
  :defer t
  :init (with-eval-after-load 'flycheck
      (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(use-package volatile-highlights
  :ensure t
  :init (require 'volatile-highlights)
  :config (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)

;;; Text editing
(use-package tildify
  :bind (("C-c u t" . tildify-region))
  :init (dolist (hook '(markdown-mode-hook
    latex-mode-hook
    rst-mode-hook))
      (add-hook hook #'tildify-mode))
  :config (add-hook 'latex-mode-hook
    (lambda () (setq-local tildify-space-string "~"))))

(use-package typo
  :ensure t
  :bind (("C-c T t" . typo-mode))
  :init (progn
      (typo-global-mode)
      (dolist (hook '(markdown-mode-hook
      rst-mode-hook))
    (add-hook hook 'typo-mode))))

(use-package delsel
  :defer t
  :init (delete-selection-mode))


;;; LaTeX with AUCTeX
(use-package tex-site                   ; AUCTeX initialization
  :ensure auctex)

(use-package tex                        ; TeX editing/processing
  :ensure auctex
  :defer t
  :config
  (progn
    (setq TeX-parse-self t              ; Parse documents to provide completion
      TeX-auto-save t               ; Automatically save style information
      TeX-electric-sub-and-superscript t ; Automatically insert braces after
      TeX-electric-math '("\\(" "\\)")
      TeX-quote-after-quote t
      TeX-clean-confirm nil
      TeX-source-correlate-mode t
      TeX-source-correlate-method 'synctex)
    (setq-default TeX-master nil        ; Ask for the master file
      TeX-engine 'luatex    ; Use a modern engine
      TeX-PDF-mode t)

    (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s")))

(use-package tex-buf                    ; TeX buffer management
  :ensure auctex
  :defer t
  :config (setq TeX-save-query nil))

(use-package tex-style                  ; TeX style
  :ensure auctex
  :defer t
  :config
  (setq LaTeX-csquotes-close-quote "}"
    LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold                   ; TeX folding
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package tex-mode                   ; TeX mode
  :ensure auctex
  :defer t
  :config
  (font-lock-add-keywords 'latex-mode
      `((,(rx "\\"
      symbol-start
      "fx" (1+ (or (syntax word) (syntax symbol)))
      symbol-end)
     . font-lock-warning-face))))

(use-package latex                      ; LaTeX editing
  :ensure auctex
  :defer t
  :config
  (progn
    (setq TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
      (,(rx (0+ space) "\\subsection*{") 3)
      (,(rx (0+ space) "\\subsubsection*{") 4)
      (,(rx (0+ space) "\\minisec{") 5))
      LaTeX-babel-hyphen nil)

    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)))    ; Easy math input

(use-package auctex-latexmk             ; latexmk command for AUCTeX
  :ensure t
  :defer t
  :init (with-eval-after-load 'latex
      (auctex-latexmk-setup)))

(use-package bibtex                     ; BibTeX editing
  :defer t
  :config
  (progn
    (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
    (bibtex-set-dialect 'biblatex)))

(defun my-reftex-find-ams-environment-caption (environment)
  "Find the caption of an AMS ENVIRONMENT."
  (let ((re (rx-to-string `(and "\\begin{" ,environment "}"))))
    (re-search-backward re)
    (goto-char (match-end 0)))
  (if (not (looking-at (rx (zero-or-more space) "[")))
      (error "Environment %s has no title" environment)
    (let ((beg (match-end 0)))
      (goto-char (1- beg))
      (forward-list)
      (buffer-substring-no-properties beg (1- (point))))))

(use-package reftex                     ; TeX/BibTeX cross-reference management
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  (progn
    (setq reftex-plug-into-AUCTeX t
      reftex-insert-label-flags '(t t)
      reftex-label-alist
      '(
    ("definition" ?d "def:" "~\\ref{%s}"
     my-reftex-find-ams-environment-caption
     ("definition" "def.") -3)
    ("theorem" ?h "thm:" "~\\ref{%s}"
     my-reftex-find-ams-environment-caption
     ("theorem" "th.") -3)
    ("example" ?x "ex:" "~\\ref{%s}"
     my-reftex-find-ams-environment-caption
     ("example" "ex") -3)
    ("algorithm" ?a "alg:" "~\\ref{%s}"
     "\\\\caption[[{]" ("algorithm" "alg") -3)))

    (unless (assq 'biblatex reftex-cite-format-builtin)
      (add-to-list 'reftex-cite-format-builtin
       '(biblatex "The biblatex package"
      ((?\C-m . "\\cite[]{%l}")
       (?t . "\\textcite{%l}")
       (?a . "\\autocite[]{%l}")
       (?p . "\\parencite{%l}")
       (?f . "\\footcite[][]{%l}")
       (?F . "\\fullcite[]{%l}")
       (?x . "[]{%l}")
       (?X . "{%l}"))))
      (setq reftex-cite-format 'biblatex)))
  :diminish reftex-mode)


;; EShell
(use-package eshell
  :bind ("H-e" . eshell)
  :config
  (progn
    ;; Scrolling
    (setq eshell-scroll-to-bottom-on-output t
      eshell-scroll-show-maximum-output t
      eshell-save-history-on-exit t
      eshell-buffer-shorthand t)
    (use-package esh-mode
      :defer t
      :config
      (progn
    (defun eshell/cds ()
      (eshell/cd (or (locate-dominating-file default-directory "src")
     (locate-dominating-file default-directory ".git"))))
    (defun eshell/clear ()
      (interactive)
      (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max)))
      (eshell-send-input))
    (add-hook 'eshell-mode-hook
      #'(lambda ()
      (bind-key "C-l" 'eshell/clear eshell-mode-map)))))
    (use-package eshell-opt
      :config
      (use-package eshell-prompt-extras
    :ensure eshell-prompt-extras))
    (use-package em-term
      :defer t
      :config
      (setq eshell-visual-commands
    (append '("tmux" "screen" "ssh") eshell-visual-commands)))
    (use-package em-hist
      :defer t
      :config
      (setq eshell-hist-ignoredups t)))
  (use-package em-smart
    :config (progn
      (setq eshell-where-to-jump 'begin)
      (setq eshell-review-quick-commands nil)
      (setq eshell-smart-space-goes-to-end t))))

(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode); Enable Flyspell program mode for emacs lisp mode, which highlights all misspelled words in comments and strings.



;;; Other markup languages
(use-package rst                        ; ReStructuredText
  :defer t
  :config
  (setq rst-indent-literal-minimized 3
    rst-indent-literal-normal 3)

  (bind-key "C-=" nil rst-mode-map)
  (bind-key  "C-c C-j" #'rst-insert-list rst-mode-map))

(use-package markdown-mode              ; Markdown
  :ensure t
  :mode ("/itsalltext/.*\\.md\\'" . gfm-mode)
  :config
  (progn
    (let ((stylesheet (expand-file-name
       (locate-user-emacs-file "etc/pandoc.css"))))
      (setq markdown-command
    (mapconcat #'shell-quote-argument
       `("pandoc" "--toc" "--section-divs"
     "--css" ,(concat "file://" stylesheet)
     "--standalone" "-f" "markdown" "-t" "html5")
       " ")))

    (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
    (add-hook 'gfm-mode-hook #'visual-line-mode)
    (add-hook 'gfm-mode-hook #'my-whitespace-style-no-long-lines)

    (bind-key "C-c C-s C" #'markdown-insert-gfm-code-block markdown-mode-map)
    (bind-key "C-c C-s P" #'markdown-insert-gfm-code-block markdown-mode-map)

    (bind-key "M-q" #'ignore gfm-mode-map)))

(use-package yaml-mode                  ; YAML
  :ensure t
  :defer t
  :config
  (add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(use-package graphviz-dot-mode          ; Graphviz
  :ensure t
  :defer t
  :config
  (setq graphviz-dot-indent-width 4))


;;; Programming utilities
(use-package prog-mode
  :defer t
  :config
  (progn
    (use-package eldoc
      :diminish ""
      :defer t
      :init (add-hook 'prog-mode-hook 'eldoc-mode))
    (use-package eldoc-extension
      :ensure t
      :defer t
      :init (require 'eldoc-extension))
    (use-package hl-todo
      :ensure hl-todo
      :defer t
      :init (add-hook 'prog-mode-hook 'hl-todo-mode))
    (use-package hl-sexp
      :ensure hl-sexp
      :defer t
      :init
      (add-hook 'prog-mode-hook 'hl-sexp-mode))))

(use-package compile                    ; Compile from Emacs
  :bind (("C-c c" . compile)
     ("C-c C" . recompile))
  :config
  (progn
    (setq compilation-ask-about-save nil ; Just save before compiling
      compilation-always-kill t     ; Just kill old compile processes before
    ; starting the new one
      compilation-scroll-output 'first-error ; Automatically scroll to first
    ; error
      )))

(use-package highlight-numbers          ; Fontify number literals
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook #'highlight-numbers-mode)
    (add-hook 'text-mode-hook #'highlight-numbers-mode)
    (add-hook 'LaTeX-mode #'highlight-numbers-mode)))

(use-package highlight-symbol           ; Highlighting and commands for symbols
  :ensure t
  :defer t
  :bind
  (("C-c s %" . highlight-symbol-query-replace)
   ("C-c s n" . highlight-symbol-next-in-defun)
   ("C-c s o" . highlight-symbol-occur)
   ("C-c s p" . highlight-symbol-prev-in-defun))
  :init
  (progn
    (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
    (add-hook 'prog-mode-hook #'highlight-symbol-mode))
  :config
  (setq highlight-symbol-idle-delay 0.4
    highlight-symbol-on-navigation-p t)
    ; navigation
  :diminish highlight-symbol-mode)

(use-package elide-head                 ; Elide lengthy GPL headers
  :bind (("C-c u h" . elide-head))
  :init (add-hook 'prog-mode-hook #'elide-head))

(use-package electric-case
  :ensure t
  :defer t
  :config
  (progn
    (setq electric-case-convert-calls t)
    (add-hook 'ahk-mode-hook #'electric-case-ahk-init)
    (add-hook 'c-mode-hook #'electric-case-c-init)
    (add-hook 'scala-mode-hook #'electric-case-scala-init)))

(use-package restclient
  :ensure t
  :defer t)

(use-package company-restclient
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-restclient)))

(use-package smartrep
  :ensure t
  :defer t)

(use-package eide
  :ensure t
  :defer t
  :config (eide-start))

(use-package smart-forward
  :ensure t
  :defer t
  :init (require 'smart-forward)
  :bind (("M-<up>" . smart-up)
     ("M-<down>" . smart-down)
     ("M-<left>" . smart-backward)
     ("M-<right>" . smart-forward)))

(use-package change-inner
  :ensure t
  :defer t
  :init (require 'change-inner)
  :bind (("M-i" . change-inner)
     ("M-o" . change-outer)))


;;; Generic Lisp
(use-package paredit                    ; Balanced sexp editing
  :ensure t
  :defer t
  :init (dolist (hook '(eval-expression-minibuffer-setup-hook
    emacs-lisp-mode-hook
    inferior-emacs-lisp-mode-hook
    clojure-mode-hook))
      (add-hook hook #'paredit-mode))
  :config
  (progn
    (define-key paredit-mode-map (kbd "M-s") nil)
    (define-key paredit-mode-map (kbd "M-S-<up>") #'paredit-splice-sexp))
  :diminish paredit-mode)

(use-package elisp-slime-nav            ; Jump to definition of symbol at point
  :ensure t
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  :diminish elisp-slime-nav-mode)

(use-package flycheck-cask              ; Setup Flycheck by Cask projects
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(use-package flycheck-package           ; Check package conventions with Flycheck
  :ensure t
  :defer t
  :init (with-eval-after-load 'flycheck (flycheck-package-setup)))

(use-package pcre2el                    ; Convert regexps to RX and back
  :ensure t
  :init (rxt-global-mode))

(use-package macrostep                  ; Interactively expand macros in code
  :ensure t
  :defer t
  :init (with-eval-after-load 'lisp-mode
      (bind-key "C-c e" #'macrostep-expand emacs-lisp-mode-map)
      (bind-key "C-c e" #'macrostep-expand lisp-interaction-mode-map)))

(use-package ielm                       ; Emacs Lisp REPL
  :bind (("C-c u z" . ielm)))

(use-package lisp-mode                  ; Emacs Lisp editing
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode ("/Cask\\'" . emacs-lisp-mode)
  :init (require 'ert))

(bind-key "C-c T d" #'toggle-debug-on-error)

;; Cucumber
(use-package feature-mode               ; Feature files for ecukes/cucumber
  :ensure t
  :defer t
  :config
  (progn
    ;; Add standard hooks for Feature Mode, since it is no derived mode
    (add-hook 'feature-mode-hook #'whitespace-mode)
    (add-hook 'feature-mode-hook #'whitespace-cleanup-mode)
    (add-hook 'feature-mode-hook #'flyspell-mode)))


;;; Lua
(use-package lua
     :mode ("\\.lua\\'" . lua-mode)
     :interpreter ("lua" . lua-mode))


;;; Markdown
(use-package markdown-mode
     :ensure t
     :mode ("\\.md\\'" . markdown-mode))


;;; CSV
(use-package csv-mode
  :mode "\\.csv\\'")



;;; Clojure
(use-package cider
  :ensure t
  :defer t
  :config
  (progn
    (setq nrepl-log-messages t)
    (setq nrepl-hide-special-buffers t)
    (setq cider-repl-tab-command #'indent-for-tab-command)
    (setq cider-prefer-local-resources t)
    (setq cider-repl-pop-to-buffer-on-connect nil)))

(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'clojure-mode-hook #'subword-mode)
    (add-hook 'clojure-mode-hook #'smartparens-strict-mode)))

(use-package clojure-mode-extra-font-locking
  :ensure t
  :init (require 'clojure-mode-extra-font-locking))

(use-package inf-clojure
  :ensure t
  :defer t
  :config (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode))

(use-package clj-refactor
  :ensure t
  :defer t)

(use-package clojure-snippets
  :ensure t
  :defer t)

(use-package flycheck-clojure
  :ensure t
  :defer t
  :config
  (progn
    (flycheck-clojure-setup)
    (global-flycheck-mode)))


;;; Scala
(defconst my-scalastyle-version '("0.6.0" . "2.10")
  "Version of scala style to use for Flycheck.

A pair of `(VERSION . SCALA-VERSION)'.")

(defconst my-scalastyle-jar
  (pcase-let ((`(,version . ,scala-version) my-scalastyle-version))
    (format "scalastyle_%s-%s-batch.jar" scala-version version))
  "Name of the scalastyle JAR.")

(defconst my-scalastyle-url
  (pcase-let ((`(,version . ,scala-version) my-scalastyle-version))
    (format "https://oss.sonatype.org/content/repositories/releases/org/scalastyle/scalastyle_%s/%s/%s"
    scala-version version my-scalastyle-jar))
  "URL to get scalastyle from.")

(use-package scala-mode2                ; Scala editing
  :ensure t
  :defer t
  :config
  (progn
    (let ((filename (locate-user-emacs-file my-scalastyle-jar)))
      (unless (file-exists-p filename)
    (message "Downloading scalastyle JAR")
    (url-copy-file my-scalastyle-url filename))

      (with-eval-after-load 'flycheck
    (setq flycheck-scalastyle-jar (expand-file-name filename)
      flycheck-scalastylerc "scalastyle-config.xml")))))

(use-package sbt-mode                   ; Scala build tool
  :ensure t
  :defer t)

(use-package ensime
  :ensure t
  :disabled t)


;;; Python
(use-package python
  :defer t
  :config
  (progn
    (add-hook 'python-mode-hook #'subword-mode)
    (setq python-check-command "pylint"
      python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")))

(use-package jedi
  :commands jedi:setup
  :init
  (progn
    (add-hook 'python-mode-hook 'jedi:setup))
  :config
  (progn
    (setq jedi:setup-keys t)
    (setq jedi:complete-on-dot t)))

(use-package anaconda-mode              ; Powerful Python backend for Emacs
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook #'anaconda-mode))

(use-package company-anaconda           ; Python backend for Company
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-anaconda)))

(use-package pip-requirements           ; requirements.txt files
  :ensure t
  :defer t)

(use-package python-django
  :ensure t
  :defer t
  :init (require 'python-django))

(use-package ein
  :ensure t
  :defer t
  :init (require 'ein)
  :config (setq ein:use-smartrep t))

(use-package elpy
  :ensure t
  :defer t
  :config
  (progn
    (elpy-enable)
    (elpy-use-ipython)
    (elpy-clean-modeline)))

(use-package pylint
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'python-mode-hook #'pylint-add-menu-items)
    (add-hook 'python-mode-hook #'pylint-add-key-bindings)))


;;; Go
(use-package go-direx
  :ensure t
  :defer t
  :init (require 'go-direx)
  :bind ("C-c C-j" . go-direx-pop-to-buffer))

(use-package go-errcheck
  :ensure t
  :defer t)

(use-package go-mode
  :ensure t
  :defer t
  :bind (("M-." . godef-jump)
     ("C-c C-c" . go-run))
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'go-mode-hook
      (lambda ()
    ((add-hook 'before-save-hook #'gofmt-before-save)
     (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
     (local-set-key (kbd "C-c i") 'go-goto-imports)
     (local-set-key (kbd "M-.") 'godef-jump)

     (if (not (string-match "go" compile-command))
     (set (make-local-variable 'compile-command)
      "go build -v && go test -v && go vet")))))))

(use-package go-eldoc
  :ensure t
  :defer t
  :init
  (progn
    (require 'go-eldoc)
    (add-hook 'go-mode-hook 'go-eldoc-setup)))

(use-package company-go
  :ensure t
  :defer t
  :init (require 'company-go)
  :config
  (progn
    (setq company-tooltip-limit 20)                      ; bigger popup window
    (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
    (setq company-echo-delay 0)                          ; remove annoying blinking
    (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
    (add-to-list 'company-backends 'company-go)))

(use-package go-play
  :ensure t
  :defer t)

(use-package go-projectile
  :ensure t
  :defer t
  :init (require 'go-projectile))

(use-package go-snippets
  :ensure t
  :defer t)

(use-package go-stacktracer
  :ensure t
  :defer t)

(use-package golint
  :ensure t
  :defer t)

(use-package gotest
  :ensure t
  :defer t)


;;; C / C++
(use-package c-eldoc
  :ensure t
  :defer t
  :config (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))

(use-package cmake-ide
  :ensure t
  :defer t)

(use-package srefactor
  :ensure t
  :defer t
  :init
  (progn
    (require 'srefactor)
    (semantic-mode 1))
  :config
  (progn
    (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
    (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)))


;;; Ruby
(use-package inf-ruby                   ; Ruby REPL
  :ensure t
  :defer t
  :init (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  :config (inf-ruby-switch-setup))

(use-package ruby-refactor
  :ensure t
  :defer t
  :init
  (progn
    (require 'ruby-refactor)
    (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)))

(use-package robe                       ; Ruby backend for Emacs
  :ensure t
  :defer t
  :config (add-to-list 'company-backends 'company-robe))


;;; Rust
(use-package rust-mode                  ; Rust
  :ensure t
  :defer t)

(use-package flycheck-rust              ; Flycheck setup for Rust
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package toml-mode                  ; Toml for Cargo files
  :ensure t
  :defer t)


;;; Rust
(use-package csharp-mode
  :ensure t
  :defer t)

(use-package fsharp-mode
  :ensure t
  :defer t)

(use-package omnisharp
  :ensure t
  :defer t)


;;; Haskell
(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'haskell-mode-hook #'subword-mode)           ; Subword navigation
    (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode) ; Scan and navigate
    (add-hook 'haskell-mode-hook #'haskell-auto-insert-module-template)
    (setq haskell-tags-on-save t
      haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-process-use-presentation-mode t ; Don't clutter the echo area
      haskell-process-show-debug-tips nil     ; Disable tips
      haskell-process-log t                   ; Log debugging information
      haskell-process-suggest-hoogle-imports nil
      haskell-process-suggest-hayoo-imports t
      haskell-process-path-ghci "ghci-ng")

    (add-to-list 'haskell-process-args-cabal-repl "--with-ghc=ghci-ng")

    (bind-key "C-c h d" #'haskell-describe haskell-mode-map)
    (bind-key "C-c h h" #'haskell-hayoo haskell-mode-map)
    (bind-key "C-c h H" #'haskell-hoogle haskell-mode-map)
    (bind-key "C-c u i" #'haskell-navigate-imports haskell-mode-map)
    (bind-key "C-c f c" #'haskell-cabal-visit-file haskell-mode-map)))

(use-package haskell
  :ensure haskell-mode
  :defer t
  :init (dolist (hook '(haskell-mode-hook haskell-cabal-mode-hook))
      (add-hook hook #'interactive-haskell-mode))
  :config
  (progn
    (bind-key "C-c C-t" #'haskell-mode-show-type-at
      interactive-haskell-mode-map)
    (bind-key "M-." #'haskell-mode-goto-loc
      interactive-haskell-mode-map)
    (bind-key "C-c u u" #'haskell-mode-find-uses
      interactive-haskell-mode-map)))

(use-package haskell-interactive-mode
  :ensure haskell-mode
  :defer t
  :config (add-hook 'haskell-interactive-mode-hook #'subword-mode))

(use-package haskell-simple-indent      ; Primitive Haskell indentation
  :ensure haskell-mode
  :defer t
  :init (add-hook 'haskell-mode-hook #'haskell-simple-indent-mode))

(use-package hindent                    ; Automated Haskell indentation
  :ensure t
  :defer t
  :init (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package flycheck-haskell           ; Setup Flycheck from Cabal projects
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


;;; Erlang
(use-package erlang
  :ensure t
  :defer t
  :config (require 'erlang-start))

(use-package edts
  :ensure t
  :defer t
  :config (require 'edts-start))



;;; Elixir
(use-package alchemist
  :ensure t
  :defer t
  :config (setq alchemist-project-compile-when-needed t)
  (setq alchemist-goto-erlang-source-dir "/path/to/erlang/source/")
  (setq alchemist-goto-elixir-source-dir "/path/to/elixir/source/"))

(use-package elixir-mode
  :ensure t
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.elixir2\\'" . elixir-mode))
  :config (add-hook
       'elixir-mode-hook
       (lambda ()
     (and (file-exists (buffer-file-name))
       (file-exists (elixir-mode-compiled-file-name))
       (elixir-cos-mode t)))))

(use-package elixir-yasnippets
  :ensure t
  :defer t)


;;; OCaml
(use-package tuareg                     ; OCaml editing
  :ensure t
  :defer t
  :config
  (progn
    (setq tuareg-use-smie nil)
    (define-key tuareg-mode-map [?\C-c ?i] nil)))

(use-package merlin                     ; Powerful Emacs backend for OCaml
  :ensure t
  :defer t
  :init (add-hook 'tuareg-mode-hook #'merlin-mode)
  :config
  (setq merlin-command 'opam
    merlin-error-after-save nil))

(use-package flycheck-ocaml             ; Check OCaml code with Merlin
  :ensure t
  :defer t
  :init (with-eval-after-load 'merlin
      (flycheck-ocaml-setup)))


;;; Web languages

(use-package web-mode                   ; Template editing
  :ensure t
  :defer t
  :config
  (setq web-mode-markup-indent-offset 2))

(use-package coffee-mode
  :ensure t
  :defer t
  :config (custom-set-variables '(coffee-tab-width 2)))

(use-package js2-mode                   ; Javascript editing
  :ensure t
  :mode "\\.js\\(?:on\\)?\\'"
  :config (setq-default js2-basic-offset 2))

(use-package js2-refactor
  :ensure t
  :init (require 'js2-refactor))

(use-package tern
  :ensure t
  :config (add-hook 'js-mode-hook #'tern-mode))

(use-package css-mode
  :defer t
  :config
  (progn
    (add-hook 'css-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
    (put 'css-indent-offset 'safe-local-variable #'integerp)))

(use-package css-eldoc                  ; Basic Eldoc for CSS
  :ensure t
  :commands (turn-on-css-eldoc)
  :init (add-hook 'css-mode-hook #'turn-on-css-eldoc))

(use-package php-mode                   ; Because sometimes you have to
  :ensure t)


;;; Misc programming languages
(use-package sh-script                  ; Shell scripts
  :mode ("\\.zsh\\'" . sh-mode)
  :config
  (setq sh-indentation 2                ; The basic indentation
    sh-basic-offset 2               ; The offset for nested indentation
    ))

(use-package puppet-mode                ; Puppet manifests
  :ensure t
  :defer t
  :config
  (setq puppet-fontify-variables-in-comments t))

(use-package nxml-mode                  ; XML editing
  :defer t
  :config (setq nxml-slash-auto-complete-flag t
    nxml-auto-insert-xml-declaration-flag t))

(use-package feature-mode               ; Feature files for ecukes/cucumber
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'feature-mode-hook #'whitespace-mode)
    (add-hook 'feature-mode-hook #'whitespace-cleanup-mode)
    (add-hook 'feature-mode-hook #'flyspell-mode)))

(use-package cmake-mode                 ; CMake files
  :ensure t
  :defer t)

(use-package thrift                     ; Thrift interface files
  :ensure t
  :defer t)

(use-package swift-mode                 ; Swift sources
  :ensure t
  :defer t
  :config (with-eval-after-load 'flycheck
    (add-to-list 'flycheck-checkers 'swift)))


;;; Proof General & Coq
(defun my-have-proofgeneral-p ()
  "Determine whether we have Proof General installed."
  (file-exists-p (locate-user-emacs-file "vendor/ProofGeneral/generic")))

(use-package proof-site
  :load-path "vendor/ProofGeneral/generic"
  :if (my-have-proofgeneral-p)
  :config
  (setq proof-three-window-enable nil   ; More predictable window management
    proof-electric-terminator-enable t))

(defvar coq-one-command-per-line)
(setq coq-one-command-per-line nil)

(use-package proof-script
  :if (my-have-proofgeneral-p)
  :defer t
  :config
  (add-hook 'proof-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(use-package isar                       ; Isabelle syntax for PG
  :if (my-have-proofgeneral-p)
  :defer t
  :config
  (add-hook 'isar-mode-hook #'my-whitespace-style-no-long-lines 'append))

(use-package company-coq
  :if (my-have-proofgeneral-p)
  :ensure t
  :defer t
  :init (add-hook 'coq-mode-hook #'company-coq-initialize))


;;; Databases
(use-package sql
  :bind (("C-c d m" . sql-mysql)))


;;; Version control
(use-package vc-hooks                   ; Simple version control
  :defer t
  :config
  (setq vc-follow-symlinks t))

(use-package diff-hl
  :ensure t
  :defer t
  :config
  (progn
    (global-diff-hl-mode)
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(use-package magit                      ; The one and only Git frontend
  :ensure t
  :bind (("C-c g"   . magit-status)
     ("C-c v g" . magit-status)
     ("C-c v v" . magit-status)
     ("C-c v g" . magit-blame-mode))
  :config
  (progn
    (setq magit-save-some-buffers 'dontask
      magit-stage-all-confirm nil
      magit-unstage-all-confirm nil
      magit-set-upstream-on-push t
      magit-completing-read-function #'magit-ido-completing-read)
    (magit-auto-revert-mode))
  :diminish magit-auto-revert-mode)

(use-package git-commit-mode
  :ensure t
  :defer t)

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package gitattributes-mode
  :ensure t
  :defer t)

(use-package git-rebase-mode
  :ensure t
  :defer t)

(use-package git-timemachine
  :ensure t
  :bind (("C-c v t" . git-timemachine)))

(use-package git-gutter
  :ensure t
  :defer t
  :config (global-git-gutter-mode t))

(use-package git-blame
  :ensure t
  :defer t)

(use-package git-messenger
  :ensure t
  :defer t)

(use-package yagist
  :ensure t
  :defer t)

(use-package gh
  :ensure t
  :defer t)

(use-package github-browse-file
  :ensure t
  :defer t)

(use-package github-clone
  :ensure t
  :defer t)


;;; Tools and utilities
(use-package projectile                 ; Project management
  :ensure t
  :defer t
  :init (projectile-global-mode)
  :config
  (progn
    (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
    (setq projectile-completion-system 'ido
      projectile-find-dir-includes-top-level t
      projectile-mode-line '(:propertize
     (:eval (concat " " (projectile-project-name)))
     face font-lock-constant-face))
    (def-projectile-commander-method ?a
      "Find ag on project."
      (call-interactively 'projectile-ag))

    (bind-key "s a" #'ag-project-regexp projectile-command-map)
    (bind-key "s A" #'ag-project projectile-command-map)
    (bind-key "s d" #'ag-project-dired-regexp projectile-command-map)
    (bind-key "s D" #'ag-project-dired projectile-command-map)
    (bind-key "s f" #'ag-project-files projectile-command-map)
    (bind-key "s k" #'ag-kill-other-buffers projectile-command-map)
    (bind-key "s K" #'ag-kill-buffers projectile-command-map))
  :diminish projectile-mode)

(use-package bug-reference              ; Turn bug references into buttons
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
       (add-hook 'text-mode-hook #'bug-reference-mode)))

(use-package paradox                    ; Better package menu
  :ensure t
  :bind (("C-c l p" . paradox-list-packages)
     ("C-c l P" . package-list-packages-no-fetch))
  :config
  (setq paradox-github-token t
    paradox-execute-asynchronously nil))

(use-package proced                     ; Edit system processes
  :if (not (eq system-type 'darwin))
  :bind ("C-x p" . proced))

(use-package calendar                   ; Built-in calendar
  :bind ("C-c u c" . calendar)
  :config
  (setq calendar-week-start-day 1))

(use-package time                       ; Show current time
  :bind (("C-c u i" . emacs-init-time)
     ("C-c u T" . display-time-world))
  :config
  (setq display-time-world-time-format "%H:%M %Z, %d. %b"
    display-time-world-list '(("Europe/Berlin"    "Berlin")
      ("Europe/London"    "London")
      ("Europe/Istanbul"  "Istanbul")
      ("America/Winnipeg" "Winnipeg (CA)")
      ("America/New_York" "New York (USA)")
      ("Asia/Tokyo"       "Tokyo (JP)"))))

(use-package bug-hunter                 ; Search init file for bugs
  :ensure t)


;;; Terminal emulation and shells
(use-package shell                      ; Dump shell in Emacs
  :bind ("C-c u s" . shell))

(use-package term                       ; Terminal emulator in Emacs
  :bind ("C-c u S" . ansi-term))


;;; Net & Web
(use-package eww                        ; Emacs' built-in web browser
  :bind (("C-c w b" . eww-list-bookmarks)
     ("C-c w w" . eww)))

(use-package sx                         ; StackExchange client for Emacs
  :ensure t
  :bind (("C-c w s" . sx-tab-frontpage)
     ("C-c w S" . sx-tab-newest)
     ("C-c w a" . sx-ask)))

(use-package sx-compose
  :ensure sx
  :defer t
  :config
  (progn
    (add-hook 'sx-compose-mode-hook #'turn-off-auto-fill)
    (add-hook 'sx-compose-mode-hook #'visual-line-mode)
    (bind-key "M-q" #'ignore sx-compose-mode-map)))

(use-package sx-question-mode
  :ensure sx
  :defer t
  :config (setq sx-question-mode-display-buffer-function #'switch-to-buffer))

(use-package sendmail                   ; Send mails from Emacs
  :defer t
  :config (setq send-mail-function 'smtpmail-send-it))

(use-package message                    ; Compose mails from Emacs
  :defer t
  :config (setq message-send-mail-function 'smtpmail-send-it
    message-kill-buffer-on-exit t))

(use-package smtpmail                   ; Send mails via SMTP
  :defer t
  :config (setq smtpmail-smtp-server "vega.uberspace.de"
    smtpmail-smtp-service 587
    smtpmail-stream-type 'starttls
    smtpmail-smtp-user user-login-name))

(use-package hackernews
  :ensure t
  :defer t
  :init (require 'hackernews)
  :config (setq hackernews-top-story-limit 50))

(use-package 2048-game
  :ensure t
  :defer t)

(use-package erc
  :defer t
  :config
  (progn
    (setq erc-server "chat.freenode.net"
      erc-port 7000
      erc-nick "sarvex"
      erc-nick-uniquifier "_"
      erc-server-connect-function 'erc-open-tls-stream)

    (add-to-list 'erc-modules 'spelling)
    (erc-update-modules)))

(use-package erc-join                   ; Automatically join channels with ERC
  :defer t
  :config
  (setq erc-autojoin-channels-alist '(("\\.freenode\\.net" . ("#emacs")))))

(use-package erc-track                  ; Track status of ERC in mode line
  :defer t
  :config
  (setq erc-track-switch-direction 'newest
    erc-track-enable-keybindings t))

(use-package rcirc                      ; Simply ERC client
  :defer t
  :config
  (progn
    (setq rcirc-default-full-name (format "%s (http://www.sarvex.com)"
      user-full-name)
      rcirc-default-nick "sarvex"
      rcirc-time-format "%Y-%m-%d %H:%M "
      rcirc-server-alist
      '(("chat.freenode.not" :port 7000 :user-name "sarvex"
     :encryption tls :channels ("#emacs" "#haskell" "#hakyll" "#zsh"))))

    (add-hook 'rcirc-mode-hook #'flyspell-mode)

    (rcirc-track-minor-mode)))


;;; * Org Mode

(use-package org
  :diminish (orgstruct-mode . "")
  :config
  (progn
    (setq org-catch-invisible-edits 'smart)
    (setq org-structure-template-alist
      '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
    ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
    ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
    ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
    ("v" "#+begin_verbatim\n?\n#+end_verbatim" "<verbatim>\n?\n</verbatim>")
    ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
    ("l" "#+begin_latex\n?\n#+end_latex"
     "<literal style=\"latex\">\n?\n</literal>")
    ("l" "#+latex: " "<literal style=\"latex\">?</literal>")
    ("h" "#+begin_html\n?\n#+end_html"
     "<literal style=\"html\">\n?\n</literal>")
    ("h" "#+html: " "<literal style=\"html\">?</literal>")
    ("a" "#+begin_ascii\n?\n#+end_ascii" "")
    ("a" "#+ascii: " "")
    ("i" "#+index: ?" "#+index: ?")
    ("i" "#+include: %file ?"
     "<include file=%file markup=\"?\">")))
    (add-hook 'prog-mode-hook 'turn-on-orgstruct)

    (defun orgstruct-lisps-turn-on ()
      (setq orgstruct-heading-prefix-regexp ";; "))

    (add-hook 'lisps-mode-hook 'orgstruct-lisps-turn-on)

    (use-package org-capture
      :bind ("C-c c" . org-capture)
      :config
      (progn
    (setq org-reverse-note-order t
      org-capture-templates
      '(("d" "Dev dump" entry (file "~/org/dev.org") "* %?\n  %i\n %a" :kill-buffer  t)
    ("j" "Journal" entry (file "~/org/journal.org") "* %U\n %?i\n %a" :kill-buffer t)))))

    (use-package org-clock
      :config
      (setq org-clock-idle-time 15
    org-clock-in-resume t
    org-clock-persist t
    org-clock-persist-query-resume nil
    org-clock-clocked-in-display 'both))))


;;; Online Help
(use-package find-func                  ; Find function/variable definitions
  :bind (("C-x F"   . find-function)
     ("C-x 4 F" . find-function-other-window)
     ("C-x K"   . find-function-on-key)
     ("C-x V"   . find-variable)
     ("C-x 4 V" . find-variable-other-window)))

(use-package apropos                    ; Search symbols for documentation
  :bind (("C-c h a" . apropos)))

(use-package ansible-doc
  :ensure t
  :defer t
  :init (add-hook 'yaml-mode-hook #'ansible-doc-mode)
  :diminish ansible-doc-mode)

(use-package dash-at-point
  :ensure t
  :defer t
  :bind (("C-c h d" . dash-at-point)
     ("C-c h D" . dash-at-point-with-docset)))

(bind-key "C-c h b" #'describe-personal-keybindings)

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :config (yas-global-mode t))

(use-package 2048-game
  :ensure t
  :defer t)

(diminish 'auto-fill-function)
(diminish 'abbrev-mode)
;(diminish 'emacs-lisp-mode "ε")
;(diminish 'lisp-mode "λ")
;;; init.el ends here



;;; Emacs Default Configuration


;(global-visual-line-mode t)
(global-prettify-symbols-mode t)
(blink-cursor-mode t)
(show-paren-mode t)
(line-number-mode t)
(column-number-mode t)
(delete-selection-mode t)
(transient-mark-mode t)

(setq-default fill-column 120)
(setq-default major-mode 'org-mode)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq gc-cons-threshold 67108864)
(setq stack-trace-on-error t)
(fringe-mode 4)
(setq message-log-max 10240)
(setq load-prefer-newer t)
(setq inhibit-default-init t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq history-length 1024)
(setq view-read-only t)
(setq tab-width 4)
(setq tab-always-indent 'complete)
(setq indicate-empty-lines t)
(setq initial-major-mode 'org-mode)
(setq require-final-newline t)
(setq kill-ring-max 256)
(setq save-interprogram-paste-before-kill t)
(setq completion-cycle-threshold 5)
(setq scroll-margin 0)
(setq scroll-conservatively 1024)
(setq scroll-error-top-bottom t)
(setq mouse-wheel-progressive-speed nil); trackpad scrolling on OS
(setq mouse-wheel-scroll-amount '(1))
(setq ring-bell-function (lambda () (message "*beep*")))
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq delete-by-moving-to-trash
      (or (not (eq system-type 'darwin))
      (fboundp 'system-move-file-to-trash)))

(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'set-goal-column 'disabled nil)
