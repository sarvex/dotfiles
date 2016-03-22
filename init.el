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

(setq message-log-max 102400)
(setq stack-trace-on-error t)

;(global-visual-line-mode t)
(global-prettify-symbols-mode t)
(blink-cursor-mode t)
(show-paren-mode t)
(line-number-mode t)
(column-number-mode t)
(delete-selection-mode t)
(transient-mark-mode t)
(electric-pair-mode t)

(setq-default fill-column 120)
(setq-default major-mode 'org-mode)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default tab-always-indent 'complete)

(setq gc-cons-threshold (* 128 1024 1024))
(setq message-log-max 10240)
(setq load-prefer-newer t)
(setq inhibit-default-init t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq history-length 1024)
(setq view-read-only t)

(setq indicate-empty-lines t)
(setq initial-major-mode 'org-mode)
(setq require-final-newline t)
(setq kill-ring-max 256)
(setq save-interprogram-paste-before-kill t)
(setq completion-cycle-threshold 5)
(setq scroll-margin 0)
(setq scroll-conservatively 1024)
(setq scroll-error-top-bottom t)

(setq ring-bell-function (lambda () (message "*beep*")))
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq delete-by-moving-to-trash (or (not (eq system-type 'darwin)) (fboundp 'system-move-file-to-trash)))



;;; Package management
(setq load-prefer-newer t)

(require #'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require #'use-package))

(require #'bind-key)
(require #'diminish)

(require #'subr-x)
(require #'rx)
(require #'time-date)

;;; Key binding
;(bind-key "C-c f v d" #'add-dir-local-variable)
;(bind-key "C-c f v l" #'add-file-local-variable)
;(bind-key "C-c f v p" #'add-file-local-variable-prop-line)
(bind-key [remap just-one-space] #'cycle-spacing)
(bind-key "C-c T d" #'toggle-debug-on-error)
(bind-key "C-c h b" #'describe-personal-keybindings)
(bind-key "C-c t v" #'variable-pitch-mode)

;;; Mac Environment fixup
(use-package exec-path-from-shell
  :if
  (and (eq system-type 'darwin)
       (display-graphic-p))
  :config
  (progn
    (when (string-match-p "/zsh$" (getenv "SHELL"))
      (setq exec-path-from-shell-arguments '("-l")))
    (dolist (var '("PYTHONPATH" "INFOPATH"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)
    (with-eval-after-load 'info
      (dolist (dir (parse-colon-path (getenv "INFOPATH")))
    (when dir (add-to-list 'Info-directory-list dir))))))

(use-package ns-win
  :if (eq system-type 'darwin)
  :config
  (progn
    (setq ns-pop-up-frames nil)
    (setq mac-option-modifier 'meta)
    (setq mac-command-modifier 'meta)
    (setq mac-right-command-modifier 'left mac-right-option-modifier 'none)
    (setq mac-function-modifier 'hyper)))

;;; Customization interface
(defconst my-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
  :config
  (progn
    (setq custom-file my-custom-file custom-buffer-done-kill nil)
    (setq custom-buffer-verbose-help nil)
    (setq custom-unlispify-tag-names nil custom-unlispify-menu-entries nil))
  :init (load my-custom-file 'no-error 'no-message))

;;; User interface

(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(use-package diminish)

(use-package unicode-fonts
  :init (unicode-fonts-setup))

(use-package darcula-theme
;  :init (require #'darcula-theme)
;  :config (load-theme 'darcula 'no-confirm)
  )

(use-package moe-theme
  :init (require #'moe-theme)
  :config
  (progn
    (load-theme 'moe-dark 'no-confirm)
    (moe-theme-set-color 'orange)))


(use-package dynamic-fonts
  :config
  (progn
    (setq dynamic-fonts-preferred-proportional-fonts
      '("Fira Sans" "Helvetica" "Segoe UI" "DejaVu Sans" "Bitstream Vera"
        "Tahoma" "Verdana" "Arial Unicode MS" "Arial"))
    (setq dynamic-fonts-preferred-proportional-point-size
      (pcase system-type (`darwin 13) (`windows-nt 10) (`gnu/linux 10)))
    (setq dynamic-fonts-preferred-monospace-fonts
      '("Source Code Pro" "Anonymous Pro" "Inconsolata" "Consolas" "Fira Mono"
        "Menlo" "DejaVu Sans Mono" "Bitstream Vera Mono" "Courier New"))
    (setq dynamic-fonts-preferred-monospace-point-size
      (pcase system-type (`darwin 13) (`windows-nt 10) (`gnu/linux 10)))
    (dynamic-fonts-setup)))


;;; The mode line
(use-package powerline
  :init (require #'powerline)
  :config (powerline-moe-theme)
;  :config (powerline-center-theme)
  )

(use-package stickyfunc-enhance
    :init
    (progn
      (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
      (require #'stickyfunc-enhance)
      (semantic-mode t)))

(use-package which-func
  :init (which-function-mode)
  :config
  (progn
    (setq which-func-unknown "⊥")
     (setq which-function-format
      `((:propertize ("➤" which-func-current)
       local-map ,which-func-keymap
       face which-func
       mouse-face mode-line-highlight)))))

(use-package fancy-battery
  :init (fancy-battery-mode))

(use-package anzu
  :init (global-anzu-mode)
  :config (setq anzu-cons-mode-line-p nil)
  :diminish anzu-mode)

(use-package golden-ratio
  :config (golden-ratio-mode t)
  :diminish golden-ratio-mode)

(use-package god-mode
  :init
  (progn
    (require #'god-mode)
    (require #'god-mode-isearch))
  :bind ("<escape>" . god-local-mode)
  :config
  (progn
    (defun my-update-cursor ()
      (setq cursor-type (if (or god-local-mode buffer-read-only)
    'hbar 'box)))
    (add-hook 'god-mode-enabled-hook #'my-update-cursor)
    (add-hook 'god-mode-disabled-hook #'my-update-cursor)
    (define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
    (define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)))

(use-package deft
  :init (require #'deft)
  :config
  (progn
    (setq deft-extension "org")
    (setq deft-text-mode 'org-mode)
    (setq deft-use-filename-as-title t)))

(use-package hydra-examples
  :config (hydra-add-font-lock)
  (global-set-key (kbd "C-x t")
      (defhydra
    hydra-toggle
    (:color teal)
    "
_a_ abbrev-mode:      %`abbrev-mode
_d_ debug-on-error    %`debug-on-error
_f_ auto-fill-mode    %`auto-fill-function
_t_ truncate-lines    %`truncate-lines

"
    ("a" abbrev-mode nil)
    ("d" toggle-debug-on-error nil)
    ("f" auto-fill-mode nil)
    ("t" toggle-truncate-lines nil)
    ("q" nil "cancel")))
  (key-chord-define-global "ds"
       (defhydra
     hydra-zoom
     ()
     "zoom"
     ("j" text-scale-increase "in")
     ("k" text-scale-decrease "out")
     ("0" (text-scale-set 0) "reset")
     ("1" (text-scale-set 0)
      :bind nil)
     ("2" (text-scale-set 0)
      :bind nil
      :color blue)))
  (defhydra
    hydra-error
    (global-map "M-g")
    "goto-error"
    ("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev")
    ("v" recenter-top-bottom "recenter")
    ("q" nil "quit"))
  (global-set-key (kbd "C-M-o")
      (defhydra
    hydra-window
    (:color amaranth)
    "
Move Point^^^^   Move Splitter   ^Ace^                       ^Split^
--------------------------------------------------------------------------------
_w_, _<up>_      Shift + Move    _C-a_: ace-window           _2_: split-window-below
_a_, _<left>_                    _C-s_: ace-window-swap      _3_: split-window-right
_s_, _<down>_                    _C-d_: ace-window-delete    ^ ^
_d_, _<right>_                   ^   ^                       ^ ^
You can use arrow-keys or WASD.
"
    ("2" split-window-below nil)
    ("3" split-window-right nil)
    ("a" windmove-left nil)
    ("s" windmove-down nil)
    ("w" windmove-up nil)
    ("d" windmove-right nil)
    ("A" hydra-move-splitter-left nil)
    ("S" hydra-move-splitter-down nil)
    ("W" hydra-move-splitter-up nil)
    ("D" hydra-move-splitter-right nil)
    ("<left>" windmove-left nil)
    ("<down>" windmove-down nil)
    ("<up>" windmove-up nil)
    ("<right>" windmove-right nil)
    ("<S-left>" hydra-move-splitter-left nil)
    ("<S-down>" hydra-move-splitter-down nil)
    ("<S-up>" hydra-move-splitter-up nil)
    ("<S-right>" hydra-move-splitter-right nil)
    ("C-a" ace-window nil)
    ("u" hydra--universal-argument nil)
    ("C-s" (lambda ()
     (interactive)
     (ace-window 4)) nil)
    ("C-d" (lambda ()
     (interactive)
     (ace-window 16)) nil)
    ("q" nil "quit")))
  (defhydra
    hydra-org-template
    (:color blue
    :hint nil)
    "
_c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
_l_atex   _E_xample   _p_erl          _i_ndex:
_a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
_s_rc     ^ ^         plant_u_ml      _H_TML:
_h_tml    ^ ^         ^ ^             _A_SCII:
"
    ("s" (hot-expand "<s"))
    ("E" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("c" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("e" (progn (hot-expand "<s")
    (insert "emacs-lisp")
    (forward-line)))
    ("p" (progn (hot-expand "<s")
    (insert "perl")
    (forward-line)))
    ("u" (progn (hot-expand "<s")
    (insert "plantuml :file CHANGE.png")
    (forward-line)))
    ("P" (progn (insert
     "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
    (hot-expand "<s")
    (insert "perl")
    (forward-line)))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("o" nil "quit"))
  (defun hot-expand (str)
    "Expand org template."
    (insert str)
    (org-try-structure-completion))
  (with-eval-after-load "org" (define-key org-mode-map "<" (lambda ()
   (interactive)
   (if (looking-back "^")
   (hydra-org-template/body)
     (self-insert-command 1))))))


;;; The minibuffer
(use-package savehist
  :init (savehist-mode t)
  :config
  (progn
    (setq savehist-save-minibuffer-history t)
    (setq savehist-autosave-interval 180)))

(use-package ido
  :init
  (progn
    (ido-mode t)
    (ido-everywhere))
  :config
  (progn
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    (setq ido-use-filename-at-point 'guess)
    (setq ido-default-file-method 'selected-window)
    (setq ido-default-buffer-method 'selected-window)
    (setq ido-use-virtual-buffers t)
    (setq ido-use-faces nil)))

(use-package ido-ubiquitous
  :init (ido-ubiquitous-mode))

(use-package ido-at-point
  :init (require #'ido-at-point)
  :config (ido-at-point-mode))

(use-package ido-complete-space-or-hyphen
  :init (require #'ido-complete-space-or-hyphen))

(use-package ido-clever-match)

(use-package ido-describe-bindings)

(use-package ido-exit-target)

(use-package flx-ido
  :init (flx-ido-mode))

(use-package flx-isearch
  :diminish flex-isearch-mode
  :bind (("C-M-s" . flex-isearch-forward)
     ("C-M-r" . flex-isearch-backward)))

(use-package smex
  :bind (([remap execute-extended-command] . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package diffview)

;;; Buffer, Windows and Frames

(setq frame-resize-pixelwise t)
(setq frame-title-format
      '(:eval (if (buffer-file-name)
      (abbreviate-file-name (buffer-file-name)) "%b")))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package ibuffer
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

(use-package ibuffer-vc
  :init (add-hook
     'ibuffer-hook
     (lambda ()
       (ibuffer-vc-set-filter-groups-by-vc-root)
       (unless (eq ibuffer-sorting-mode 'alphabetic)
     (ibuffer-do-sort-by-alphabetic)))))



(use-package windmove                              ; Move between windows with Shift+Arrow
  :bind (("S-<left>"  . windmove-left)
     ("S-<right>" . windmove-right)
     ("S-<up>"    . windmove-up)
     ("S-<down>"  . windmove-down)))

(use-package winner                                ; Undo and redo window configurations
  :init (winner-mode))

(use-package ediff-wind
  :config (setq ediff-window-setup-function #'ediff-setup-windows-plain ediff-split-window-function
    #'split-window-horizontally))

(use-package writeroom-mode
  :bind (("C-c T R" . writeroom-mode)))

(use-package fringe-current-line
  :init (require #'fringe-current-line)
  :config (global-fringe-current-line-mode t))

(use-package git-gutter+
  :init (global-git-gutter+-mode t)
  :config (progn (custom-set-variables '(git-gutter:update-interval 2))
     (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
     (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
     (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
     (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
     (define-key git-gutter+-mode-map (kbd "C-x C-y")
       'git-gutter+-stage-and-commit-whole-buffer)
     (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer))
  :diminish git-gutter+-mode)

(use-package git-gutter-fringe+
  :init (require #'git-gutter-fringe+)
  :config (setq git-gutter-fr+-side 'right-fringe))

;;; File handling

(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq delete-by-moving-to-trash (or (not (eq system-type 'darwin)) ; Trash is well supported on other
  ; systems
  (fboundp 'system-move-file-to-trash)))

(use-package tramp                                 ; Access remote files
  :config (setq tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save")))

(use-package dired
  :config (progn
    (require #'dired-x)
    (setq dired-auto-revert-buffer t
      dired-listing-switches "-alhF" dired-ls-F-marks-symlinks t
      dired-recursive-copies 'always)
    (when (or (memq system-type '(gnu gnu/linux))
      (string= (file-name-nondirectory insert-directory-program) "gls"))
      (setq dired-listing-switches (concat dired-listing-switches
     " --group-directories-first -v")))))

(use-package dired-x
  :bind (("C-x C-j" . dired-jump))
  :config (progn
    (setq dired-omit-verbose nil)
    (when (eq system-type 'darwin)
      (setq dired-guess-shell-gnutar "tar"))))

(use-package copyright
  :bind (("C-c u C" . copyright-update))
  :init (add-hook 'find-file-hook #'copyright-update)
  :config (setq copyright-year-ranges t copyright-names-regexp (regexp-quote user-full-name)))

(use-package ignoramus
  :init (ignoramus-setup))

(use-package hardhat
  :init (global-hardhat-mode t)
  :config (setq hardhat-mode-lighter nil))

(use-package bookmark
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
  :config (setq recentf-max-saved-items 512 recentf-max-menu-items 64 recentf-auto-cleanup 300
    recentf-exclude (list "/\\.git/.*\\'" "/elpa/.*\\'" "/itsalltext/"
    #'ignoramus-boring-p)))

(use-package autorevert
  :init (progn (global-auto-revert-mode)
       (setq auto-revert-check-vc-info t)))

(use-package image-file
  :init (auto-image-file-mode))

(use-package launch
  :init (global-launch-mode))

;;; Basic editing

(use-package electric
  :init (electric-layout-mode))

(setq indicate-empty-lines t require-final-newline t)

(setq kill-ring-max 200
      save-interprogram-paste-before-kill t)

(use-package subword
  :diminish subword-mode
  :config (add-hook 'prog-mode-hook #'subword-mode))

(use-package adaptive-wrap
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package visual-fill-column
  :init (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package visual-regexp
  :bind (("C-c r" . vr/query-replace)
     ("C-c R" . vr/replace)))

(use-package browse-kill-ring
  :bind (("C-c y" . browse-kill-ring)))

(use-package zop-to-char
  :bind (("M-z" . zop-to-char)
     ("M-Z" . zop-up-to-char)))

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
     ([remap mark-sexp]      . easy-mark)))

(use-package align                                 ;
  :bind (("C-c A a" . align)
     ("C-c A c" . align-current)
     ("C-c A r" . align-regexp)))

(use-package multiple-cursors                      ; Edit text with multiple cursors
  :bind (("C-c m e"   . mc/mark-more-like-this-extended)
     ("C-c m h"   . mc/mark-all-like-this-dwim)
     ("C-c m l"   . mc/edit-lines)
     ("C-c m n"   . mc/mark-next-like-this)
     ("C-c m p"   . mc/mark-previous-like-this)
     ("C-c m r"   . vr/mc-mark)
     ("C-c m C-a" . mc/edit-beginnings-of-lines)
     ("C-c m C-e" . mc/edit-ends-of-lines)
     ("C-c m C-s" . mc/mark-all-in-region))
  :config (setq mc/mode-line
    '(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
      face
      font-lock-warning-face)))

(use-package move-text
  :bind (("<C-M-up>" . move-text-up)
     ("<C-M-down>" . move-text-down)))

(use-package expand-region                         ; Expand region by semantic units
  :bind (("C-=" . er/expand-region)))

(use-package undo-tree                             ; Branching undo
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package nlinum                                ; Line numbers in display margin
  :bind (("C-c T l" . nlinum-mode)))

(use-package server
  :init (server-start))

(bind-key [remap just-one-space] #'cycle-spacing)

;;; Navigation and scrolling

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-conservatively 1000)
(setq scroll-error-top-bottom t)
(setq scroll-margin 0)

(use-package smooth-scrolling
  :init (require #'smooth-scrolling)
  :config (setq smooth-scroll-margin 2))

 (use-package ace-jump-mode
  :bind (("C-c SPC" . ace-jump-mode)
     ("C-c j"   . ace-jump-mode)
     ("C-c J"   . ace-jump-mode-pop-mark))
  :config (ace-jump-mode-enable-mark-sync))

(use-package ace-jump-buffer)

(use-package ace-jump-zap)

(use-package ace-window
  :bind ("M-p" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package wiki-nav
  :init (require #'wiki-nav)
  :config (global-wiki-nav-mode t)
  :diminish wiki-nav-mode
  button-lock-mode)

(use-package page-break-lines                      ; Turn page breaks into lines
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package outline                               ; Navigate outlines in buffers
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
      (add-hook hook #'outline-minor-mode))
  :diminish outline-minor-mode)

(use-package imenu-anywhere                        ; IDO-based imenu across open buffers
  :bind (("C-c i" . imenu-anywhere)))

;;; Search
(use-package ag                                    ; Search code in files/projects
  :bind (("C-c a a" . ag-regexp)
     ("C-c a A" . ag)
     ("C-c a d" . ag-dired-regexp)
     ("C-c a D" . ag-dired)
     ("C-c a f" . ag-files)
     ("C-c a k" . ag-kill-other-buffers)
     ("C-c a K" . ag-kill-buffers))
  :config (setq ag-reuse-buffers t      ; Don't spam buffer list with ag buffers
    ag-highlight-search t   ; A little fanciness
    ag-project-root-function (lambda (d)
     (let ((default-directory d))
   (projectile-project-root)))))

(use-package wgrep)

(use-package wgrep-ag)


;;; Highlights
(use-package whitespace                            ; Highlight bad whitespace
  :config (progn
    (setq whitespace-action '(auto-cleanup))
    (setq whitespace-style '(face tab-mark empty trailing lines-tail space-before-tab
    indentation empty space-after-tab))
    (setq whitespace-line-column nil)
    (global-whitespace-mode t))
  :diminish global-whitespace-mode)

(use-package hl-line
  :init (global-hl-line-mode t))

(use-package hi-lock
  :init (global-hi-lock-mode))

(use-package paren
  :init (show-paren-mode)
  :config (setq show-paren-when-point-inside-paren t show-paren-when-point-in-periphery t))

(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (progn
    (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-mode))))

(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :init (require #'rainbow-delimiters)
  :config
  (progn
    (dolist (hook '(text-mode-hook prog-mode-hook))
      (add-hook hook #'rainbow-delimiters-mode))))

(use-package rainbow-identifiers
  :config (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode))
  :diminish rainbow-identifiers-mode)

(use-package color-identifiers-mode
  :config (global-color-indentifiers-mode)
  :diminish color-identifiers-mode)

(use-package rainbow-blocks
  :disabled t
  :init (require #'rainbow-blocks)
  :config (dolist (hook '(eval-expression-minibuffer-setup-hook emacs-lisp-mode-hook
  inferior-emacs-lisp-mode-hook
  lisp-mode-hook clojure-mode-hook))
    (add-hook hook #'rainbow-blocks-mode))
  :diminish rainbow-blocks-mode)

(use-package color-moccur
  :init (require #'color-moccur))



;;; Lisp
(use-package paredit
  :config
  (dolist(hook '(eval-expression-minibuffer-setup-hook
                 emacs-lisp-mode-hook
                 inferior-emacs-lisp-mode-hook
                 lisp-mode-hook
                 clojure-mode-hook))
    (add-hook hook #'paredit-mode))
  :init (require 'paredit))

(use-package paredit-everywhere
  :init (require 'paredit)
  :config (add-hook #'prog-mode-hook #'paredit-everywhere-mode))

(use-package paredit-menu
  :init (require 'paredit)
  :config (require 'paredit-menu))

(use-package adjust-parens
  :init (require #'adjust-parens)
  :config
  (dolist (hook '(eval-expression-minibuffer-setup-hook
                  emacs-lisp-mode-hook
                  inferior-emacs-lisp-mode-hook
                  lisp-mode-hook
                  clojure-mode-hook))
    (add-hook hook #'adjust-parens-mode)))


;;; Skeletons, completion and expansion

(setq completion-cycle-threshold 5)

(use-package hippie-exp                            ; Powerful expansion and completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config (setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol)))

;;; Company Mode Auto Completions

(use-package company
  :init (global-company-mode)
  :config (progn (bind-key [remap completion-at-point] #'company-complete company-mode-map)
     (setq company-tooltip-align-annotations t)
     (setq company-show-numbers t)
     (setq company-tooltip-limit 20)
     (setq company-idle-delay .3)
     (setq company-echo-delay 0)
     (setq company-begin-commands '(self-insert-command)))
  :diminish company-mode)

(use-package company-anaconda
  :init (require #'company-anaconda)
  :config (add-to-list 'company-backends 'company-anaconda))

(use-package company-ansible
  :init (require #'company-ansible)
  :config (add-to-list 'company-backends 'company-ansible))

(use-package company-arduino
  :init (require #'company-arduino)
  :config (add-to-list 'company-backends 'company-arduino))

(use-package company-c-headers
  :config (require #'company-c-headers)
  :init (add-to-list 'company-backends 'company-c-headers))

(use-package company-cabal
  :config (require #'company-cabal)
  :init (add-to-list 'company-backends 'company-cabal))

(use-package company-dcd
  :init (require #'company-dcd)
  :config (add-to-list 'company-backends 'company-dcd))

(use-package company-edbi
  :init (require #'company-edbi)
  :config (add-to-list 'company-backends 'company-edbi))

(use-package company-emoji
  :init (require #'company-emoji)
  :config (add-to-list 'company-backends 'company-emoji))

(use-package company-flx
  :init (require #'company-flx)
  :config (add-to-list 'company-backends 'company-flx))

(use-package company-ghci
  :config (require #'company-ghci)
  :init (add-to-list 'company-backends 'company-ghci))

(use-package company-go
  :init (require #'company-go)
  :config (add-to-list 'company-backends 'company-go))

(use-package company-inf-ruby
  :config (require #'company-inf-ruby)
  :init (add-to-list 'company-backends 'company-inf-ruby))

(use-package company-irony
  :init (require #'company-irony)
  :config (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :init (require #'company-irony-c-headers)
  :config (add-to-list 'company-backends 'company-irony-c-headers))

(use-package company-jedi
  :config (require #'company-jedi)
  :init (add-to-list 'company-backends 'company-jedi))

(use-package company-math
  :config (require #'company-math)
  :init (progn (add-to-list 'company-backends 'company-math-symbols-unicode)
               (add-to-list 'company-backends 'company-math-symbols-latex)))

(use-package company-nixos-options
  :init (require #'company-nixos-options)
  :config (add-to-list 'company-backends 'company-nixos-options))

(use-package company-qml
  :init (require #'company-qml)
  :config (add-to-list 'company-backends 'company-qml))

(use-package company-quickhelp
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

(use-package company-racer
  :init (require #'company-racer)
  :config (add-to-list 'company-backends 'company-racer))

(use-package company-restclient
  :config (require #'company-restclient)
  :init (add-to-list 'company-backends 'company-restclient))

(use-package company-shell
  :init (require #'company-shell)
  :config (add-to-list 'company-backends 'company-shell))

(use-package company-sourcekit
  :init (require #'company-sourcekit)
  :config (add-to-list 'company-backends 'company-sourcekit))

(use-package company-statistics
  :init (require #'company-statistics)
  :config (add-to-list 'company-backends 'company-statistics))

(use-package company-tern
  :config (require #'company-tern)
  :init (add-to-list 'company-backends 'company-tern))

(use-package company-try-hard
  :init (require #'company-try-hard)
  :config (add-to-list 'company-backends 'company-try-hard))

(use-package company-web
  :config (require 'company-web)
  :init (add-to-list 'company-backends 'company-web))


;;; Spelling and syntax checking
(use-package ispell                                ; Spell checking
  :if (eq system-type 'darwin)
  :config (progn
    (setq ispell-program-name (if (eq system-type 'darwin)
    (executable-find "aspell")
  (executable-find "hunspell")))
    (setq ispell-dictionary "en_GB")
    (setq ispell-silently-savep t)
    (setq ispell-choices-win-default-height 5)
    (unless ispell-program-name
      (warn
       "No spell checker available.  Install Hunspell or ASpell for OS X."))))

(use-package flyspell                              ; On-the-fly spell checking
  :bind (("C-c T s" . flyspell-mode))
  :if (eq system-type 'darwin)
  :init (progn (dolist (hook '(text-mode-hook message-mode-hook))
     (add-hook hook 'turn-on-flyspell))
       (add-hook 'prog-mode-hook #'flyspell-prog-mode))
  :config (progn
    (setq flyspell-use-meta-tab nil)
    (setq flyspell-issue-welcome-flag nil)
    (setq flyspell-issue-message-flag nil)
    (define-key flyspell-mode-map "\M-\t" nil))
  :diminish flyspell-mode)

(use-package flycheck
  :bind (("C-c l e" . list-flycheck-errors)
     ("C-c T f" . flycheck-mode))
  :config (progn (global-flycheck-mode t)
     (set-face-attribute 'flycheck-error-list-checker-name nil
   :inherit 'italic)
     (setq flycheck-completion-system 'ido)
     (setq flycheck-indication-mode 'left-fringe)
     (use-package flycheck-color-mode-line
       :init (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode)))
  :diminish flycheck-mode)

(use-package flycheck-pos-tip                      ; Show Flycheck messages in popups
  :init (with-eval-after-load 'flycheck
      (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))


(use-package volatile-highlights
  :init (require #'volatile-highlights)
  :config (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)

;;; Text editing
(use-package tildify
  :bind (("C-c u t" . tildify-region))
  :init (dolist (hook '(markdown-mode-hook latex-mode-hook rst-mode-hook))
      (add-hook hook #'tildify-mode))
  :config (add-hook 'latex-mode-hook (lambda ()
     (setq-local tildify-space-string "~"))))

(use-package typo
  :bind (("C-c T t" . typo-mode))
  :init (progn (typo-global-mode)
       (dolist (hook '(markdown-mode-hook rst-mode-hook))
     (add-hook hook #'typo-mode))))

(use-package delsel
  :init (delete-selection-mode))

(use-package hungry-delete
  :init (require #'hungry-delete)
  :config (global-hungry-delete-mode))


;;; LaTeX with AUCTeX
(use-package tex-site)

(use-package tex
  :config (progn
    (setq TeX-parse-self t                   ; Parse documents to pro∫vide completion
      TeX-auto-save t                    ; Automatically save style information
      TeX-electric-sub-and-superscript t ; Automatically insert braces after
      TeX-electric-math '("\\(" "\\)") TeX-quote-after-quote t TeX-clean-confirm nil
      TeX-source-correlate-mode t TeX-source-correlate-method 'synctex)
    (setq-default TeX-master nil     ; Ask for the master file
      TeX-engine 'luatex ; Use a modern engine
      TeX-PDF-mode t)
    (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s")))

(use-package tex-buf                               ; TeX buffer management
  :config (setq TeX-save-query nil))

(use-package tex-style                             ; TeX style
  :config (setq LaTeX-csquotes-close-quote "}" LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold                              ; TeX folding
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package tex-mode                              ; TeX mode
  :config (font-lock-add-keywords
       'latex-mode
       `((,(rx "\\" symbol-start "fx" (1+ (or (syntax word)
    (syntax symbol))) symbol-end) . font-lock-warning-face))))

(use-package latex                                 ; LaTeX editing
  :config (progn
    (setq TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
    (,(rx (0+ space) "\\subsection*{") 3)
    (,(rx (0+ space) "\\subsubsection*{") 4)
    (,(rx (0+ space) "\\minisec{") 5)) LaTeX-babel-hyphen nil)
    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode))) ; Easy math input

(use-package auctex-latexmk
  :config (auctex-latexmk-setup))

(use-package company-auctex
  :init (require #'company-auctex)
  :config (add-to-list 'company-backends 'company-auctex))

(use-package bibtex                                ; BibTeX editing
  :config (progn (add-hook 'bibtex-mode-hook (lambda ()
     (run-hooks 'prog-mode-hook)))
     (bibtex-set-dialect 'biblatex)))

(defun my-reftex-find-ams-environment-caption (environment)
  "Find the caption of an AMS ENVIRONMENT."
  (let ((re (rx-to-string `(and "\\begin{"
    ,environment
    "}"))))
    (re-search-backward re)
    (goto-char (match-end 0)))
  (if (not (looking-at (rx (zero-or-more space) "[")))
      (error
       "Environment %s has no title"
       environment)
    (let ((beg (match-end 0)))
      (goto-char (1- beg))
      (forward-list)
      (buffer-substring-no-properties
       beg
       (1- (point))))))

(use-package reftex                                ; TeX/BibTeX cross-reference management
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config (progn
    (setq reftex-plug-into-AUCTeX t reftex-insert-label-flags '(t t) reftex-label-alist '(("definition"
         ?d
         "def:"
         "~\\ref{%s}"
         my-reftex-find-ams-environment-caption
         ("definition"
    "def.")
         -3)
        ("theorem"
         ?h
         "thm:"
         "~\\ref{%s}"
         my-reftex-find-ams-environment-caption
         ("theorem"
    "th.")
         -3)
        ("example"
         ?x
         "ex:"
         "~\\ref{%s}"
         my-reftex-find-ams-environment-caption
         ("example"
    "ex")
         -3)
        ("algorithm"
         ?a
         "alg:"
         "~\\ref{%s}"
         "\\\\caption[[{]"
         ("algorithm"
    "alg")
         -3)))
    (unless (assq 'biblatex reftex-cite-format-builtin)
      (add-to-list 'reftex-cite-format-builtin '(biblatex "The biblatex package" ((?\C-m .
       "\\cite[]{%l}")
        (?t .
        "\\textcite{%l}")
        (?a .
        "\\autocite[]{%l}")
        (?p .
        "\\parencite{%l}")
        (?f .
        "\\footcite[][]{%l}")
        (?F .
        "\\fullcite[]{%l}")
        (?x .
        "[]{%l}")
        (?X .
        "{%l}"))))
      (setq reftex-cite-format 'biblatex)))
  :diminish reftex-mode)

;; EShell
(use-package eshell
  :bind ("M-e" . eshell)
  :config (progn
    ;; Scrolling
    (setq eshell-scroll-to-bottom-on-output t eshell-scroll-show-maximum-output t
      eshell-save-history-on-exit t eshell-buffer-shorthand t)
    (use-package esh-mode
      :config (progn
    (defun eshell/cds ()
      (eshell/cd (or (locate-dominating-file default-directory "src")
   (locate-dominating-file default-directory ".git"))))
    (defun eshell/clear ()
      (interactive)
      (let ((inhibit-read-only t))
    (delete-region (point-min)
     (point-max)))
      (eshell-send-input))
    (add-hook 'eshell-mode-hook (lambda ()
    (bind-key "C-l" 'eshell/clear
  eshell-mode-map)))))
    (use-package eshell-opt
      :config (use-package
    eshell-prompt-extras))
    (use-package em-term
      :config (setq eshell-visual-commands (append '("tmux" "screen" "ssh")
     eshell-visual-commands)))
    (use-package em-hist
      :config (setq eshell-hist-ignoredups t)))
  (use-package em-smart
    :config (progn
      (setq eshell-where-to-jump 'begin)
      (setq eshell-review-quick-commands nil)
      (setq eshell-smart-space-goes-to-end t))))

(add-hook 'emacs-lisp-mode-hook #'flyspell-prog-mode)

;;; Other markup languages
(use-package rst                                   ; ReStructuredText
  :config (setq rst-indent-literal-minimized 3 rst-indent-literal-normal 3)
  (bind-key "C-=" nil rst-mode-map)
  (bind-key  "C-c C-j" #'rst-insert-list rst-mode-map))

(use-package markdown-mode                         ; Markdown
  :mode ("/itsalltext/.*\\.md\\'" . gfm-mode)
  :config (progn (let ((stylesheet (expand-file-name (locate-user-emacs-file "etc/pandoc.css"))))
       (setq markdown-command (mapconcat #'shell-quote-argument `("pandoc" "--toc"
        "--section-divs"
        "--css" ,(concat
      "file://"
      stylesheet)
        "--standalone" "-f"
        "markdown" "-t"
        "html5") " ")))
     (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
     (add-hook 'gfm-mode-hook #'visual-line-mode)
     (add-hook 'gfm-mode-hook #'my-whitespace-style-no-long-lines)
     (bind-key "C-c C-s C" #'markdown-insert-gfm-code-block markdown-mode-map)
     (bind-key "C-c C-s P" #'markdown-insert-gfm-code-block markdown-mode-map)
     (bind-key "M-q" #'ignore gfm-mode-map)))

(use-package yaml-mode                             ; YAML
  :config (add-hook 'yaml-mode-hook (lambda ()
    (run-hooks 'prog-mode-hook))))

(use-package graphviz-dot-mode                     ; Graphviz
  :config (setq graphviz-dot-indent-width 4))


;;; Java
(use-package grails-mode
  :diminish grails-mode)

(use-package malabar-mode
  :config (activate-malabar-mode))

(use-package java-imports
  :config
  :bind ("M-I" . java-imports-add-import)
  :config
  (progn
    (require 'java-imports)
    (setq java-imports-find-block-function 'java-imports-find-place-sorted-block)))

(use-package java-snippets)

(use-package nexus)

(use-package mvn)


;;; Programming utilities
(use-package eldoc
  :diminish eldoc-mode
  :init (add-hook 'prog-mode-hook #'eldoc-mode))

(use-package eldoc-extension
  :init (require #'eldoc-extension))

(use-package hl-todo
  :init
  (progn
    (dolist
    (hook '(prog-mode-hook text-mode-hook))
      (add-hook hook #'hl-todo-mode))))

(use-package hl-sexp
  :init (add-hook 'prog-mode-hook #'hl-sexp-mode))

(use-package ggtags
  :config
  (add-hook
   'c-mode-common-hook
   (lambda ()
     (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
       (ggtags-mode 1)
       (setq-local eldoc-documentation-function #'ggtags-eldoc-function)))))

(use-package compile                               ; Compile from Emacs
  :bind (("C-c c" . compile)
     ("C-c C" . recompile))
  :config
  (progn
    (setq compilation-always-kill t )
    (setq compilation-scroll-output 'first-error)
    (setq compilation-ask-about-save nil)))

(use-package highlight-numbers
  :init
  (progn
    (dolist (hook '(prog-mode-hook text-mode-hook LaTeX-mode-hook))
      (add-hook hook #'highlight-numbers-mode))))

(use-package highlight-symbol
  :bind (("C-c s %" . highlight-symbol-query-replace)
     ("C-c s n" . highlight-symbol-next-in-defun)
     ("C-c s o" . highlight-symbol-occur)
     ("C-c s p" . highlight-symbol-prev-in-defun))
  :init
  (progn
    (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
    (add-hook 'prog-mode-hook #'highlight-symbol-mode))
  :config
  (progn
    (setq highlight-symbol-idle-delay 0.4)
    (setq highlight-symbol-on-navigation-p t))
  :diminish highlight-symbol-mode)

(use-package elide-head                            ; Elide lengthy GPL headers
  :bind (("C-c u h" . elide-head))
  :init (add-hook 'prog-mode-hook #'elide-head))

(use-package electric-spacing
  :config (add-hook 'prog-mode-hook #'electric-spacing-mode))

(use-package electric-case
  :config
  (progn
    (setq electric-case-convert-calls t)
    (add-hook 'ahk-mode-hook #'electric-case-ahk-init)
    (add-hook 'c-mode-hook #'electric-case-c-init)
    (add-hook 'scala-mode-hook #'electric-case-scala-init)))

(use-package restclient
  :init (require #'restclient))

(use-package smartrep)

(use-package eide
  :config (eide-start))

(use-package smart-forward
  :init (require #'smart-forward)
  :bind (("M-<up>" . smart-up)
     ("M-<down>" . smart-down)
     ("M-<left>" . smart-backward)
     ("M-<right>" . smart-forward)))

(use-package change-inner
  :init (require #'change-inner)
  :bind (("M-i" . change-inner)
     ("M-o" . change-outer)))

(use-package disaster
  :init (require #'disaster)
  :config (define-key c-mode-base-map (kbd "C-c d") 'disaster))

(use-package autodisass-java-bytecode
  :init (require #'autodisass-java-bytecode))

(use-package llvm-mode
  :init
  (progn
    (require #'llvm-mode)
    (require #'tablegen-mode)))

(use-package autodisass-llvm-bitcode
  :init (require #'autodisass-llvm-bitcode))

;;; Generic Lisp

(use-package paredit
  :init
  (progn
    (dolist
      (hook '(eval-expression-minibuffer-setup-hook
      emacs-lisp-mode-hook
      inferior-emacs-lisp-mode-hook
      clojure-mode-hook))
      (add-hook hook #'paredit-mode)))
  :config
    (progn
      (define-key paredit-mode-map (kbd "M-s") nil)
      (define-key paredit-mode-map (kbd "M-S-<up>") #'paredit-splice-sexp))
    :diminish paredit-mode)

(use-package paxedit
  :bind (("M-<right>". paxedit-transpose-forward)
     ("M-<left>". paxedit-transpose-backward)
     ("M-<up>". paxedit-backward-up)
     ("M-<down>" . paxedit-backward-end)
     ("C-M-b" . paxedit-previous-symbol)
     ("C-M-f" . paxedit-next-symbol)
     ("C-%" . paxedit-copy)
     ("C-&" . paxedit-kill)
     ("C-*" . paxedit-delete)
     ("C-^" . paxedit-sexp-raise)
     ("M-u" . paxedit-symbol-change-case)
     ("C-@" . paxedit-symbol-copy)
     ("C-#" . paxedit-symbol-kill))
  :config
  (progn (add-hook 'clojure-mode-hook 'paxedit-mode)
     (add-hook 'elisp-mode-hook 'paxedit-mode))
  :diminish paxedit-mode)

(use-package elisp-slime-nav
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  :diminish elisp-slime-nav-mode)

(use-package flycheck-cask
  :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(use-package flycheck-package
  :init (with-eval-after-load 'flycheck (flycheck-package-setup)))

(use-package pcre2el
  :init (rxt-global-mode))

(use-package macrostep
  :init (with-eval-after-load 'lisp-mode (bind-key "C-c e" #'macrostep-expand emacs-lisp-mode-map)
      (bind-key "C-c e" #'macrostep-expand lisp-interaction-mode-map)))

(use-package ielm
  :bind (("C-c u z" . ielm)))

(use-package lisp-mode
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode ("/Cask\\'" . emacs-lisp-mode)
  :init (require #'ert))

(bind-key "C-c T d" #'toggle-debug-on-error)

(use-package racket-mode
  :config (add-hook 'racket-mode-hook (lambda ()
  (define-key racket-mode-map (kbd "C-c r") 'racket-run)
  (define-key racket-mode-map (kbd "C-c t") 'racket-test))))

(use-package geiser
  :init (setq geiser-active-implementations '(racket chicken guile)))

;;; Cucumber
(use-package feature-mode                          ; Feature files for ecukes/cucumber
  :config (progn
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
  :mode ("\\.md\\'" . markdown-mode))

;;; CSV
(use-package csv-mode
  :mode "\\.csv\\'")


;;; Clojure
(use-package cider
  :config (progn (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
     (add-hook 'cider-mode-hook 'company-mode)
     (add-hook 'cider-repl-mode-hook 'company-mode)
     (add-hook 'cider-repl-mode-hook 'paredit-mode)
     (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
     (setq nrepl-log-messages t nrepl-hide-special-buffers t cider-repl-tab-command
       #'indent-for-tab-command cider-prefer-local-resources t
       cider-repl-pop-to-buffer-on-connect nil)))

(use-package cider-decompile)

(use-package cider-eval-sexp-fu
  :init (require #'cider-eval-sexp-fu))

(use-package nrepl-eval-sexp-fu
  :config
    (progn
      (require 'highlight)
      (require 'nrepl-eval-sexp-fu)))

(use-package clojure-mode
  :config (add-hook 'clojure-mode-hook #'smartparens-strict-mode))

(use-package clojure-mode-extra-font-locking
  :init (require #'clojure-mode-extra-font-locking))

(use-package inf-clojure
  :config (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode))

(use-package clj-refactor)

(use-package clojure-snippets)

(use-package flycheck-clojure
  :config (flycheck-clojure-setup))

(use-package 4clojure)


;;; Scala
(defconst my-scalastyle-version '("0.8.0" . "2.11")
  "Version of scala style to use for Flycheck.

A pair of `(VERSION . SCALA-VERSION)'.")

(defconst my-scalastyle-jar
  (pcase-let ((`(,version . ,scala-version) my-scalastyle-version))
    (format "scalastyle_%s-%s-batch.jar" scala-version version))
  "Name of the scalastyle JAR.")

(defconst my-scalastyle-url
  (pcase-let ((`(,version . ,scala-version) my-scalastyle-version))
    (format
     "https://oss.sonatype.org/content/repositories/releases/org/scalastyle/scalastyle_%s/%s/%s"
     scala-version version my-scalastyle-jar))
  "URL to get scalastyle from.")

(use-package scala-mode2                           ; Scala editing
  :config (progn (let ((filename (locate-user-emacs-file my-scalastyle-jar)))
       (unless (file-exists-p filename)
     (message "Downloading scalastyle JAR")
     (url-copy-file my-scalastyle-url filename))
       (with-eval-after-load 'flycheck
     (setq flycheck-scalastyle-jar (expand-file-name filename) flycheck-scalastylerc
       "scalastyle-config.xml")))))

(use-package sbt-mode                              ; Scala build tool
  :config (add-hook 'scala-mode-hook #'ensime-scala-mode-hook))

(use-package ensime
  :disabled t)

;;; Python
(use-package python
  :config (progn
    (setq python-check-command "pylint" python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")))

(use-package jedi
  :commands jedi:setup
  :init (progn (add-hook 'python-mode-hook #'jedi:setup))
  :config (progn
    (setq jedi:setup-keys t)
    (setq jedi:complete-on-dot t)))

(use-package anaconda-mode                         ; Powerful Python backend for Emacs
  :diminish anaconda-mode
  :init (add-hook 'python-mode-hook #'anaconda-mode))


(use-package pip-requirements)

(use-package python-django
  :init (require #'python-django))

(use-package ein
  :init (require #'ein)
  :config (setq ein:use-smartrep t))

(use-package elpy
  :config (progn (elpy-enable)
     (elpy-use-ipython)
     (elpy-clean-modeline)))

(use-package pylint
  :config (progn (add-hook 'python-mode-hook #'pylint-add-menu-items)
     (add-hook 'python-mode-hook #'pylint-add-key-bindings)))

(use-package flycheck-pyflakes
  :init (require #'flycheck-pyflakes)
  :config (add-hook 'python-mode-hook #'flycheck-mode))

;;; Go
(use-package go-direx
  :init (require #'go-direx)
  :bind ("C-c C-j" . go-direx-pop-to-buffer))

(use-package go-errcheck)

(use-package go-mode
  :bind (("M-." . godef-jump)
     ("C-c C-c" . go-run))
  :config (progn
    (setq gofmt-command "goimports")
    (add-hook 'go-mode-hook (lambda ()
    ((add-hook 'before-save-hook #'gofmt-before-save)
     (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
     (local-set-key (kbd "C-c i") 'go-goto-imports)
     (local-set-key (kbd "M-.") 'godef-jump)
     (if (not (string-match "go" compile-command))
     (set (make-local-variable 'compile-command)
  "go build -v && go test -v && go vet")))))))

(use-package go-eldoc
  :init (progn
      (require #'go-eldoc)
      (add-hook 'go-mode-hook #'go-eldoc-setup)))


(use-package go-snippets)

(use-package go-stacktracer)
(use-package golint)
(use-package gotest)


;;; C / C++
(use-package c-eldoc
  :config (add-hook 'c-mode-hook #'c-turn-on-eldoc-mode))

(use-package cmake-ide)

(use-package srefactor
  :init (progn
      (require #'srefactor)
      (semantic-mode 1))
  :config (progn (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
     (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)))

(use-package flycheck-google-cpplint
  :init (progn
      (require #'flycheck-google-cpplint)
      (flycheck-add-next-checker 'c/c++-clang 'c/c++-googlelint 'append)))

;;; Ruby
(use-package inf-ruby                              ; Ruby REPL
  :init (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  :config (inf-ruby-switch-setup))

(use-package ruby-refactor
  :init
  (progn
    (require #'ruby-refactor)
    (add-hook 'ruby-mode-hook #'ruby-refactor-mode-launch)))

(use-package robe                                  ; Ruby backend for Emacs
  :config (add-to-list 'company-backends 'company-robe))

;;; Rust
(use-package rust-mode
  :init (require #'rust-mode))

(use-package racer
  :config
  (progn
    ((setq racer-rust-src-path "")
     (setq racer-cmd "")
     (add-hook 'rust-mode-hook #'racer-mode))))

(use-package cargo
  :config
  (progn
    ((add-hook 'rust-mode-hook #'cargo-minor-mode))))

(use-package flycheck-rust                         ; Flycheck setup for Rust
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package toml-mode)

(with-eval-after-load 'rust-mode (add-hook 'rust-mode-hook #'yas-minor-mode-on)
      (add-hook 'rust-mode-hook (lambda ()
    (setq compile-command (rust-compile-command
       (rust-test-file-p
        buffer-file-name)))))
      (define-key rust-mode-map (kbd "C-c C-c") 'rust-compile-and-maybe-run))

(defun rust-test-file-p (filename)
  (string-match-p (rx not-newline word-boundary "test.rs" string-end) filename))

(defun rust-compile-command
    (&optional
     run)
  (when buffer-file-name (let* ((source-file (file-name-nondirectory buffer-file-name))
    (executable (file-name-sans-extension source-file))
    (cmd (format (if (rust-test-file-p source-file) "rustc --test %s"
     "rustc %s") source-file)))
       (if run (concat cmd (pcase system-type (`windows-nt " && ")
    (_ " && ./")) executable) cmd))))

(defun rust-compile-and-maybe-run
    (&optional
     force-run)
  "Compile current file, when it is a test file or when command
is run with prefix argument - also execute resulting binary."
  (interactive "P")
  (let ((compile-command (rust-compile-command (or force-run
     (rust-test-file-p buffer-file-name)))))
    (recompile)))

;;; .NET
(use-package csharp-mode)

(use-package fsharp-mode
  :mode "\\.fs[iylx]?$")

(use-package omnisharp)

;;; Haskell
(use-package haskell-mode
  :config (progn (add-hook 'haskell-mode-hook 'turn-on-hi2)
     (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode) ; Scan and navigate
     (add-hook 'haskell-mode-hook #'haskell-auto-insert-module-template)
     (setq haskell-tags-on-save t haskell-process-suggest-remove-import-lines t
       haskell-process-auto-import-loaded-modules t
       haskell-process-use-presentation-mode t haskell-process-show-debug-tips nil
       haskell-process-log t haskell-process-suggest-hoogle-imports nil
       haskell-process-suggest-hayoo-imports t haskell-process-type 'cabal-repl)
     (bind-keys :map haskell-mode-map ("C-c C-l" . haskell-process-load-or-reload)
    ("C-c C-z" . haskell-interactive-switch)
    ("C-c C-n C-t" . haskell-process-do-type)
    ("C-c C-n C-i" . haskell-process-do-info)
    ("C-c C-n C-c" . haskell-process-cabal-build)
    ("C-c C-n c" . haskell-process-cabal)
    ("SPC" . haskell-mode-contextual-space))
     (bind-key "C-c h d" #'haskell-describe haskell-mode-map)
     (bind-key "C-c h h" #'haskell-hayoo haskell-mode-map)
     (bind-key "C-c h H" #'haskell-hoogle haskell-mode-map)
     (bind-key "C-c h i" #'haskell-navigate-imports haskell-mode-map)
     (bind-key "C-c h c" #'haskell-cabal-visit-file haskell-mode-map)))

(use-package haskell
  :init (dolist (hook '(haskell-mode-hook haskell-cabal-mode-hook))
      (add-hook hook #'interactive-haskell-mode))
  :config (progn (bind-key "C-c C-t" #'haskell-mode-show-type-at interactive-haskell-mode-map)
     (bind-key "M-." #'haskell-mode-goto-loc interactive-haskell-mode-map)
     (bind-key "C-c u u" #'haskell-mode-find-uses interactive-haskell-mode-map)))

(use-package ghc
  :config (progn (autoload 'ghc-init "ghc" nil t)
     (autoload 'ghc-debug "ghc" nil t)
     (setq ghc-ghc-options '("-fno-warn-unused-do-bind"))
     (add-hook 'haskell-mode-hook (lambda ()
  (ghc-init)))))

(use-package shm
  :config (progn (add-hook 'haskell-mode-hook #'structured-haskell-mode)
     (define-key shm-map (kbd "M-s") nil)
     (define-key shm-map (kbd "M-S") 'shm/splice)
     (require #'shm-reformat)
     (setq hindent-style "chris-done")
     (bind-key "C-c i" 'shm-reformat-decl haskell-mode-map)))

(use-package haskell-interactive-mode)

(use-package haskell-simple-indent                 ; Primitive Haskell indentation
  :init (add-hook 'haskell-mode-hook #'haskell-simple-indent-mode))

(use-package hindent                               ; Automated Haskell indentation
  :init (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package flycheck-haskell                      ; Setup Flycheck from Cabal projects
  :init (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(use-package flycheck-ghcmod
  :init (require #'flycheck-ghcmod))

(use-package company-ghc
  :config (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code)))


;;; Erlang
(use-package erlang
  :config (require #'erlang-start))

(use-package edts
  :config (require #'edts-start))


;;; Elixir
(use-package alchemist
  :config (setq alchemist-project-compile-when-needed t)
  (setq alchemist-goto-erlang-source-dir "/path/to/erlang/source/")
  (setq alchemist-goto-elixir-source-dir "/path/to/elixir/source/"))

(use-package elixir-mode
  :init (add-to-list 'auto-mode-alist '("\\.elixir2\\'" . elixir-mode))
  :config (add-hook 'elixir-mode-hook (lambda ()
  (and (file-exists (buffer-file-name))
   (file-exists (elixir-mode-compiled-file-name))
   (elixir-cos-mode t)))))

(use-package elixir-yasnippets)

;;; Julia
(use-package julia-mode
  :init (require #'julia-mode))

;;; Idris
(use-package idris-mode)


(use-package kivy-mode
  :init (require #'kivy-mode)
  :mode ("\\.kv$" . kivy-mode))

;;; OCaml
(use-package tuareg                                ; OCaml editing
  :config (progn
    (setq tuareg-use-smie nil)
    (define-key tuareg-mode-map [?\C-c ?i] nil)))

(use-package merlin                                ; Powerful Emacs backend for OCaml
  :init (add-hook 'tuareg-mode-hook #'merlin-mode)
  :config (progn
    (setq merlin-error-after-save nil)
    (add-to-list 'company-backends 'merlin-company-backend)))

(use-package flycheck-ocaml                        ; Check OCaml code with Merlin
  :init (with-eval-after-load 'merlin (flycheck-ocaml-setup)))

;;; Web languages

(use-package web-mode                              ; Template editing
  :config (setq web-mode-markup-indent-offset 2))

(use-package coffee-mode
  :config (custom-set-variables '(coffee-tab-width 2)))

(use-package js2-mode                              ; Javascript editing
  :mode "\\.js\\(?:on\\)?\\'"
  :config (setq-default js2-basic-offset 2))

(use-package js2-refactor
  :init (require #'js2-refactor))

(use-package tern
  :config (add-hook 'js-mode-hook #'tern-mode))

(use-package css-mode
  :config (progn (add-hook 'css-mode-hook (lambda ()
  (run-hooks 'prog-mode-hook)))
     (put 'css-indent-offset 'safe-local-variable #'integerp)))

(use-package css-eldoc                             ; Basic Eldoc for CSS
  :commands (turn-on-css-eldoc)
  :init (add-hook 'css-mode-hook #'turn-on-css-eldoc))

(use-package php-mode)

;;; Misc programming languages
(use-package sh-script                             ; Shell scripts
  :mode ("\\.zsh\\'" . sh-mode)
  :config (setq sh-indentation 2        ; The basic indentation
    sh-basic-offset 2       ; The offset for nested indentation
    ))

(use-package puppet-mode                           ; Puppet manifests
  :config (setq puppet-fontify-variables-in-comments t))

(use-package nxml-mode                             ; XML editing
  :config (setq nxml-slash-auto-complete-flag t nxml-auto-insert-xml-declaration-flag t))

(use-package feature-mode                          ; Feature files for ecukes/cucumber
  :config (progn (add-hook 'feature-mode-hook #'flyspell-mode)))

(use-package cmake-mode                            ; CMake files
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
     ("\\.cmake\\'" . cmake-mode)))

(use-package thrift)

(use-package protobuf-mode
  :init (require #'protobuf-mode))

(use-package swift-mode                            ; Swift sources
  :config (with-eval-after-load 'flycheck (add-to-list 'flycheck-checkers 'swift)))


;;; Databases
(use-package sql
  :bind (("C-c d m" . sql-mysql)))


;;; Version control
(use-package vc-hooks                              ; Simple version control
  :config (setq vc-follow-symlinks t))

(use-package diff-hl
  :config (progn (global-diff-hl-mode)
     (add-hook 'dired-mode-hook #'diff-hl-dired-mode)))

(use-package magit
  :bind
  (("C-c g" . magit-status)
   ("C-c f" . magit-grep)
   ("C-c v g" . magit-blame-mode))
  :config
  (progn
    (setq magit-save-some-buffers 'dontask
      magit-stage-all-confirm nil
      magit-unstage-all-confirm nil
      magit-set-upstream-on-push t
      magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
      magit-restore-window-configuration t
      magit-completing-read-function #'magit-ido-completing-read))
  :init
  (progn
    (autoload 'magit-status "magit" nil t)
    (require 'magit)
    ))

(use-package magit-filenotify
  :config (add-hook 'magit-status-mode-hook #'magit-filenotify-mode))

(use-package magit-find-file
  :init (require #'magit-find-file))

(use-package magit-gitflow
  :init (require #'magit-gitflow)
  :config (add-hook 'magit-mode-hook #'turn-on-magit-gitflow))

(use-package magit-annex)

(use-package magit-stgit)

(use-package magit-rockstar)

(use-package magit-topgit)

(use-package magit-gh-pulls)

(use-package magit-gerrit)

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package gitattributes-mode)

(use-package git-timemachine
  :bind ("C-c v t" . git-timemachine))

(use-package git-blame)

(use-package git-messenger)

(use-package yagist)

(use-package gh)

(use-package github-browse-file)

(use-package github-clone)


;;; Tools and utilities

(use-package projectile                            ; Project management
  :bind (("C-c p s" . projectile-switch-project)
     ("C-c p f" . projectile-files-in-project-directory )
     ("C-c p k" . projectile-kill-buffers))
  :init (progn
      (require #'projectile)
      (projectile-global-mode))
  :config (progn (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
     (setq projectile-completion-system 'ido projectile-enable-caching t
       projectile-require-project-root nil projectile-find-dir-includes-top-level t
       projectile-mode-line
       '(:propertize (:eval (concat " " (projectile-project-name)))
   face
   font-lock-constant-face))
     (def-projectile-commander-method ?a "Find ag on project." (call-interactively
      'projectile-ag))
     (bind-key "s a" #'ag-project-regexp projectile-command-map)
     (bind-key "s A" #'ag-project projectile-command-map)
     (bind-key "s d" #'ag-project-dired-regexp projectile-command-map)
     (bind-key "s D" #'ag-project-dired projectile-command-map)
     (bind-key "s f" #'ag-project-files projectile-command-map)
     (bind-key "s k" #'ag-kill-other-buffers projectile-command-map)
     (bind-key "s K" #'ag-kill-buffers projectile-command-map))
  :diminish projectile-mode)

(use-package ibuffer-projectile)

(use-package go-projectile
  :init (require #'go-projectile))

(use-package discover
  :init (require #'discover)
  :config (global-discover-mode t))

(use-package grails-projectile-mode
  :init
  (progn
    (require #'grails-projectile-mode)
    (grails-projectile-global-mode t)
    (require #'grails-projectile-discover)
    (grails-projectile-discover-setup-keybindings))
  :diminish grails-projectile-mode)

(use-package org-projectile
  :bind (("C-c n p" . org-projectile:project-todo-completing-read)
     ("C-c c" . org-capture))
  :config
  (progn
    (setq org-projectile:projects-file "projects.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
    (add-to-list 'org-capture-templates (org-projectile:project-todo-entry "p"))))

(use-package projectile-rails
  :init (require #'projectile-rails)
  :config (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package projectile-speedbar
  :bind ("C-c p s" . projectile-speedbar-open-current-file-in-tree)
  :init (require #'projectile-speedbar))

(use-package bug-reference                         ; Turn bug references into buttons
  :init (progn (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
       (add-hook 'text-mode-hook #'bug-reference-mode)))

(use-package paradox
  :bind (("C-c l p" . paradox-list-packages)
     ("C-c l P" . package-list-packages-no-fetch))
  :config (setq paradox-github-token '803d33a93c2d337a2e8ad7bd5fd4581b9b6ab7f4
    paradox-execute-asynchronously t))

(use-package proced                                ; Edit system processes
  :if (not (eq system-type 'darwin))
  :bind ("C-x p" . proced))

(use-package calendar                              ; Built-in calendar
  :bind ("C-c u c" . calendar)
  :config (setq calendar-week-start-day 1))

(use-package time                                  ; Show current time
  :bind (("C-c u i" . emacs-init-time)
     ("C-c u T" . display-time-world))
  :config (setq display-time-world-time-format "%H:%M %Z, %d. %b" display-time-world-list '(("Europe/Berlin"
       "Berlin")
      ("Europe/London"
       "London")
      ("Europe/Istanbul"
       "Istanbul")
      ("America/Winnipeg"
       "Winnipeg (CA)")
      ("America/New_York"
       "New York (USA)")
      ("Asia/Tokyo"
       "Tokyo (JP)"))))

(use-package sr-speedbar
  :init (require #'sr-speedbar)
  :bind ("C-c C-s" . sr-speedbar-toggle))

(use-package bug-hunter)


;;; Terminal emulation and shells
(use-package shell                                 ; Dump shell in Emacs
  :bind ("C-c u s" . shell))

(use-package term                                  ; Terminal emulator in Emacs
  :bind ("C-c u S" . ansi-term))

;;; Net & Web
(use-package eww                                   ; Emacs' built-in web browser
  :bind (("C-c w b" . eww-list-bookmarks)
     ("C-c w w" . eww)))

(use-package sx                                    ; StackExchange client for Emacs
  :bind (("C-c w s" . sx-tab-frontpage)
     ("C-c w S" . sx-tab-newest)
     ("C-c w a" . sx-ask)))

(use-package sx-compose
  :config (progn (add-hook 'sx-compose-mode-hook #'turn-off-auto-fill)
     (add-hook 'sx-compose-mode-hook #'visual-line-mode)
     (bind-key "M-q" #'ignore sx-compose-mode-map)))

(use-package sx-question-mode
  :config (setq sx-question-mode-display-buffer-function #'switch-to-buffer))

(use-package sendmail                              ; Send mails from Emacs
  :config (setq send-mail-function 'smtpmail-send-it))

(use-package message                               ; Compose mails from Emacs
  :config (setq message-send-mail-function 'smtpmail-send-it message-kill-buffer-on-exit t))

(use-package smtpmail
  :config (setq smtpmail-smtp-server "vega.uberspace.de" smtpmail-smtp-service 587
    smtpmail-stream-type 'starttls smtpmail-smtp-user user-login-name))

(use-package hackernews
  :init (require #'hackernews)
  :config (setq hackernews-top-story-limit 50))

(use-package 2048-game)

(use-package erc
  :config (progn
    (setq erc-server "chat.freenode.net" erc-port 7000 erc-nick "sarvex" erc-nick-uniquifier
      "_" erc-server-connect-function 'erc-open-tls-stream)
    (add-to-list 'erc-modules 'spelling)
    (erc-update-modules)))

(use-package erc-join                              ; Automatically join channels with ERC
  :config (setq erc-autojoin-channels-alist '(("\\.freenode\\.net" . ("#emacs")))))

(use-package erc-track                             ; Track status of ERC in mode line
  :config (setq erc-track-switch-direction 'newest erc-track-enable-keybindings t))

(use-package rcirc                                 ; Simply ERC client
  :config (progn
    (setq rcirc-default-full-name (format "%s (http://www.sarvex.com)" user-full-name)
      rcirc-default-nick "sarvex" rcirc-time-format "%Y-%m-%d %H:%M " rcirc-server-alist
      '(("chat.freenode.not" :port 7000
     :user-name "sarvex"
     :encryption tls
     :channels ("#emacs" "#haskell" "#hakyll" "#zsh"))))
    (add-hook 'rcirc-mode-hook #'flyspell-mode)
    (rcirc-track-minor-mode)))

;;; * Org Mode

(use-package org
  :diminish (orgstruct-mode . "")
  :config (progn
    (setq org-catch-invisible-edits 'smart)
    (setq org-structure-template-alist '(("s" "#+begin_src ?\n\n#+end_src"
    "<src lang=\"?\">\n\n</src>")
   ("e" "#+begin_example\n?\n#+end_example"
    "<example>\n?\n</example>")
   ("q" "#+begin_quote\n?\n#+end_quote"
    "<quote>\n?\n</quote>")
   ("v" "#+begin_verse\n?\n#+end_verse"
    "<verse>\n?\n</verse>")
   ("v" "#+begin_verbatim\n?\n#+end_verbatim"
    "<verbatim>\n?\n</verbatim>")
   ("c" "#+begin_center\n?\n#+end_center"
    "<center>\n?\n</center>")
   ("l" "#+begin_latex\n?\n#+end_latex"
    "<literal style=\"latex\">\n?\n</literal>")
   ("l" "#+latex: "
    "<literal style=\"latex\">?</literal>")
   ("h" "#+begin_html\n?\n#+end_html"
    "<literal style=\"html\">\n?\n</literal>")
   ("h" "#+html: "
    "<literal style=\"html\">?</literal>")
   ("a" "#+begin_ascii\n?\n#+end_ascii" "")
   ("a" "#+ascii: " "")
   ("i" "#+index: ?" "#+index: ?")
   ("i" "#+include: %file ?"
    "<include file=%file markup=\"?\">")))
    (add-hook 'prog-mode-hook 'turn-on-orgstruct)
    (defun orgstruct-lisps-turn-on ()
      (setq orgstruct-heading-prefix-regexp ";; "))
    (add-hook 'lisps-mode-hook #'orgstruct-lisps-turn-on)
    (use-package org-capture
      :bind ("C-c c" . org-capture)
      :config (progn
    (setq org-reverse-note-order t org-capture-templates '(("d" "Dev dump" entry
      (file
       "~/org/dev.org")
      "* %?\n  %i\n %a"
      :kill-buffer  t)
         ("j" "Journal" entry
      (file
       "~/org/journal.org")
      "* %U\n %?i\n %a"
      :kill-buffer t)))))
    (use-package org-clock
      :config (setq org-clock-idle-time 15 org-clock-in-resume t org-clock-persist t
    org-clock-persist-query-resume nil org-clock-clocked-in-display 'both))))

;;; Online Help
(use-package find-func                             ; Find function/variable definitions
  :bind (("C-x F"   . find-function)
     ("C-x 4 F" . find-function-other-window)
     ("C-x K"   . find-function-on-key)
     ("C-x V"   . find-variable)
     ("C-x 4 V" . find-variable-other-window)))

(use-package apropos                               ; Search symbols for documentation
  :bind ("C-c h a" . apropos))

(use-package ansible-doc
  :init (add-hook 'yaml-mode-hook #'ansible-doc-mode)
  :diminish ansible-doc-mode)

(use-package dash
  :config (dash-enable-font-lock))

(use-package dash-at-point
  :bind (("C-c h d" . dash-at-point)
     ("C-c h D" . dash-at-point-with-docset)))

(bind-key "C-c h b" #'describe-personal-keybindings)

(use-package yasnippet
  :diminish yas-minor-mode
  :mode ("\\.yasnippet$" . snippet-mode)
  :config (progn (yas-global-mode t)
     (setq-default yas-prompt-functions '(yas-ido-prompt))))

(diminish 'auto-fill-function)
(diminish 'abbrev-mode)


;;; Emacs Default Configuration


(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'before-save-hook #'whitespace-cleanup)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'set-goal-column 'disabled nil)
(set-cursor-color "orange")



(use-package key-chord
  :config
  (progn
    (setq key-chord-two-keys-delay 0.05)
    (key-chord-define-global "x1" 'delete-other-windows)
    (key-chord-define-global "xk" 'ace-window)
    (key-chord-define-global "0o" 'delete-window)
    (key-chord-define-global "xn" 'helm-mini)
    (key-chord-define-global "xb" 'projectile-switch-to-buffer)
    (key-chord-define-global "jk" 'magit-status)
    (key-chord-define-global "xm" 'helm-M-x)
    (key-chord-define-global "1q" "!")
    (key-chord-define-global "2w" "@")
    (key-chord-define-global "3e" "#")
    (key-chord-define-global "4r" "$")
    (key-chord-define-global "5t" "%")
    (key-chord-define-global "6y" "^")
    (key-chord-define-global "7y" "&")
    (key-chord-define-global "8u" "*")
    (key-chord-define-global "9i" "(")
    (key-chord-define-global "-p" "_")
    (key-chord-mode +1)))

(use-package quickrun
  :init (require #'quickrun))

(use-package goto-chg
  :bind ("C-M-." . goto-last-change))

(use-package pandoc-mode
  :config (progn (add-hook 'markdown-mode-hook #'pandoc-mode)
     (add-hook 'org-mode-hook #'pandoc-mode)
     (add-hook 'pandoc-mode-hook #'pandoc-load-default-settings))
  :diminish pandoc-mode)

(use-package swoop
  :bind (("C-o" . swoop)
     ("C-M-o" . swoop-multi)
     ("M-o" . swoop-pcre-regexp)
     ("C-S-o" . swoop-back-to-last-position))
  :config (setq swoop-font-size-change: nil))

(use-package fullframe
  :config
  (progn
    (fullframe magit-status magit-mode-quit-window)
    (fullframe monky-status monky-quit-window)
    (fullframe ibuffer ibuffer-quit)))

(use-package skeletor
  :config (setq skeletor-scala-use-ensime t))

(use-package worf)

(use-package popwin
  :init (require #'popwin)
  :config (popwin-mode t))

(use-package auto-install
  :init (require #'auto-install)
  :config
  (progn
    (setq auto-install-directory "~/.emacs.d/auto-install/")
    (add-to-list 'load-path (expand-file-name "~/elisp/auto-install"))))

(use-package sublimity
  :init (require #'sublimity)
  :config
  (progn
    (sublimity-mode t)
    (setq sublimity-map-fraction 0.3)
    (setq sublimity-map-text-scale -7)
    (setq sublimity-map-size 20)))

(use-package neotree
  :init (require #'neotree)
  :bind ([f8] . neotree-toggle))

(use-package helm-swoop
  :init (require #'helm-swoop)
  :bind (("M-i" . helm-swoop)
     ("M-I" . helm-swoop-back-to-last-point)
     ("C-c M-i" . helm-multi-swoop)
     ("C-c M-i" . helm-multi-swoop-all))
  :config
  (progn
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
    (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
    (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
    (setq helm-multi-swoop-edit-save t)
    (setq helm-swoop-split-with-multiple-windows nil)
    (setq helm-swoop-split-direction 'split-window-vertically)))

(use-package emacs-eclim
  :init
  (progn
    (require #'eclim)
    (require #'eclimd)
    (require #'company-emacs-eclim)
    (company-emacs-eclim-setup))
  :config
  (progn
    (setq help-at-pt-display-when-idle t)
    (setq eclimd-executable "c:/Program Files/eclipse/eclimd.bat")
    (setq help-at-pt-timer-delay 0.1)
    (setq eclimd-default-workspace "c:/Users/Sarvex/Workspace")
    (global-eclim-mode)
    (dolist (hook '(java-mode-hook scala-mode-hook groovy-mode-hook android-mode-hook))
      (addhook hook (lambda () (eclipse-mode t))))
    (help-at-pt-set-timer)))

(use-package auto-package-update
  :init (require #'auto-package-update)
  :config (auto-package-update-maybe))

(use-package android-mode)

(use-package auto-dim-other-buffers
  :diminish auto-diminish-other-buffers-mode
  :config (auto-dim-other-buffers-mode t))

(use-package gradle-mode
  :init (require #'gradle-mode)
  :config (gradle-mode t)
  :diminish gradle-mode)

(use-package groovy-mode
  :mode "\.groovy$"
  :interpreter "groovy"
  :init (require #'groovy-mode)
  :config
  (add-hook
   'groovy-mode-hook
   (lambda () (groovy-electric-mode))))

(use-package grunt
)

(use-package guru-mode
  :init (require #'guru-mode)
  :config
  (progn
    (guru-global-mode t)
    (setq guru-warn-only t))
  :diminish guru-mode)

(use-package smart-compile
  :bind ("C-c C-l" . smart-compile)
  :init (require #'smart-compile)
  :config
  (progn
    (add-to-list
     'smart-compile-alist
     '(append ("\\.rs\\'" . "cargo run")
      ("\\.toml\\'" . "cargo run")))))

(use-package docker)

(use-package vagrant)
