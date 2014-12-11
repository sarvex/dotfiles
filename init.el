(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(setq package-list '(abl-mode ac-anaconda  anaconda-mode ac-c-headers ac-cake2  ac-capf ac-cider ac-dabbrev ac-dcd ac-emmet ac-etags ac-geiser ac-haskell-process ac-helm ac-inf-ruby ac-ispell ac-js2 ac-math  ac-octave ac-slime auto-complete ac-sly ace-isearch helm-swoop ace-jump-buffer ace-jump-zap ace-link ace-window ace-jump-mode all-ext anaconda-mode json-rpc android-mode angry-police-captain angular-snippets ant anything-exuberant-ctags anything-git-files anything-git-grep anything-milkode anything-project anything-prosjekt anything-replace-string anything-sage apache-mode arduino-mode auto-compile auto-complete-c-headers auto-complete-chunk auto-complete-clang auto-complete-clang-async auto-complete-exuberant-ctags auto-complete-nxml auto-complete-sage auto-dictionary auto-dim-other-buffers auto-indent-mode auto-install auto-save-buffers-enhanced auto-shell-command auto-yasnippet autodisass-java-bytecode autodisass-llvm-bitcode autofit-frame fit-frame autopair awk-it babel blank-mode bongo boxquote browse-kill-ring+ browse-url-dwim buffer-move buffer-stack buffer-utils bufshow bundler buster-snippets butler button-lock cake-inflector calfw calfw-gcal caml cask shut-up cdlatex cerbere cfengine-code-style checkbox chef-mode chess chicken-scheme chm-view cider-decompile javap-mode cider-profile cider cider-spy circe  clojure-mode-extra-font-locking clojure-mode clojure-snippets closure-lint-mode cmake-mode cobra-mode coffee-fof coffee-mode csv-mode cuda-mode cypher-mode cython-mode d-mode dart-mode datomic-snippets dired+ dired-details+ dired-details dired-dups dired-efap dired-filter dired-imenu dired-k dired-narrow dired-open  dired-rainbow dired-hacks-utils dired-ranger dired-hacks-utils dired-single dired-sort dired-sort-menu+ dired-subtree dired-toggle dired-toggle-sudo direx-grep direx dirtree-prosjekt dirtree tree-mode django-mode django-snippets edts erlang eproject auto-highlight-symbol el-autoyas el-get el-mock el-el-sprunge el-spy el-swank-fuzzy el-x el2markdown eldoc-eval electric-case electric-spacing elfeed-org elfeed elixir-mix elixir-mode elixir-yasnippets elpy idomenu find-file-in-project company eproject helm erlang evil-args evil-escape key-chord evil-exchange evil-god-state god-mode evil-indent-textobject evil-jumper evil-leader evil-lisp-state evil-matchit evil-nerd-commenter evil-numbers evil-org evil-paredit evil-search-highlight-persist evil-space evil-surround evil-tabs elscreen evil-terminal-cursor-changer evil-visualstar exec-path-from-shell fit-frame flex-isearch flx-ido flx-isearch flx flycheck-cask flycheck-color-mode-line flycheck-d-unittest flycheck-dedukti flycheck-dmd-dub flycheck-google-cpplint flycheck flycheck-haskell flycheck-hdevtools flycheck-ledger flycheck-mercury flycheck-pos-tip flycheck-pyflakes flycheck-rust flycheck-tip flylisp flymake-cppcheck flymake-easy flymake-css flymake-cursor flymake-elixir flymake-gjshint flymake-google-cpplint flymake-haml flymake-haskell-multi flymake-hlint flymake-jshint flymake-jslint flymake-json flymake-lua flymake-perlcritic flymake-php flymake-phpcs flymake-puppet flymake-python-pyflakes flymake-ruby flymake-rust flymake-sass flymake-shell flymake-yaml geiser go-snippets helm-R helm-ack helm-ad helm-ag helm-anything helm-aws helm-backup helm-bibtex ebib helm-bibtexkey helm-bind-key helm-bm helm-c-yasnippet helm-chrome helm-cmd-t helm-company helm-css-scss helm-dash  helm-descbinds helm-dictionary helm-dired-recent-dirs helm-emmet helm-filesets filesets+ helm-flycheck helm-flymake helm-ghc helm-ghq helm-gist helm-git helm-git-files helm-git-grep helm-github-stars helm-gitlab helm-go-package helm-google helm-growthforecast helm-gtags helm-hatena-bookmark helm-hayoo haskell-mode helm-helm-commands helm-hoogle helm-idris helm-itunes helm-j-cheatsheet helm-ls-git helm-ls-hg helm-make helm-migemo helm-mode-manager helm-open-github helm-orgcard helm-package helm-perldoc helm-proc helm-project-persist helm-projectile helm-projectile-all helm-prosjekt helm-pydoc helm-rails helm-rb helm-ag-r helm-recoll helm-robe helm-rubygems-local helm-rubygems-org helm-sage helm-sheet helm-spaces helm-spotify helm-swoop highlight highlight-indentation ht ido-at-point ido-complete-space-or-hyphen ido-gnus ido-hacks ido-load-library ido-select-window ido-sort-mtime imenu-anywhere java-snippets jedi-direx json-rpc key-chord kill-ring-ido lfe-mode load-theme-buffer-local lui magit-annex magit-filenotify magit-find-file magit-gerrit magit-gh-pulls logito magit-gitflow magit-log-edit magit-push-remote magit-stgit magit-svn  magit-topgit magit-tramp maker-mode malabar-mode fringe-helper mallard-snippets mallard-mode manage-minor-mode markdown-mode+ markdown-mode math-symbol-lists matlab-mode maude-mode migemo milkode minor-mode-hack mocha-snippets moe-theme navi-mode newlisp-mode nhexl-mode org-ac log4e org-mac-iCal org-magit org-repo-todo orglue org-mac-link epic htmlize org package-build  parent-mode peek-mode elnode fakir persp-mode php+-mode php-auto-yasnippets php-mode  php-refactor-mode picolisp-mode pig-mode pig-snippets plantuml-mode auto-complete plim-mode pony-mode popup popwin pov-mode powerline-evil powerline evil goto-chg pretty-mode project-persist projectile py-autopep8 py-gnitset py-import-check py-isort py-test pydoc-info pylint python-cell python-django python-environment python-info python-magic r-autoyas ess racket-mode sage-shell-mode seti-theme shut-up skewer-mode js2-mode simple-httpd slime-annot slime-company slime-ritz slime-volleyball sly smart-compile smart-cursor-color smart-forward expand-region smart-indent-rigidly smart-mode-line-powerline-theme smart-mode-line rich-minority powerline smart-newline smart-operator smart-shift smart-tab smart-tabs-mode smart-window smartparens smartrep smartscan stan-snippets stan-mode starter-kit-bindings starter-kit starter-kit-eshell starter-kit-js starter-kit-lisp elisp-slime-nav starter-kit-ruby inf-ruby string-utils list-utils textmate-to-yas tree-mode undo-tree web web-beautify web-server weechat yasnippet))

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
 '(default-frame-alist (quote ((vertical-scroll-bars) (width . 120) (height . 40))))
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

(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; Show the current function name in the header line
(which-function-mode)
(setq which-func-unknown "n/a")
(setq-default header-line-format '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info (assq-delete-all 'which-func-mode mode-line-misc-info))


