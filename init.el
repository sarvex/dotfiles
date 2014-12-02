(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages") t)
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(setq package-list '(abl-mode ac-anaconda dash anaconda-mode f dash s dash json-rpc auto-complete popup ac-c-headers ac-cake auto-complete popup cake anything historyf cake-inflector s ac-cake2 auto-complete popup cake2 anything historyf cake-inflector s ht f dash s s dash ac-capf auto-complete popup ac-cider auto-complete popup cider queue pkg-info epl dash clojure-mode ac-dabbrev ac-dcd flycheck-dmd-dub flycheck pkg-info epl dash auto-complete popup ac-emmet auto-complete popup emmet-mode ac-etags auto-complete popup ac-geiser auto-complete popup geiser ac-haskell-process haskell-mode auto-complete popup ac-helm popup auto-complete popup helm async ac-inf-ruby auto-complete popup inf-ruby ac-ispell auto-complete popup ac-js2 skewer-mode js2-mode simple-httpd js2-mode ac-math math-symbol-lists auto-complete popup ac-octave auto-complete popup ac-slime slime auto-complete popup ac-sly auto-complete popup sly ace-isearch helm-swoop helm async ace-jump-mode ace-jump-buffer dash ace-jump-mode ace-jump-zap ace-jump-mode ace-link noflet ace-jump-mode ace-window ace-jump-mode afternoon-theme ag s dash alchemist alect-themes all-ext all ample-theme ample-zen-theme anaconda-mode f dash s dash json-rpc android-mode angry-police-captain angular-snippets dash s ant anti-zenburn-theme anything-exuberant-ctags anything anything-git-files anything anything-git-grep anything anything-milkode anything milkode anything-project anything imakado anything-prosjekt anything prosjekt dash anything-replace-string anything anything-sage sage-shell-mode deferred anything apache-mode arduino-mode ascii ascii-art-to-unicode auto-compile packed auto-complete-c-headers auto-complete popup auto-complete-chunk auto-complete popup auto-complete-clang auto-complete popup auto-complete-clang-async auto-complete-exuberant-ctags auto-complete popup auto-complete-nxml auto-complete popup auto-complete-sage sage-shell-mode deferred auto-complete popup auto-dictionary auto-dim-other-buffers auto-indent-mode auto-install auto-save-buffers-enhanced auto-shell-command popwin deferred auto-yasnippet yasnippet autodisass-java-bytecode autodisass-llvm-bitcode autofit-frame fit-frame autopair awk-it babel badger-theme bbdb2erc bbdb birds-of-paradise-plus-theme blank-mode bliss-theme bongo boron-theme boxquote browse-kill-ring+ browse-kill-ring browse-kill-ring browse-url-dwim string-utils list-utils bubbleberry-theme buffer-move buffer-stack buffer-utils bufshow bundler inf-ruby buster-snippets busybee-theme butler deferred button-lock cake anything historyf cake-inflector s cake2 anything historyf cake-inflector s ht f dash s s dash calfw calfw-gcal calmer-forest-theme caml cask package-build shut-up epl f dash s dash s caskxy yaxception log4e cdlatex cerbere pkg-info epl f dash s s cfengine-code-style chatwork checkbox chef-mode cherry-blossom-theme chess chicken-scheme chm-view cider-decompile javap-mode cider queue pkg-info epl dash clojure-mode cider-profile cider queue pkg-info epl dash clojure-mode cider-spy dash cider queue pkg-info epl dash clojure-mode circe lcs lui tracking shorten clojure-mode-extra-font-locking clojure-mode clojure-snippets yasnippet clojure-test-mode cider queue pkg-info epl dash clojure-mode clojure-mode closure-lint-mode clues-theme cmake-mode cobra-mode coffee-fof coffee-mode coffee-mode colemak-evil evil goto-chg undo-tree colonoscopy-theme color-theme-approximate color-theme-buffer-local color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow color-theme-solarized color-theme colorsarenice-theme csv-mode cuda-mode cyberpunk-theme cypher-mode cython-mode d-mode dakrone-theme darcula-theme dark-krystal-theme darkburn-theme darkmine-theme dart-mode datomic-snippets yasnippet dash s dired+ dired-avfs dired-hacks-utils dash dash dired-details+ dired-details dired-details dired-dups dired-efap dired-filter dired-hacks-utils dash dash dired-imenu dired-k dired-narrow dired-hacks-utils dash dash dired-open dired-hacks-utils dash dash dired-rainbow dired-hacks-utils dash dash dired-ranger dired-hacks-utils dash dash dired-single dired-sort dired-sort-menu+ dired-sort-menu dired-sort-menu dired-subtree dired-hacks-utils dash dash dired-toggle dired-toggle-sudo direx-grep direx dirtree-prosjekt dirtree windata tree-mode prosjekt dash display-theme distinguished-theme django-mode django-snippets yasnippet django-theme docbook-snippets yasnippet edts popup f dash s erlang eproject helm async dash auto-highlight-symbol auto-complete popup el-autoyas el-get el-mock el-spec el-spice thingatpt+ el-sprunge htmlize web-server el-spy el-swank-fuzzy el-x el2markdown eldoc-eval electric-case electric-spacing elfeed-org org elfeed elixir-mix elixir-mode elixir-yasnippets yasnippet elpy yasnippet pyvenv idomenu highlight-indentation find-file-in-project company eproject helm async erlang espresso-theme evil-args evil goto-chg undo-tree evil-escape key-chord evil goto-chg undo-tree evil-exchange evil goto-chg undo-tree evil-god-state god-mode evil goto-chg undo-tree evil-indent-textobject evil goto-chg undo-tree evil-jumper evil goto-chg undo-tree evil-leader evil goto-chg undo-tree evil-lisp-state smartparens dash evil goto-chg undo-tree evil-matchit evil-nerd-commenter evil-numbers evil-org org evil goto-chg undo-tree evil-paredit paredit evil goto-chg undo-tree evil-search-highlight-persist highlight evil-space evil goto-chg undo-tree evil-surround evil-tabs elscreen evil goto-chg undo-tree evil-terminal-cursor-changer evil goto-chg undo-tree evil-visualstar evil goto-chg undo-tree exec-path-from-shell firebelly-theme firecode-theme fit-frame flatland-black-theme flatland-theme flatui-theme flex-isearch flx-ido flx flx-isearch flx flycheck-cask dash flycheck pkg-info epl dash flycheck-color-mode-line dash flycheck pkg-info epl dash flycheck-d-unittest dash flycheck pkg-info epl dash flycheck-dedukti dedukti-mode flycheck pkg-info epl dash flycheck-dmd-dub flycheck pkg-info epl dash flycheck-google-cpplint flycheck pkg-info epl dash flycheck-haskell dash haskell-mode flycheck pkg-info epl dash flycheck-hdevtools dash flycheck pkg-info epl dash flycheck-ledger flycheck pkg-info epl dash flycheck-mercury dash s flycheck pkg-info epl dash flycheck-pos-tip popup flycheck pkg-info epl dash flycheck-pyflakes flycheck pkg-info epl dash flycheck-rust dash flycheck pkg-info epl dash flycheck-tip s popup dash flycheck pkg-info epl dash flylisp flymake-cppcheck flymake-easy flymake-css flymake-easy flymake-cursor flymake-elixir flymake-gjshint flymake-google-cpplint flymake-easy flymake-haml flymake-easy flymake-haskell-multi flymake-easy flymake-hlint flymake-easy flymake-jshint flymake-easy flymake-jslint flymake-easy flymake-json flymake-easy flymake-lua flymake-perlcritic flymake-php flymake-easy flymake-phpcs flymake-easy flymake-puppet flymake-easy flymake-python-pyflakes flymake-easy flymake-ruby flymake-easy flymake-rust flymake-easy flymake-sass flymake-easy flymake-shell flymake-easy flymake-yaml flymake-easy gandalf-theme geiser go-snippets yasnippet god-mode gotham-theme grandshell-theme gruber-darker-theme gruvbox-theme hc-zenburn-theme helm-R ess helm async helm-ack helm async helm-ad helm async dash helm-ag helm async helm-anything anything helm async helm-aws helm-backup s helm async helm-bibtex f dash s dash s ebib dash helm async helm-bibtexkey helm async helm-bind-key helm async bind-key helm-bm s helm async bm helm-c-moccur color-moccur helm async helm-c-yasnippet yasnippet helm async helm-chrome helm async helm-cmd-t helm-company company helm async helm-css-scss helm async helm-dash helm async helm-descbinds helm async helm-dictionary helm async helm-dired-recent-dirs helm async helm-emmet emmet-mode helm async helm-filesets filesets+ helm async helm-flycheck helm async flycheck pkg-info epl dash dash helm-flymake helm async helm-ghc ghc helm async helm-ghq helm async helm-gist gist gh logito pcache helm async helm-git helm-git-files helm async helm-git-grep helm async helm-github-stars helm async helm-gitlab gitlab request pkg-info epl dash s helm async dash s helm-go-package deferred go-mode helm async helm-google google helm async helm-growthforecast helm async helm-gtags helm async helm-hatena-bookmark helm async helm-hayoo haskell-mode helm async helm-helm-commands helm async helm-hoogle helm async helm-idris idris-mode helm async helm-itunes helm async helm-j-cheatsheet helm async helm-ls-git helm async helm-ls-hg helm async helm-make helm async helm-migemo migemo helm async helm-mode-manager helm async helm-open-github gh logito pcache helm async helm-orgcard helm async helm-package helm async helm-perldoc deferred helm async helm-proc helm async helm-project-persist project-persist helm async helm-projectile projectile pkg-info epl dash f dash s s helm async helm-projectile-all s dash projectile pkg-info epl dash f dash s s helm async helm-prosjekt helm async prosjekt dash helm-pydoc helm async helm-rails inflections helm async helm-rb helm-ag-r helm async helm async helm-recoll helm async helm-robe helm async helm-rubygems-local helm async helm-rubygems-org helm async helm-sage sage-shell-mode deferred helm async helm-sheet helm async helm-spaces spaces helm async helm-spotify multi helm async helm-swoop helm async helm-themes helm async hemisu-theme heroku-theme highlight highlight-indentation hipster-theme historyf ht ido-at-point ido-complete-space-or-hyphen ido-gnus ido-hacks ido-load-library pcache persistent-soft list-utils pcache ido-select-window ido-sort-mtime ido-vertical-mode idomenu idris-mode imakado imenu-anywhere inflections inkpot-theme ir-black-theme java-snippets yasnippet javap-mode jedi-direx direx jedi python-environment deferred auto-complete popup epc ctable concurrent deferred json-rpc key-chord kill-ring-ido lavender-theme lcs leuven-theme lfe-mode light-soap-theme load-theme-buffer-local lui tracking shorten lush-theme magit-annex magit git-rebase-mode git-commit-mode magit-filenotify magit git-rebase-mode git-commit-mode magit-find-file dash magit git-rebase-mode git-commit-mode magit-gerrit magit git-rebase-mode git-commit-mode magit-gh-pulls s pcache magit git-rebase-mode git-commit-mode gh logito pcache magit-gitflow magit git-rebase-mode git-commit-mode magit-log-edit magit git-rebase-mode git-commit-mode magit-push-remote magit git-rebase-mode git-commit-mode magit-stgit magit git-rebase-mode git-commit-mode magit-svn magit git-rebase-mode git-commit-mode magit-topgit magit git-rebase-mode git-commit-mode magit-tramp magit git-rebase-mode git-commit-mode maker-mode dash s malabar-mode fringe-helper mallard-snippets mallard-mode yasnippet manage-minor-mode markdown-mode+ markdown-mode markdown-mode math-symbol-lists matlab-mode maude-mode mbo70s-theme mellow-theme migemo milkode minimal-theme minor-mode-hack mocha-snippets yasnippet moe-theme molokai-theme monochrome-theme monokai-theme multi mustang-theme mustard-theme naquadah-theme navi-mode newlisp-mode nhexl-mode niflheim-theme noctilux-theme nose obsidian-theme occidental-theme oldlace-theme org-ac yaxception log4e auto-complete-pcmp yaxception log4e auto-complete popup org-mac-iCal org-magit org magit git-rebase-mode git-commit-mode org-repo-todo organic-green-theme orglue org-mac-link epic htmlize org package-build packed parent-mode pastels-on-dark-theme peacock-theme peek-mode elnode kv db kv fakir kv dash noflet creole kv noflet s noflet dash web s dash persistent-soft list-utils pcache persp-mode phoenix-dark-mono-theme phoenix-dark-pink-theme php+-mode php-auto-yasnippets yasnippet php-mode php-mode php-refactor-mode picolisp-mode pig-mode pig-snippets yasnippet pkgbuild-mode planet-theme plantuml-mode auto-complete popup plim-mode pony-mode popup popwin pov-mode powerline-evil powerline evil goto-chg undo-tree pretty-mode professional-theme project-persist projectile pkg-info epl dash f dash s s prosjekt dash purple-haze-theme py-autopep8 py-gnitset py-import-check py-isort py-test f dash s dash pydoc-info pylint python-cell python-django python-environment deferred python-info python-magic pyvenv queue r-autoyas yasnippet ess racket-mode rcirc-alert rcirc-alertify alert gntp rcirc-color rcirc-controls rcirc-groups rcirc-notify react-snippets yasnippet request reverse-theme sage-shell-mode deferred seti-theme shut-up skewer-mode js2-mode simple-httpd slime-annot slime slime-company company slime slime-ritz slime-theme slime-volleyball sly smart-compile smart-cursor-color smart-forward expand-region smart-indent-rigidly smart-mode-line-powerline-theme smart-mode-line rich-minority dash powerline smart-newline smart-operator smart-shift smart-tab smart-tabs-mode smart-window smartparens dash smartrep smartscan smarty-mode smyx-theme soft-charcoal-theme soft-morning-theme soft-stone-theme solarized-theme dash soothe-theme spacegray-theme spaces sphinx-doc s stan-snippets stan-mode yasnippet starter-kit-bindings starter-kit magit git-rebase-mode git-commit-mode ido-ubiquitous smex find-file-in-project idle-highlight-mode paredit starter-kit-eshell starter-kit-js starter-kit magit git-rebase-mode git-commit-mode ido-ubiquitous smex find-file-in-project idle-highlight-mode paredit starter-kit-lisp elisp-slime-nav starter-kit magit git-rebase-mode git-commit-mode ido-ubiquitous smex find-file-in-project idle-highlight-mode paredit starter-kit-perl starter-kit-ruby starter-kit magit git-rebase-mode git-commit-mode ido-ubiquitous smex find-file-in-project idle-highlight-mode paredit inf-ruby stekene-theme string-utils list-utils subatomic-theme subatomic256-theme sublime-themes sunny-day-theme svg-mode-line-themes xmlgen tango-2-theme tango-plus-theme tangotango-theme textmate-to-yas theme-changer thingatpt+ tommyh-theme tox toxi-theme tree-mode tronesque-theme twilight-anti-bright-theme twilight-bright-theme twilight-theme ujelly-theme underwater-theme undo-tree virtualenv waher-theme warm-night-theme web s dash web-beautify web-server weechat tracking shorten s windata xmlgen yascroll yasnippet yaxception zen-and-art-theme zenburn-theme zonokai-theme))

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
 '(initial-frame-alist (quote ((vertical-scroll-bars) (width . 120) (height . 40))))
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


