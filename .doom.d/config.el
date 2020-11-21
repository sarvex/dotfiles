(map! :leader
      :desc "List bookmarks"
      "b L" #'list-bookmarks
      :leader
      :desc "Save current bookmarks to bookmark file"
      "b w" #'bookmark-save)

(setq centaur-tabs-set-bar 'over
      centaur-tabs-set-icons t
      centaur-tabs-gray-out-icons 'buffer
      centaur-tabs-height 24
      centaur-tabs-set-modified-marker t
      centaur-tabs-style "bar"
      centaur-tabs-modified-marker "•")
(map! :leader
      :desc "Toggle tabs on/off"
      "t c" #'centaur-tabs-local-mode)
(evil-define-key 'normal centaur-tabs-mode-map (kbd "g <right>") 'centaur-tabs-forward        ; default Doom binding is 'g t'
                                               (kbd "g <left>")  'centaur-tabs-backward       ; default Doom binding is 'g T'
                                               (kbd "g <down>")  'centaur-tabs-forward-group
                                               (kbd "g <up>")    'centaur-tabs-backward-group)

(map! :leader
      :desc "Dired"
      "d d" #'dired
      :leader
      :desc "Dired jump to current"
      "d j" #'dired-jump
      (:after dired
        (:map dired-mode-map
         :leader
         :desc "Peep-dired image previews"
         "d p" #'peep-dired
         :leader
         :desc "Dired view file"
         "d v" #'dired-view-file)))
;; Make 'h' and 'l' go back and forward in dired. Much faster to navigate the directory structure!
(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
;; If peep-dired is enabled, you will get image previews as you go up/down with 'j' and 'k'
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(setq doom-theme 'doom-one)
(map! :leader
      :desc "Load new theme"
      "h t" #'counsel-load-theme)

(custom-set-variables
 '(elfeed-feeds
   (quote
    (("https://www.reddit.com/r/linux.rss" reddit linux)
     ("https://www.gamingonlinux.com/article_rss.php" gaming linux)
     ("https://hackaday.com/blog/feed/" hackaday linux)
     ("https://opensource.com/feed" opensource linux)
     ("https://linux.softpedia.com/backend.xml" softpedia linux)
     ("https://itsfoss.com/feed/" itsfoss linux)
     ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
     ("https://www.phoronix.com/rss.php" phoronix linux)
     ("http://feeds.feedburner.com/d0od" omgubuntu linux)
     ("https://www.computerworld.com/index.rss" computerworld linux)
     ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
     ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
     ("https://betanews.com/feed" betanews linux)
     ("http://lxer.com/module/newswire/headlines.rss" lxer linux)
     ("https://distrowatch.com/news/dwd.xml" distrowatch linux)))))

(require 'emms-setup)
(require 'emms-info)
(require 'emms-cue)
(require 'emms-mode-line)
(require 'emms-playing-time)
(emms-all)
(emms-default-players)
(emms-mode-line 1)
(emms-playing-time 1)
(setq emms-source-file-default-directory "~/Music/Non-Classical/70s-80s/"
      emms-playlist-buffer-name "*Music*"
      emms-info-asynchronously t
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
(map! :leader
      :desc "Go to emms playlist"
      "a a" #'emms-playlist-mode-go
      :leader
      :desc "Emms pause track"
      "a x" #'emms-pause
      :leader
      :desc "Emms stop track"
      "a s" #'emms-stop
      :leader
      :desc "Emms play previous track"
      "a p" #'emms-previous
      :leader
      :desc "Emms play next track"
      "a n" #'emms-next)

(map! :leader
      :desc "Evaluate elisp in buffer"
      "e b" #'eval-buffer
      :leader
      :desc "Evaluate defun"
      "e d" #'eval-defun
      :leader
      :desc "Evaluate elisp expression"
      "e e" #'eval-expression
      :leader
      :desc "Evaluate last sexpression"
      "e l" #'eval-last-sexp
      :leader
      :desc "Evaluate elisp in region"
      "e r" #'eval-region)

(setq browse-url-browser-function 'eww-browse-url)
(map! :leader
      :desc "Eww web browser"
      "e w" #'eww
      :leader
      :desc "Eww reload page"
      "e R" #'eww-reload
      :leader
      :desc "Search web for text between BEG/END"
      "s w" #'eww-search-words)

(require 'exwm)
(require 'exwm-config)
(exwm-config-default)
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(require 'exwm-randr)
(exwm-randr-enable)
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
              "xrandr" nil "xrandr --output DisplayPort-0 --mode 1920x1080 --pos 0x0 --rotate normal
                                   --output DisplayPort-1 --primary --mode 1920x1080 --pos 1920x0 --rotate normal
                                   --output HDMI-A-0 --mode 1920x1080 --pos 3840x0 --rotate normal")))
(setq exwm-workspace-number 10
      exwm-randr-workspace-output-plist '(0 "DisplayPort-0"
                                          1 "DisplayPort-1"
                                          2 "HDMI-A-0")
      exwm-input-prefix-keys '(?\M-x
                               ?\M-:)
      exwm-input-simulation-keys '(([?\s-F] . [?\C-f])
                                   )
      exwm-input-global-keys '(([?\s-&] . (lambda (command)
                                          (interactive (list (read-shell-command "$ ")))
                                          (start-process-shell-command command nil command)))
                               ;; splits
                               ([?\s-v] . evil-window-vsplit)
                               ([?\s-z] . evil-window-split)
                               ;; managing workspaces
                               ([?\s-w] . exwm-workspace-switch)
                               ([?\s-W] . exwm-workspace-swap)
                               ([?\s-\C-w] . exwm-workspace-move)
                               ;; essential programs
                               ([?\s-d] . dired)
                               ([s-return] . eshell)
                               ([s-S-return] . dmenu)
                               ;; killing buffers and windows
                               ([?\s-b] . ibuffer)
                               ([?\s-B] . kill-current-buffer)
                               ([?\s-C] . +workspace/close-window-or-workspace)
                               ;; change window focus with super+h,j,k,l
                               ([?\s-h] . evil-window-left)
                               ([?\s-j] . evil-window-next)
                               ([?\s-k] . evil-window-prev)
                               ([?\s-l] . evil-window-right)
                               ;; move windows around using SUPER+SHIFT+h,j,k,l
                               ([?\s-H] . +evil/window-move-left)
                               ([?\s-J] . +evil/window-move-down)
                               ([?\s-K] . +evil/window-move-up)
                               ([?\s-L] . +evil/window-move-right)
                               ;; move window to far left or far right with SUPER+CTRL+h,l
                               ([?\s-\C-h] . side-left-window)
                               ([?\s-\C-j] . side-bottom-window)
                               ([?\s-\C-l] . side-right-window)
                               ([?\s-\C-d] . side-window-delete-all)
                               ([?\s-\C-r] . resize-window)
                               ;; switch workspace with SUPER+{0-9}
                               ([?\s-0] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
                               ([?\s-1] . (lambda () (interactive) (exwm-workspace-switch-create 1)))
                               ([?\s-2] . (lambda () (interactive) (exwm-workspace-switch-create 2)))
                               ([?\s-3] . (lambda () (interactive) (exwm-workspace-switch-create 3)))
                               ([?\s-4] . (lambda () (interactive) (exwm-workspace-switch-create 4)))
                               ([?\s-5] . (lambda () (interactive) (exwm-workspace-switch-create 5)))
                               ([?\s-6] . (lambda () (interactive) (exwm-workspace-switch-create 6)))
                               ([?\s-7] . (lambda () (interactive) (exwm-workspace-switch-create 7)))
                               ([?\s-8] . (lambda () (interactive) (exwm-workspace-switch-create 8)))
                               ([?\s-9] . (lambda () (interactive) (exwm-workspace-switch-create 9)))
                               ;; move window workspace with SUPER+SHIFT+{0-9}
                               ([?\s-\)] . (lambda () (interactive) (exwm-workspace-move-window 0)))
                               ([?\s-!] . (lambda () (interactive) (exwm-workspace-move-window 1)))
                               ([?\s-@] . (lambda () (interactive) (exwm-workspace-move-window 2)))
                               ([?\s-#] . (lambda () (interactive) (exwm-workspace-move-window 3)))
                               ([?\s-$] . (lambda () (interactive) (exwm-workspace-move-window 4)))
                               ([?\s-%] . (lambda () (interactive) (exwm-workspace-move-window 5)))
                               ([?\s-^] . (lambda () (interactive) (exwm-workspace-move-window 6)))
                               ([?\s-&] . (lambda () (interactive) (exwm-workspace-move-window 7)))
                               ([?\s-*] . (lambda () (interactive) (exwm-workspace-move-window 8)))
                               ([?\s-\(] . (lambda () (interactive) (exwm-workspace-move-window 9)))
                               ;; setting some toggle commands
                               ([?\s-f] . exwm-floating-toggle-floating)
                               ([?\s-m] . exwm-layout-toggle-mode-line)
                               ([f11] . exwm-layout-toggle-fullscreen)))

(defun dt/exwm-start-picom ()
  (interactive)
  (start-process-shell-command "picom" nil "picom"))

(defun dt/exwm-start-nm-applet ()
  (interactive)
  (start-process-shell-command "nm-applet" nil "nm-applet"))

(defun dt/exwm-start-volume-icon ()
  (interactive)
  (start-process-shell-command "volume-icon" nil "volume-icon"))

(after! exwm-config
  (dt/exwm-start-picom)
  (dt/exwm-start-nm-applet)
  (dt/exwm-start-volume-icon))

(setq doom-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 15)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
      doom-big-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(require 'ivy-posframe)
(setq ivy-posframe-display-functions-alist
      '((swiper                     . ivy-posframe-display-at-point)
        (complete-symbol            . ivy-posframe-display-at-point)
        (counsel-M-x                . ivy-display-function-fallback)
        (counsel-esh-history        . ivy-posframe-display-at-window-center)
        (counsel-describe-function  . ivy-display-function-fallback)
        (counsel-describe-variable  . ivy-display-function-fallback)
        (counsel-find-file          . ivy-display-function-fallback)
        (counsel-recentf            . ivy-display-function-fallback)
        (counsel-register           . ivy-posframe-display-at-frame-bottom-window-center)
        (dmenu                      . ivy-posframe-display-at-frame-top-center)
        (nil                        . ivy-posframe-display))
      ivy-posframe-height-alist
      '((swiper . 20)
        (dmenu . 20)
        (t . 10)))
(ivy-posframe-mode 1) ; 1 enables posframe-mode, 0 disables it.

(map! :leader
      :desc "Ivy push view"
      "v p" #'ivy-push-view
      :leader
      :desc "Ivy switch view"
      "v s" #'ivy-switch-view)

(setq display-line-numbers-type t)
(map! :leader
      :desc "Toggle truncate lines"
      "t t" #'toggle-truncate-lines)

(require 'ox-groff)

(after! mastodon
  (setq mastodon-instance-url "https://mastodon.technology/"))

(setq md4rd-subs-active '(archlinux commandline DistroTube DoomEmacs emacs freesoftware lbry linux linux4noobs linuxmasterrace linnuxquestions orgmode qutebrowser suckless Ubuntu unixporn UsabilityPorn vim xmonad))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'smtpmail)
(setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a"
      mu4e-update-interval  300
      user-mail-address "derek@distrotube.com"
      user-full-name  "Derek Taylor"
      mu4e-compose-signature
       (concat
         "Derek Taylor\n"
         "http://www.youtube.com/DistroTube\n")
      message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.1and1.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.1and1.com" 587 "derek@distrotube.com" nil))
      smtpmail-default-smtp-server "smtp.1and1.com"
      smtpmail-smtp-server "smtp.1and1.com"
      smtpmail-smtp-service 587
      mu4e-sent-folder "/Sent"
      mu4e-drafts-folder "/Drafts"
      mu4e-trash-folder "/Trash"
      mu4e-refile-folder "/All Mail"
      mu4e-maildir-shortcuts
      '(("/derek-distrotube/Inbox"    . ?i)
        ("/derek-distrotube/Sent"     . ?s)
        ("/derek-distrotube/All Mail" . ?a)
        ("/derek-distrotube/Trash"    . ?t)))

(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))
(after! doom-themes
  (setq doom-neotree-enable-variable-pitch t))
(map! :leader
      :desc "Toggle neotree file viewer"
      "t n" #'toggle-neotree)

(map! :leader
      :desc "Edit agenda file"
      "- a" #'(lambda () (interactive) (find-file "~/Org/agenda.org"))
      :leader
      :desc "Edit doom config.org"
      "- c" #'(lambda () (interactive) (find-file "~/.doom.d/config.org"))
      :leader
      :desc "Edit eshell aliases"
      "- e" #'(lambda () (interactive) (find-file "~/.doom.d/aliases"))
      :leader
      :desc "Edit doom init.el"
      "- i" #'(lambda () (interactive) (find-file "~/.doom.d/init.el"))
      :leader
      :desc "Edit doom packages.el"
      "- p" #'(lambda () (interactive) (find-file "~/.doom.d/packages.el")))

(after! org
  (require 'org-bullets)  ; Nicer bullets in org-mode
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-directory "~/Org/"
        org-agenda-files '("~/Org/agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-log-done 'time
        org-journal-dir "~/Org/journal/"
        org-journal-date-format "%B %d, %Y (%A)"
        org-journal-file-format "%Y-%m-%d.org"
        org-hide-emphasis-markers t
        ;; ex. of org-link-abbrev-alist in action
        ;; [[arch-wiki:Name_of_Page][Description]]
        org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
          '(("google" . "http://www.google.com/search?q=")
            ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
            ("ddg" . "https://duckduckgo.com/?q=")
            ("wiki" . "https://en.wikipedia.org/wiki/"))
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "TODO(t)"           ; A task that is ready to be tackled
             "BLOG(b)"           ; Blog writing assignments
             "GYM(g)"            ; Things to accomplish at the gym
             "PROJ(p)"           ; A project that contains other tasks
             "VIDEO(v)"          ; Video assignments
             "WAIT(w)"           ; Something is holding up this task
             "|"                 ; The pipe necessary to separate "active" states and "inactive" states
             "DONE(d)"           ; Task has been completed
             "CANCELLED(c)" )))) ; Task has been cancelled

(defun dt/org-babel-tangle-async (file)
  "Invoke `org-babel-tangle-file' asynchronously."
  (message "Tangling %s..." (buffer-file-name))
  (async-start
   (let ((args (list file)))
  `(lambda ()
        (require 'org)
        ;;(load "~/.emacs.d/init.el")
        (let ((start-time (current-time)))
          (apply #'org-babel-tangle-file ',args)
          (format "%.2f" (float-time (time-since start-time))))))
   (let ((message-string (format "Tangling %S completed after " file)))
     `(lambda (tangle-time)
        (message (concat ,message-string
                         (format "%s seconds" tangle-time)))))))

(defun dt/org-babel-tangle-current-buffer-async ()
  "Tangle current buffer asynchronously."
  (dt/org-babel-tangle-async (buffer-file-name)))

(map! :leader
      :desc "Copy to register"
      "r c" #'copy-to-register
      :leader
      :desc "Frameset to register"
      "r f" #'frameset-to-register
      :leader
      :desc "Insert contents of register"
      "r i" #'insert-register
      :leader
      :desc "Jump to register"
      "r j" #'jump-to-register
      :leader
      :desc "List registers"
      "r l" #'list-registers
      :leader
      :desc "Number to register"
      "r n" #'number-to-register
      :leader
      :desc "Interactively choose a register"
      "r r" #'counsel-register
      :leader
      :desc "View a register"
      "r v" #'view-register
      :leader
      :desc "Window configuration to register"
      "r w" #'window-configuration-to-register
      :leader
      :desc "Increment register"
      "r +" #'increment-register
      :leader
      :desc "Point to register"
      "r SPC" #'point-to-register)

(map! :leader
      :desc "Ssh into distrotube.com"
      "\\ d" #'(lambda () (interactive) (find-file "/scp:derek@distrotube.com"))
      :leader
      :desc "Ssh into my nextcloud"
      "\\ n" #'(lambda () (interactive) (find-file "/scp:derek@distrotube.net")))

(setq shell-file-name "/bin/fish"
      eshell-aliases-file "~/.doom.d/aliases"
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "zsh")
      vterm-max-scrollback 5000)
(map! :leader
      :desc "Counsel eshell history"
      "e h" #'counsel-esh-history)

(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)
(map! :leader
      :desc "Clone indirect buffer other window"
      "b c" #'clone-indirect-buffer-other-window)

(require 'sublimity-scroll)
(require 'sublimity-map)
(require 'sublimity-attractive)
(sublimity-mode 0)

(map! :leader
      :desc "Winner redo"
      "w <right>" #'winner-redo
      :leader
      :desc "Winner undo"
      "w <left>" #'winner-undo)
