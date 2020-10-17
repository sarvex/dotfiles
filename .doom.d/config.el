(setq doom-font (font-spec :family "SauceCodePro Nerd Font" :size 15)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
      doom-big-font (font-spec :family "SauceCodePro Nerd Font" :size 24))
(after! doom-themes
  (setq
   doom-themes-enable-bold t
   doom-themes-enable-italic t))

(setq doom-theme 'doom-one)
(map! :leader
      :desc "Load new theme"
      "h t" #'counsel-load-theme)

(setq display-line-numbers-type t)
(map! :leader
      :desc "Toggle truncate lines"
      "l t" #'toggle-truncate-lines)

(after! org
  (setq org-directory "~/Org/"
        org-agenda-files '("~/Org/agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-log-done 'time
        org-journal-dir "~/Org/journal/"
        org-journal-date-format "%B %d, %Y (%A)"
        org-journal-file-format "%Y-%m-%d.org"
        ;; ex. of org-link-abbrev-alist in action
        ;; [[arch-wiki:Name_of_Page][Description]]
        org-link-abbrev-alist
          '(("google" . "http://www.google.com/search?q=")
            ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
            ("ddg" . "https://duckduckgo.com/?q=")
            ("wiki" . "https://en.wikipedia.org/wiki/"))
        org-todo-keywords '((sequence "TODO(t)" "PROJ(p)" "VIDEO(v)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)" )))
  ;; Nicer bullets in org-mode
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq shell-file-name "/bin/fish")

(setq eshell-aliases-file "~/.doom.d/aliases")

(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)

(setq centaur-tabs-set-bar 'over
      centaur-tabs-set-icons t
      centaur-tabs-gray-out-icons 'buffer
      centaur-tabs-height 24
      centaur-tabs-set-modified-marker t
      centaur-tabs-style "bar"
      centaur-tabs-modified-marker "•")
(map! :leader
      :desc "Toggle tabs on/off"
      "t o" #'centaur-tabs-local-mode
      :leader
      :desc "Switch tab groups"
      "t s" #'centaur-tabs-counsel-switch-group
      :leader
      :desc "Toggle tab groups"
      "t t" #'centaur-tabs-toggle-groups
      :leader
      :desc "Kill all buffers in group"
      "t k" #'centaur-tabs-kill-all-buffer-in-current-group
      :leader
      :desc "Next tab"
      "t n" #'centaur-tabs-forward
      :leader
      :desc "Previous tab"
      "t p" #'centaur-tabs-backward)

(require 'ox-groff)

(map! :leader
      :desc "Edit agenda file"
      "/ a" #'(lambda () (interactive) (find-file "~/Org/agenda.org")))
(map! :leader
      :desc "Edit doom config.org"
      "/ c" #'(lambda () (interactive) (find-file "~/.doom.d/config.org")))
(map! :leader
      :desc "Edit eshell aliases"
      "/ e" #'(lambda () (interactive) (find-file "~/.doom.d/aliases")))
(map! :leader
      :desc "Edit doom init.el"
      "/ i" #'(lambda () (interactive) (find-file "~/.doom.d/init.el")))
(map! :leader
      :desc "Edit doom packages.el"
      "/ p" #'(lambda () (interactive) (find-file "~/.doom.d/packages.el")))
(map! :leader
      :desc "Ssh into distrotube.com"
      "/ s" #'(lambda () (interactive) (find-file "/scp:derek@distrotube.com")))

(map! :leader
      :desc "Ssh into distrotube.com"
      "\\ d" #'(lambda () (interactive) (find-file "/scp:derek@distrotube.com")))
(map! :leader
      :desc "Ssh into my nextcloud"
      "\\ n" #'(lambda () (interactive) (find-file "/scp:derek@distrotube.net")))

(map!
  (:after dired
    (:map dired-mode-map
     :leader
     "l i" #'peep-dired
     )))
(evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file
                                             (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))
(after! doom-themes
  (setq doom-neotree-enable-variable-pitch t))

(require 'sublimity-scroll)
(require 'sublimity-map)
;; (require 'sublimity-attractive)
(sublimity-mode 0)

(setq browse-url-browser-function 'eww-browse-url)

(use-package emms
  :ensure t
  :config
  (require 'emms-setup)
  (require 'emms-info)
  (require 'emms-cue)
  (require 'emms-mode-line)
  (require 'emms-playing-time)
  (setq emms-source-file-default-directory "~/Music/Non-Classical/70s-80s/")
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-info-asynchronously t)
  (unless (eq system-type 'windows-nt)
    (setq emms-source-file-directory-tree-function
          'emms-source-file-directory-tree-find))
  (emms-all)
  (emms-default-players)
  (emms-mode-line 1)
  (emms-playing-time 1))

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
      smtpmail-smtp-service 587)
(setq mu4e-sent-folder "/Sent"
      mu4e-drafts-folder "/Drafts"
      mu4e-trash-folder "/Trash"
      mu4e-refile-folder "/All Mail")
(setq mu4e-maildir-shortcuts
      '(("/derek-distrotube/Inbox"    . ?i)
        ("/derek-distrotube/Sent"     . ?s)
        ("/derek-distrotube/All Mail" . ?a)
        ("/derek-distrotube/Trash"    . ?t)))

(after! mastodon
  (setq mastodon-instance-url "https://mastodon.technology/"))

(setq md4rd-subs-active '(archlinux commandline DistroTube DoomEmacs emacs freesoftware lbry linux linux4noobs linuxmasterrace linnuxquestions orgmode qutebrowser suckless Ubuntu unixporn UsabilityPorn vim xmonad))

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
