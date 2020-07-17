(setq doom-font (font-spec :family "Mononoki Nerd Font" :size 15)
       doom-variable-pitch-font (font-spec :family "Mononoki Nerd Font" :size 15))

(setq doom-theme 'doom-palenight)

(setq org-directory "~/Documents/org/")

(setq display-line-numbers-type t)
(global-set-key "\C-x\ t" 'toggle-truncate-lines)

(setq neo-window-fixed-size nil)

(setq browse-url-browser-function 'eww-browse-url)

(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)

(map!
  (:after dired
    (:map dired-mode-map
     "C-x i" #'peep-dired
     )))
(evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file
                                             (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

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
(setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a"
      mu4e-update-interval  300)
(setq
   user-mail-address "derek@distrotube.com"
   user-full-name  "Derek Taylor"
   mu4e-compose-signature
    (concat
      "Derek Taylor\n"
      "http://www.youtube.com/DistroTube\n"))
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.1and1.com" 587 nil nil))
   smtpmail-auth-credentials
     '(("smtp.1and1.com" 587 "derek@distrotube.com" nil))
   smtpmail-default-smtp-server "smtp.1and1.com"
   smtpmail-smtp-server "smtp.1and1.com"
   smtpmail-smtp-service 587)

(setq mastodon-instance-url "https://mastodon.technology")

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
     ("https://distrowatch.com/news/dwd.xml" distrowatch linux))))
 '(package-selected-packages
   (quote
    (mastodon exwm peep-dired nav-flash evil-mu4e emms elfeed))))

(custom-set-faces
 )
(put 'downcase-region 'disabled nil)
