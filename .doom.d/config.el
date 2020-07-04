;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   START OF EXWM CONFIG   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Also shrink fringes to 1 pixel.
(fringe-mode 1)

;; Turn on `display-time-mode' if you don't use an external bar.
(setq display-time-default-load-average nil)
(display-time-mode t)

;; You are strongly encouraged to enable something like `ido-mode' to alter
;; the default behavior of 'C-x b', or you will take great pains to switch
;; to or back from a floating frame (remember 'C-x 5 o' if you refuse this
;; proposal however).
;; You may also want to call `exwm-config-ido' later (see below).
(ido-mode 1)

;; Emacs server is not required to run EXWM but it has some interesting uses
;; (see next section).
(server-start)

;;;; Below are configurations for EXWM.

;; Add paths (not required if EXWM is installed from GNU ELPA).
;(add-to-list 'load-path "/path/to/xelb/")
;(add-to-list 'load-path "/path/to/exwm/")

;; Load EXWM.
(require 'exwm)

;; Fix problems with Ido (if you use it).
(require 'exwm-config)
(exwm-config-ido)

;; Set the initial number of workspaces (they can also be created later).
(setq exwm-workspace-number 10)

(require 'exwm-randr)
(setq exwm-randr-workspace-monitor-plist '(0 "DisplayPort-0" 1 "DisplayPort-1" 2 "HDMI-A-0"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output DisplayPort-0 --mode 1920x1080 --pos 0x0 --rotate normal --output DisplayPort-1 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-A-0 --mode 1920x1080 --pos 3840x0 --rotate normal")))
(exwm-randr-enable)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

;; All buffers created in EXWM mode are named "*EXWM*". You may want to
;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
;; are run when a new X window class name or title is available.  Here's
;; some advice on this topic:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + For applications with multiple windows (e.g. GIMP), the class names of
;    all windows are probably the same.  Using window titles for them makes
;;   more sense.
;; In the following example, we use class names for all windows except for
;; Java applications and GIMP.
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; Global keybindings can be defined with `exwm-input-global-keys'.
;; Here are a few examples:
(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-r] . exwm-reset)
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-&] . (lambda (command)
           (interactive (list (read-shell-command "$ ")))
           (start-process-shell-command command nil command)))
        ([s-return] . (lambda ()
          (interactive)
          (start-process "" nil "alacritty")))
        ([?\s-p] . (lambda ()
          (interactive)
          (start-process "" nil "dmenu_run")))
        ([?\s-f] . (lambda ()
          (interactive)
          (start-process "" nil "firefox")))
        ([s-f2] . (lambda ()
          (interactive)
          (start-process "" nil "/usr/bin/slock")))))

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

;; The following example demonstrates how to use simulation keys to mimic
;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
;; and DEST is what EXWM actually sends to application.  Note that both SRC
;; and DEST should be key sequences (vector or string).
(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))

;; You can hide the minibuffer and echo area when they're not used, by
;; uncommenting the following line.
;(setq exwm-workspace-minibuffer-position 'bottom)

;; Do not forget to enable EXWM. It will start by itself when things are
;; ready.  You can put it _anywhere_ in your configuration.
(exwm-enable)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   END OF EXWM CONFIG   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Mononoki Nerd Font" :size 15)
       doom-variable-pitch-font (font-spec :family "Mononoki Nerd Font" :size 15))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-palenight)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Setting the neotree width to be adjustable.
(setq neo-window-fixed-size nil)

;; Sets binding to toggle line wrap on/off.
(global-set-key "\C-x\ t" 'toggle-truncate-lines)

;; Sets binding to launch elfeed
(global-set-key (kbd "s-a") 'emms)
(global-set-key (kbd "s-b") 'ibuffer)
(global-set-key (kbd "s-d") 'dired)
(global-set-key (kbd "s-m") 'mu4e)
(global-set-key (kbd "s-n") 'elfeed)
(global-set-key (kbd "s-s") 'eshell)

;; Set browser to eww
(setq browse-url-browser-function 'eww-browse-url)

;; Force splits to open on the right
(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)

;; Image previews in dired
(global-set-key (kbd "C-x i") 'peep-dired)
(evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file
                                             (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

;; Sets transparency for focuses and unfocused frames.
;; (set-frame-parameter (selected-frame) 'alpha '(95 . 80))
;; (add-to-list 'default-frame-alist '(alpha . (95 . 80)))

;; Setting the indent guides to show a pipe character.
;; (def-package! highlight-indent-guides
;;   :commands highlight-indent-guides-mode
;;   :hook (prog-mode . highlight-indent-guides-mode)
;;   :config
;;   (setq highlight-indent-guides-method 'character
;;         highlight-indent-guides-character ?\|
;;         highlight-indent-guides-delay 0.01
;;         highlight-indent-guides-responsive 'top
;;         highlight-indent-guides-auto-enabled nil))
(use-package emms
  :ensure t
  :config
  (require 'emms-setup)
  (require 'emms-info)
  (require 'emms-cue)          ;; Required for processing flacs.
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


;; Setting up mu4e which is an email client that works within emacs.
;; You must install mu4e and mbsync through your Linux distribution's
;; package manager.
 
;; Adding path to mu4e
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(require 'mu4e)
(setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a"
      mu4e-update-interval  300)
      
;; Fields that are auto-filled in email such as the full name of the
;; sender, the salutation and signature.
(setq
   user-mail-address "derek@distrotube.com"
   user-full-name  "Derek Taylor"
   mu4e-compose-signature
    (concat
      "Derek Taylor\n"
      "http://www.youtube.com/DistroTube\n"))

;; Setting up smtp for sending mail. Make sure the gnutls command 
;; line utils are installed. Package 'gnutls-bin' in Debian/Ubuntu, 
;; and 'gnutls' in Arch.
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.1and1.com" 587 nil nil))
   smtpmail-auth-credentials
     '(("smtp.1and1.com" 587 "derek@distrotube.com" nil))
   smtpmail-default-smtp-server "smtp.1and1.com"
   smtpmail-smtp-server "smtp.1and1.com"
   smtpmail-smtp-service 587)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-feeds
   (quote
    ("https://www.gamingonlinux.com/article_rss.php" "https://hackaday.com/blog/feed/" "https://opensource.com/feed" "https://linux.softpedia.com/backend.xml" "https://itsfoss.com/feed/" "https://www.zdnet.com/topic/linux/rss.xml" "https://www.phoronix.com/rss.php" "http://feeds.feedburner.com/d0od" "https://www.computerworld.com/index.rss" "https://www.networkworld.com/category/linux/index.rss" "https://www.techrepublic.com/rssfeeds/topic/open-source/" "https://betanews.com/feed" "http://lxer.com/module/newswire/headlines.rss" "https://distrowatch.com/news/dwd.xml")))
 '(package-selected-packages (quote (exwm peep-dired nav-flash evil-mu4e emms elfeed))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
