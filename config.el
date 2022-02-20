;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Daniil Rose"
      user-mail-address "daniil.rose@member.fsf.org")

;; Load private scripts
(add-to-list 'load-path "~/.doom.d/lisp/")

;; Custom feature flags
(setq catster/use-mail nil)
(setq catster/use-exwm nil)
(setq catster/use-irc t)

;; I keep all my org mode notes synced with my iPad Air and Pixel 5a.
(setq org-directory "~/Nextcloud/org/")

;; I don't care to constantly see lines, and the performance boost is nice.
(setq display-line-numbers-type nil)

;; JetBrains Mono has been my favourite font since first learning to program
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12)
      ivy-posframe-font (font-spec :family "JetBrainsMono Nerd Font" :size 15))

;; Nice things to see related to windows, and to use trash instead of perma delete.
(setq-default
 window-combination-resize t
 delete-by-moving-to-trash t
 x-stretch-cursor t)

;; Give me as much undo as you can.
(setq undo-limit 80000000
      auto-save-default nil
      truncate-string-ellipsis "…"
      scroll-margin 20)

;; Subword mode is nice.
(with-eval-after-load 'subword
  (diminish 'subword-mode))
(global-subword-mode 1)

;; Keep files up to date.
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; Scrolling should be one line at a time.
(setq scroll-step 1)

;; Fix SSL issues
(setq gnutls-min-prime-bits 1024)
(setq gnutls-algorithm-priority "SECURE128:-VERS-SSL3.0:-VERS-TLS1.3")
(setq ssl-certificate-directory "/etc/ssl/certs")
(setq ssl-program-name "gnutls-cli")
(setq ssl-program-arguments
      '("--port" service
        "--x509cafile" "/etc/ssl/certs/ca-certificates.crt"
        "--priority" "SECURE:-VERS-SSL3.0"
        "--priority" "SECURE128:-VERS-SSL3.0:-VERS-TLS1.3"
        host))

;; Disable invasive lsp-mode features
(setq lsp-ui-sideline-enable nil   ; not anymore useful than flycheck
      lsp-ui-doc-enable nil        ; slow and redundant with K
      lsp-enable-symbol-highlighting nil
      +lsp-prompt-to-install-server 'quiet)

;; Nice features when I'm on my MacBook Pro 16", which is essentially always.
(when IS-MAC
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'super))

;; Nice to see the time.
(display-time-mode 1)

;; Don't bring up key recipient dialogue.
(require 'epa-file)
(setq epa-file-select-keys 1)
(setq epa-file-encrypt-to '("<daniil.rose@member.fsf.org>"))

;; Increase the password cache expiry time, technically doesn't do anything for GPG2
(setq password-cache-expiry (* 60 15))

;; Fix EasyPG error.
;; From https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html.
(setf epg-pinentry-mode 'loopback)
(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd
              (concat (replace-regexp-in-string "%22" "\""
                                                (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))

;; Email configuration
(when catster/use-mail
  (add-to-list 'load-path "~/.doom.d/mu4e/")
  (require 'catsters-mail))

;; Improve org mode performance
(after! org
  (setq org-fontify-quote-and-verse-blocks nil
        org-fontify-whole-heading-line nil
        org-hide-leading-stars nil
        ))

;; Replace iSearch with Consult Line
(map! :desc "Search buffer" :g "C-s" #'consult-line)

;; Replace splash screen so people stop asking why I'm playing a game
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

(defun doom-dashboard-draw-ascii-emacs-banner-fn ()
  (let* ((banner
          '("      ___          ___          ___          ___          ___ "
            "     /\\__\\        /\\  \\        /\\  \\        /\\__\\        /\\__\\"
            "    /:/ _/_      |::\\  \\      /::\\  \\      /:/  /       /:/ _/_"
            "   /:/ /\\__\\     |:|:\\  \\    /:/\\:\\  \\    /:/  /       /:/ /\\  \\"
            "  /:/ /:/ _/_  __|:|\\:\\  \\  /:/ /::\\  \\  /:/  /  ___  /:/ /::\\  \\"
            " /:/_/:/ /\\__\\/::::|_\\:\\__\\/:/_/:/\\:\\__\\/:/__/  /\\__\\/:/_/:/\\:\\__\\"
            " \\:\\/:/ /:/  /\\:\\~~\\  \\/__/\\:\\/:/  \\/__/\\:\\  \\ /:/  /\\:\\/:/ /:/  /"
            "  \\::/_/:/  /  \\:\\  \\       \\::/__/      \\:\\  /:/  /  \\::/ /:/  /"
            "   \\:\\/:/  /    \\:\\  \\       \\:\\  \\       \\:\\/:/  /    \\/_/:/  /"
            "    \\::/  /      \\:\\__\\       \\:\\__\\       \\::/  /       /:/  /"
            "     \\/__/        \\/__/        \\/__/        \\/__/        \\/__/"
            ))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(unless (display-graphic-p) ; for some reason this messes up the graphical splash screen atm
  (setq +doom-dashboard-ascii-banner-fn #'doom-dashboard-draw-ascii-emacs-banner-fn))

;;; Configuring Packages Defined in ~packages.el~

(use-package! company
  :config
  (setq company-idle-delay 0)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t))

(use-package! auto-sudoedit-mode
  :defer 1
  :diminish auto-sudoedit-mode
  :config
  (auto-sudoedit-mode 1))

(use-package! default-text-scale
  :defer 1
  :config
  (default-text-scale-mode))

(use-package! super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package! atom-one-dark-theme
  :config
  (load-theme 'atom-one-dark t))

(setq org-roam-v2-ack t)
(use-package! org-roam
  :custom
  (org-roam-v2-ack t)
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-directory (file-truename "~/Nextcloud/org/org-roam"))
  :config
  (org-roam-setup)
  (require 'org-roam-protocol))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package! hackernews
  :commands (hackernews))

(use-package blamer
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))

;;; IRC
(when catster/use-irc
  (require 'irc-config))

;;; EXWM
(when catster/use-exwm
  (add-to-list 'load-path "~/.doom.d/exwm")
  (display-battery-mode 1)
  (require 'catsters-exwm))
