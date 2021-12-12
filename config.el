;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Daniil Rose"
      user-mail-address "daniil.rose@member.fsf.org")

(setq org-directory "~/Nextcloud/org/")

(setq display-line-numbers-type nil)

(setq-default
 window-combination-resize t
 delete-by-moving-to-trash t
 x-stretch-cursor t)

(setq undo-limit 80000000
      auto-save-default nil
      truncate-string-ellipsis "…"
      scroll-margin 20)

(with-eval-after-load 'subword
  (diminish 'subword-mode))
(global-subword-mode 1)

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(setq scroll-step 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(when IS-MAC
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'super))

(display-time-mode 1)

(unless (string-match-p "battery N/A" (battery))
  (display-battery-mode 1))

;; Don't bring up key recipient dialogue.
(require 'epa-file)
(setq epa-file-select-keys 1)
(setq epa-file-encrypt-to '("<danielrose@member.fsf.org>"))

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
(setq catster/use-mail nil)

(when catster/use-mail
  (add-to-list 'load-path "~/.doom.d/mu4e/")
  (require 'catsters-mail))

;; Improve org mode performance
(remove-hook 'org-mode-hook #'org-superstar-mode)

(after! org
  (setq org-fontify-quote-and-verse-blocks nil
        org-fontify-whole-heading-line nil
        org-hide-leading-stars nil
        org-startup-indented nil))

;;; Configuring Packages Defined in ~packages.el~

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
