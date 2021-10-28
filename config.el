;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Daniil Rose"
      user-mail-address "daniil.rose@member.fsf.org")

(setq doom-theme 'doom-one)

(setq org-directory "~/Nextcloud/org/")

(setq display-line-numbers-type nil)

;; Improve org mode performance

(remove-hook 'org-mode-hook #'org-superstar-mode)

(after! org
  (setq org-fontify-quote-and-verse-blocks nil
        org-fontify-whole-heading-line nil
        org-hide-leading-stars nil
        org-startup-indented nil))
