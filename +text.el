;;;  -*- lexical-binding: t; -*-

;;;;;;;;;
;; Org ;;
;;;;;;;;;

(after! text-mode
  (setq-hook! 'text-mode-hook truncate-lines nil tab-width 8))

(when (featurep :system 'macos)
  (setq org-directory (expand-file-name "~/Documents/org")))

(when (featurep :system 'linux)
  (setq org-directory (expand-file-name "~/Nextcloud/org")))

(setq org-agenda-files (list org-directory)
      org-ellipsis " ▼ "
      org-hide-emphasis-markers t
      org-babel-python-command "python3"
      ;; The standard unicode characters are usually misaligned depending on the
      ;; font. This bugs me. Markdown #-marks for headlines are more elegant.
      org-bullets-bullet-list '("#"))

(after! org-agenda
  ;; https://old.reddit.com/r/emacs/comments/hnf3cw/my_orgmode_agenda_much_better_now_with_category/
  (setq org-agenda-category-icon-alist
        `(("work" ,(list (all-the-icons-material "work")) nil nil :ascent center)
          ("chore" ,(list (all-the-icons-material "home")) nil nil :ascent center)
          ("events" ,(list (all-the-icons-material "event")) nil nil :ascent center)
          ("todo" ,(list (all-the-icons-material "check_box")) nil nil :ascent center)
          ("solution" ,(list (all-the-icons-material "done")) nil nil :ascent center)
          ("birthday" ,(list (all-the-icons-material "cake")) nil nil :ascent center)
          ("anniversary" ,(list (all-the-icons-material "favorite")) nil nil :ascent center))))

(after! org
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i" :prepend t :kill-buffer t)

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ("p" "Templates for projects")
          ("pt" "Project todo" entry       ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("pn" "Project notes" entry      ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("pc" "Project changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-notes-file "Unreleased")
           "* TODO %?\n%i" :prepend t :kill-buffer t)))

  (setq org-log-into-drawer "LOGBOOK")

  ;; Schedule/deadline popup with default time
  (defvar org-default-time "10:30"
    "The default time for deadlines.")

  (defun advise-org-default-time (func arg &optional time)
    (let ((old-time (symbol-function #'org-read-date)))
      (cl-letf (((symbol-function #'org-read-date)
                 #'(lambda (&optional a b c d default-time f g)
                     (let ((default-time (or default-time
                                             org-default-time)))
                       (apply old-time a b c d f default-time g)))))

        (apply func arg time))))

  (advice-add #'org-deadline :around #'advise-org-default-time)
  (advice-add #'org-schedule :around #'advise-org-default-time))

(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

(setq org-roam-capture-templates
      '(("c" "computers" plain
         "%?"
         :if-new (file+head "computers/${slug}.org"
                            "#+title: ${title}\n#+filetags: \n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "references/${title}.org" "#+title: ${title}\n#+filetags: \n")
         :immediate-finish t
         :unnarrowed t)
        ("w" "languages" plain "%?"
         :if-new
         (file+head "languages/${title}.org" "#+title: ${title}\n#+filetags: \n")
         :immediate-finish t
         :unnarrowed t)
        ("l" "life" plain "%?"
         :if-new
         (file+head "life/${title}.org" "#+title: ${title}\n#+filetags: \n")
         :immediate-finish t
         :unnarrowed t)
        ("p" "penn state" plain "%?"
         :if-new
         (file+head "penn state/${title}.org" "#+title: ${title}\n#+filetags: \n")
         :immediate-finish t
         :unnarrowed t)))

(after! org-roam
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error ""))))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(after! ox-pandoc
  (setq org-pandoc-options-for-revealjs '((variable . "highlight-theme=github")
                                          (variable . "theme=white"))))

(use-package! pomm
  :defer t
  :commands (pomm pomm-third-time)
  :config
  (setq pomm-work-period 55
        pomm-long-break-period 25
        pomm-short-break-period 5
        dotty-asset-dir (expand-file-name "~/.config/dotty/assets/"))
  (when (file-exists-p! dotty-asset-dir)
    ;; Use custom audio files and remove tick audio
    (setq pomm-audio-files
          `((work . ,(concat dotty-asset-dir "sounds/Glass.wav"))
            (short-break . ,(concat dotty-asset-dir "sounds/Glass.wav"))
            (long-break . ,(concat dotty-asset-dir "sounds/Glass.wav"))
            (stop . ,(concat dotty-asset-dir "sounds/Blow.wav")))))

  (setq alert-default-style (if (featurep :system 'macos) 'osx-notifier 'libnotify)
        pomm-audio-enabled t)
  (pomm-mode-line-mode))

(use-package! org-appear
  :defer t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-delay 0.3))

;;;;;;;;;;;;;;
;; Markdown ;;
;;;;;;;;;;;;;;

(setq grip-update-after-change nil)

(setq markdown-fontify-code-blocks-natively t)

(remove-hook 'text-mode-hook #'auto-fill-mode)

(use-package! edit-indirect :defer t)

(after! markdown-mode
  (advice-add #'markdown-follow-thing-at-point :around #'doom-set-jump-a))

(use-package! tldr
  :defer t
  :config
  (setq tldr-directory-path (concat doom-etc-dir "tldr/"))
  (set-popup-rule! "^\\*tldr\\*" :side 'right :select t :quit t))


(use-package! link-hint :defer t)

(use-package! symbol-overlay :defer t)

(after! so-long
  (setq so-long-target-modes (delete 'text-mode so-long-target-modes)))

(use-package! adoc-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode)))

;;;;;;;;;
;; PDF ;;
;;;;;;;;;

(add-hook! 'pdf-view-mode (pdf-view-themed-minor-mode 1))
(add-hook! 'pdf-view-mode (pdf-annot-minor-mode 0))
(add-hook! 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; ;; ;; ;; ;;
;; Obsidian ;;
;; ;; ;; ;; ;;

(use-package! obsidian
  :config
  (obsidian-specify-path "~/Main Vault")
  (global-obsidian-mode t)
  :custom
  ;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "Inbox")
  :bind (:map obsidian-mode-map
              ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
              ("C-c C-o" . obsidian-follow-link-at-point)
              ;; Jump to backlinks
              ("C-c C-b" . obsidian-backlink-jump)
              ;; If you prefer you can use `obsidian-insert-link'
              ("C-c C-l" . obsidian-insert-wikilink)
              ;; Open the Obsidian hydra
              ("C-c M-o" . obsidian-hydra/body)))
