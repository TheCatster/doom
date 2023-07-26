;;; +ui.el -*- lexical-binding: t; -*-

;; Causing issues with Rust Analyzer
;; (use-package! catppuccin-theme
;;   :config
;;   (load-theme 'catppuccin t t)
;;   (setq catppuccin-flavor 'frappe)
;;   (catppuccin-reload)
;;   (setq doom-theme 'catppuccin))

(defun adjust-font-sizes ()
  "This fixes my issues with fonts when starting as a daemon"
  (interactive)
  (setq user-font
        (cond
         ((find-font (font-spec :name "JetBrainsMono Nerd Font")) "JetBrainsMono Nerd Font")
         ((find-font (font-spec :name "Droid Sans Fallback")) "Droid Sans Fallback")))

  ;; calculate the font size based on display-pixel-height
  (setq resolution-factor 1.33)
  (when (display-graphic-p)
    (setq resolution-factor (eval (/ (x-display-pixel-height) 1440.0))))
  (setq doom-font (font-spec :family user-font :size (eval (round (* 13 resolution-factor))))
        doom-big-font (font-spec :family user-font :size (eval (round (* 18 resolution-factor))))
        doom-variable-pitch-font (font-spec :family user-font :size (eval (round (* 13 resolution-factor))))
        +modeline-height (eval (round (* 14 resolution-factor))))
  (setq doom-font-increment 1))

(adjust-font-sizes)
(add-hook 'window-size-change-functions #'adjust-font-sizes)

;; Update window divider in terminal
;; https://www.reddit.com/r/emacs/comments/3u0d0u/how_do_i_make_the_vertical_window_divider_more/
(unless (display-graphic-p)
  (defun my-change-window-divider ()
    (ignore-errors
      (let ((display-table (or buffer-display-table standard-display-table)))
        (set-display-table-slot display-table 5 ?│))))
  ;; (set-window-display-table (selected-window) display-table)

  (add-hook 'window-configuration-change-hook #'my-change-window-divider))

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-major-mode-icon t
        ;; My mac vsplit screen won't fit
        doom-modeline-window-width-limit (- fill-column 10)))

(setq +workspaces-on-switch-project-behavior t)

(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)

;; disable line-numbers by default
(setq display-line-numbers-type nil)

(defface breakpoint-enabled '((t)) "Breakpoint face.")

;; (when IS-MAC
;;   ;; enable ligatures support
;;   ;; details here: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
;;   (ignore-errors
;;     (mac-auto-operator-composition-mode)))


(after! ibuffer
  (setq-hook! 'ibuffer-hook ibuffer-formats
              '((mark modified read-only locked " "
                 (name 50 18 :left :elide)
                 " "
                 (size 9 -1 :right)
                 " "
                 (mode 16 16 :left :elide)
                 " " filename-and-process)
                (mark " "
                      (name 16 -1)
                      " " filename))))

(use-package! all-the-icons-ibuffer
  :after ibuffer
  :init (all-the-icons-ibuffer-mode 1))


(add-hook! 'process-menu-mode-hook
  (setq-local tabulated-list-format [("Process" 30 t)
                                     ("PID"      7 t)
                                     ("Status"   7 t)
                                     ("Buffer"  15 t)
                                     ("TTY"     12 t)
                                     ("Command"  0 t)]))

(after! centered-window
  (setq cwm-centered-window-width 160))

;; Replace splash screen so people stop asking why I am playing a game
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-mode-hook #'(lambda () (hl-line-mode -1)))
(add-hook '+doom-dashboard-mode-hook 'hide-mode-line-mode)

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
            "     \\/__/        \\/__/        \\/__/        \\/__/        \\/__/"))

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

(setq +doom-dashboard-ascii-banner-fn #'doom-dashboard-draw-ascii-emacs-banner-fn)
