;;; +ui.el -*- lexical-binding: t; -*-

;; Default font scale
(setq resolution-factor 1)

;; When on my desktop, increase the font size
(when (string= (downcase (system-name)) "korriban")
  (setq resolution-factor 1.5))

;; JetBrains Mono has been my favourite font since first learning to program
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size (eval (round (* 13 resolution-factor)))))
(setq doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size (eval (round (* 18 resolution-factor)))))
(setq +modeline-height (eval (round (* 14 resolution-factor))))
(setq doom-font-increment 1)

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

;; I actually have grown to really appreciate line numbers
(setq display-line-numbers-type t)

(defface breakpoint-enabled '((t)) "Breakpoint face.")

(when IS-MAC
  ;; enable ligatures support
  ;; details here: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
  (ignore-errors
    (mac-auto-operator-composition-mode)))


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
