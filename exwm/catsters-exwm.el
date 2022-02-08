;;; catsters-exwm.el -*- lexical-binding: t; -*-
(use-package! exwm)
(require 'ido)
(use-package! windower)
(use-package! app-launcher)

(require 'browse-url)
(require 'exwm-manage)

(defun catster/run-in-background (command)
  "Run command asynchronously in the background."
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun catster/run-external-program (program)
  "Run given external program"
  (interactive)
  (start-process-shell-command "" nil program))

(defun catster/reboot-linux-computer (program)
  "Run given external program"
  (interactive)
  (start-process-shell-command "" nil "sudo reboot"))

(defun catster/exwm-start-nyxt-in-char-mode ()
  (when (string-prefix-p "nyxt" exwm-instance-name)
    (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))

(defun catster/exwm-start-firefox-in-char-mode ()
  (when (string-prefix-p "firefox" exwm-instance-name)
    (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))

(defun ambrevar/exwm-start-emacs-in-char-mode ()
  (when (string-prefix-p "emacs" exwm-instance-name)
    (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))

(defun catster/exwm-start-alacritty-in-char-mode ()
  (when (string-prefix-p "Alacritty" exwm-instance-name)
    (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))

(add-hook 'exwm-manage-finish-hook 'catster/exwm-start-nyxt-in-char-mode)
(add-hook 'exwm-manage-finish-hook 'catster/exwm-start-firefox-in-char-mode)
(add-hook 'exwm-manage-finish-hook 'catster/exwm-start-alacritty-in-char-mode)
(add-hook 'exwm-manage-finish-hook 'ambrevar/exwm-start-emacs-in-char-mode)

(defun ambrevar/call-process-to-string (program &rest args)
  "Call PROGRAM with ARGS and return output.
See also `process-lines'."
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'process-file program nil t nil args))))

(setq browse-url-generic-program
      (or
       (executable-find (or (getenv "BROWSER") ""))
       (when (executable-find "xdg-mime")
         (let ((desktop-browser (ambrevar/call-process-to-string "xdg-mime" "query" "default" "text/html")))
           (substring desktop-browser 0 (string-match "\\.desktop" desktop-browser))))
       (executable-find browse-url-chrome-program)))

(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(defun my-exwm-config-setup ()
  "My modified configuration for EXWM. Based on everything else on the internet ;D"
  (unless (get 'exwm-workspace-number 'saved-value)
    (setq exwm-workspace-number 10))
  ;; Make buffer names more meaningful
  (add-hook 'exwm-update-class-hook
            (lambda ()  (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)  (string= "gimp" exwm-instance-name))
                          (exwm-workspace-rename-buffer exwm-class-name))))
  ;; Prefix keybindings, helpful for using M-x, C-c, and C-x when in EXWM buffers
  (setq exwm-input-prefix-keys
        '(?\M-x
          ?\M-:
          ?\C-x
          ?\C-c))
  ;; Global keybindings. 0-9 bcDfFgGhHijJkKlLmnoOpQrRwW !@#$%^&*() tab f2 backspace volume-keys
  (unless (get 'exwm-input-global-keys 'saved-value)
    (setq exwm-input-global-keys
          `(
            ;; (,(kbd "s-b") . exwm-workspace-switch-to-buffer)
            (,(kbd "s-<return>") . (lambda ()
                                     (interactive)
                                     (start-process "" nil "alacritty"))) ;; vterm is nice, but I want a more real terminal right now.
            (,(kbd "s-b") . switch-to-buffer) ;; List and select buffers
            ;; (,(kbd "s-c") . )
            (,(kbd "s-G") . locate) ;; locate file, based on Linux locate command
            ;; (,(kbd "s-g") . )
            ;; (,(kbd "s-g") . )
            (,(kbd "s-g") . grep) ;; Grep search in files
            (,(kbd "s-d") . app-launcher-run-app) ;; Start an application using dmenu
            ;; (,(kbd "s-W") . )
            (,(kbd "s-m") . (lambda () ;; Toggle display of mode-line and minibuffer, in an EXWM window
                              (interactive)
                              (exwm-layout-toggle-mode-line)
                              (exwm-workspace-toggle-minibuffer)))
            (,(kbd "s-i") . exwm-input-toggle-keyboard) ;; Toggle between "line-mode" and "char-mode" in an EXWM window
            ;; 's-r': Reset (to line-mode).
            (,(kbd "s-R") . exwm-reset) ;; Try to reset EXWM to a sane mode. Panic key
            ;; Interactively select, and switch to, a workspace. Only works in non EXWM windows.
            (,(kbd "s-w") . (lambda ()
                                    (interactive)
                                    (start-process "" nil (getenv "BROWSER"))))
            ;; 's-a': Launch application.
            ;; (,(kbd "s-a") . (lambda (command)
            ;;              (interactive (list (read-shell-command "$ ")))
            ;;              (start-process-shell-command command nil command)))
            ;; 's-N': Switch to a certain workspace.
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))
            ;; 'S-s-N': Move window to, and switch to, a certain workspace.
            ,@(cl-mapcar (lambda (c n)
                           `(,(kbd (format "s-%c" c)) .
                             (lambda ()
                               (interactive)
                               (exwm-workspace-move-window ,n)
                               (exwm-workspace-switch ,n))))
                         '(?\) ?! ?@ ?# ?$ ?% ?^ ?& ?* ?\()
                         ;; '(?\= ?! ?\" ?# ?¤ ?% ?& ?/ ?\( ?\))
                         (number-sequence 0 9))

            ;; Bind "s-<f2>" to "slock", a simple X display locker.
            (,(kbd "s-<f2>") . (lambda ()
                                 (interactive)
                                 (start-process "" nil "slock")))
            (,(kbd "s-h") . windmove-left)  ;; Move to window to the left of current one. Uses universal arg
            (,(kbd "s-j") . windmove-down)  ;; Move to window below current one. Uses universal arg
            (,(kbd "s-k") . windmove-up)    ;; Move to window above current one. Uses universal arg
            (,(kbd "s-l") . windmove-right) ;; Move to window to the right of current one. Uses universal arg
            (,(kbd "s-x") . split-window-right)  ;; Split window right even when in char mode, such as Nyxt or another Emacs
            (,(kbd "s-X") . split-window-below)  ;; Split window below even when in char mode, such as Nyxt or another Emacs
            (,(kbd "s-f") . find-file)
            (,(kbd "s-<tab>") . windower-switch-to-last-buffer) ;; Switch to last open buffer in current window
            (,(kbd "s-o") . windower-toggle-single) ;; Toggle between multiple windows, and a single window
            (,(kbd "s-O") . windower-toggle-split)  ;; Toggle between vertical and horizontal split. Only works with exactly two windows.
            (,(kbd "s-H") . windower-swap-left)  ;; Swap current window with the window to the left
            (,(kbd "s-J") . windower-swap-below) ;; Swap current window with the window below
            (,(kbd "s-K") . windower-swap-above) ;; Swap current window with the window above
            (,(kbd "s-L") . windower-swap-right) ;; Swap current window with the window to the right
            (,(kbd "s-F") . exwm-floating-toggle-floating) ;; Toggle the current window between floating and non-floating states
            (,(kbd "s-Q") . exwm-layout-toggle-fullscreen) ;; Toggle fullscreen mode, when in an EXWM window.
            (,(kbd "s-q") . kill-this-buffer)
            (,(kbd "s-<backspace>") . kill-this-buffer)
            (,(kbd "s-n") . rename-buffer)
            ;; screenshots
            (,(kbd "s-p") . (lambda ()
                              (interactive)
                              (start-process "" nil "flameshot" "gui")))
            ;; audio controls
            (,(kbd "<XF86AudioRaiseVolume>") . (lambda ()
                                                 (interactive)
                                                 (start-process "" nil "pamixer" "-i" "5")))
            (,(kbd "<XF86AudioLowerVolume>") . (lambda ()
                                                 (interactive)
                                                 (start-process "" nil "pamixer" "-d" "5")))
            (,(kbd "<XF86AudioMute>") . (lambda ()
                                          (interactive)
                                          (start-process "" nil "pamixer" "-t")))
            (,(kbd "<XF86MonBrightnessUp>") . (lambda ()
                                                (interactive)
                                                (start-process "" nil "brightnessctl" "s" "10%+")))
            (,(kbd "<XF86MonBrightnessDown>") . (lambda ()
                                                  (interactive)
                                                  (start-process "" nil "brightnessctl" "s" "10%-"))))))
  ;; Line-editing shortcuts: abBdefFknpsvVwWy
  (unless (get 'exwm-input-simulation-keys 'saved-value)
    (setq exwm-input-simulation-keys
          `((,(kbd "H-b") . ,(kbd "<left>"))
            (,(kbd "H-B") . ,(kbd "C-<left>"))
            (,(kbd "H-f") . ,(kbd "<right>"))
            (,(kbd "H-F") . ,(kbd "C-<right>"))
            (,(kbd "H-p") . ,(kbd "<up>"))
            (,(kbd "H-n") . ,(kbd "<down>"))
            (,(kbd "H-a") . ,(kbd "<home>"))
            (,(kbd "H-e") . ,(kbd "<end>"))
            ;; q and w are convenient if Caps Lock key is Hyper key
            (,(kbd "H-v") . ,(kbd "<prior>"))
            (,(kbd "H-V") . ,(kbd "<next>"))
            (,(kbd "H-d") . ,(kbd "<delete>"))
            (,(kbd "H-k") . ,(kbd "S-<end> <delete>"))
            ;; cut/paste.
            (,(kbd "H-W") . ,(kbd "C-x"))
            (,(kbd "H-w") . ,(kbd "C-c"))
            (,(kbd "H-y") . ,(kbd "C-v"))
            ;; search
            (,(kbd "H-s") . ,(kbd "C-f")))))

  ;; Default is save-buffers-kill-terminal, but that may kill daemon before its finished
  (global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)
  (add-hook 'exwm-update-title-hook 'catster/exwm-rename-buffer)
  ;; Do not mess with EXWM windows when considering splitting
  ;; Especially annoying with Magit
  (defun thecatster/window-exwm-p (window)
    "Return t if WINDOW is exwm-mode"
    (equal 'exwm-mode (buffer-local-value 'major-mode (window-buffer window))))
  (defun thecatster/window-list-filter-advice (result)
    "Advice fn to exclude exwm windows from returned list, unless all are exwm."
    (or (-reject 'thecatster/window-exwm-p result) result))
  (defun thecatster/display-buffer-around-advice (orig-fun buffer-or-name
                                                           &optional action frame)
    "Advice for `display-buffer` to only use non-exwm windows if possible."
    (advice-add #'window-list-1 :filter-return #'thecatster/window-list-filter-advice)
    (unwind-protect
        (apply orig-fun buffer-or-name action frame)
      (advice-remove #'window-list-1 #'thecatster/window-list-filter-advice)))
  (advice-add #'display-buffer :around #'thecatster/display-buffer-around-advice)
  ;; Ensure that EXWM input mode is displayed in mode line
  (add-hook 'exwm-input--input-mode-change-hook
            'force-mode-line-update)
  ;; Allow resizing of non-floating windows, with mouse.
  (setq window-divider-default-bottom-width 2
        window-divider-default-right-width 2)
  (window-divider-mode)
  ;; Polybar!
(setq dw/panel-process nil)
(defun dw/kill-panel ()
  (interactive)
  (when dw/panel-process
    (ignore-errors
      (kill-process dw/panel-process)))
  (setq dw/panel-process nil))

(defun dw/start-panel ()
  (interactive)
  (dw/kill-panel)
  (setq dw/panel-process (start-process-shell-command "polybar" nil "polybar panel")))
   (savehist-mode 1)
   (add-to-list 'savehist-additional-variables 'kill-ring)
  ;; Allow switching to EXWM buffers not belonging to current workspace.
  ;; This behaviour takes some getting used to, I guess thats why its not default
   (setq exwm-layout-show-all-buffers t)
   (setq exwm-workspace-show-all-buffers t)
  ;; Mouse auto follow, and auto select the window with mouse
   (setq mouse-autoselect-window t)
   (use-package! exwm-mff
     :config
     (exwm-mff-mode 1))
  ;; This causes issues in performance for me for some strange reason,
  ;; so I disable it.
   (setq focus-follows-mouse t)
   (setq x-wait-for-event-timeout nil)
  ;; Configure Ido
   (my-exwm-config-ido)
  ;; Other configurations
   (my-exwm-config-misc))

;; This is copied from exwm-config.el
 (defun my-exwm-config--fix/ido-buffer-window-other-frame ()
   "Fix `ido-buffer-window-other-frame'."
   (defalias 'exwm-config-ido-buffer-window-other-frame
     (symbol-function #'ido-buffer-window-other-frame))
   (defun ido-buffer-window-other-frame (buffer)
     "This is a version redefined by EXWM.

You can find the original one at `exwm-config-ido-buffer-window-other-frame'."
     (with-current-buffer (window-buffer (selected-window))
       (if (and (derived-mode-p 'exwm-mode)
                exwm--floating-frame)
           ;; Switch from a floating frame.
           (with-current-buffer buffer
             (if (and (derived-mode-p 'exwm-mode)
                      exwm--floating-frame
                      (eq exwm--frame exwm-workspace--current))
                 ;; Switch to another floating frame.
                 (frame-root-window exwm--floating-frame)
               ;; Do not switch if the buffer is not on the current workspace.
               (or (get-buffer-window buffer exwm-workspace--current)
                   (selected-window))))
         (with-current-buffer buffer
           (when (derived-mode-p 'exwm-mode)
             (if (eq exwm--frame exwm-workspace--current)
                 (when exwm--floating-frame
                   ;; Switch to a floating frame on the current workspace.
                   (frame-selected-window exwm--floating-frame))
               ;; Do not switch to exwm-mode buffers on other workspace (which
               ;; won't work unless `exwm-layout-show-all-buffers' is set)
               (unless exwm-layout-show-all-buffers
                 (selected-window)))))))))

 (defun my-exwm-config-ido ()
   "Configure Ido to work with EXWM."
   ;; (ido-mode 1)
   (add-hook 'exwm-init-hook #'my-exwm-config--fix/ido-buffer-window-other-frame))

 (defun my-exwm-config-misc ()
   "Other configurations."
   ;; Make more room
   (menu-bar-mode -1)
   (tool-bar-mode -1)
   (scroll-bar-mode -1))

;; Rename buffer to window title.
 (defun catster/exwm-rename-buffer ()
   (interactive)
   (exwm-workspace-rename-buffer (concat exwm-class-name " : " (substring exwm-title 0 (min 12 (length exwm-title))))))

 (my-exwm-config-setup) ;; Does not start X11 or EXWM. Start should be done from commandline.

(provide 'catsters-exwm)
;;; catsters-exwm.el ends here
