;;; ~/.doom.d/+misc.el -*- lexical-binding: t; -*-

(use-package! screenshot
  :defer t)

(use-package! keycast
  :defer t)

(after! ssh-deploy
  (setq ssh-deploy-automatically-detect-remote-changes 1))

(use-package! imenu-list
  :defer t
  :config
  (set-popup-rules! '(("^\\*Ilist\\*" :side right :size 40 :select t))))

(add-hook! 'better-jumper-post-jump-hook #'recenter)

(after! nav-flash
  (defun +advice/nav-flash-show (orig-fn &rest args)
    (ignore-errors (apply orig-fn args)))
  (advice-add 'nav-flash-show :around #'+advice/nav-flash-show))

;; Use ) key to toggle it
(after! dired
  ;; Rust version ls
  (when-let (exa (executable-find "exa"))
    (setq insert-directory-program exa)
    (setq dired-listing-switches (string-join (list "-ahl" "--group-directories-first") " "))))

(use-package! dired-x
  :config
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

(use-package! diredfl
  :hook
  ((dired-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(after! dash-docs
  (setq dash-docs-use-workaround-for-emacs-bug nil)
  (setq dash-docs-browser-func 'browse-url-generic))

(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook) #'highlight-indent-guides-mode)

(use-package! citre
  :defer t
  :init
  (require 'citre-config)
  :config
  ;; better jump set marker
  (advice-add #'citre-jump :around #'doom-set-jump-a)

  (remove-hook! 'citre-after-jump-hook #'citre-recenter-and-blink)
  (add-hook 'citre-after-jump-hook #'+nav-flash-blink-cursor-maybe-h)
  (setq
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   citre-project-root-function #'projectile-project-root
   ;; Set this if you want to always use one location to create a tags file.
   citre-default-create-tags-file-location 'package-cache
   ;; See the "Create tags file" section above to know these options
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t))
;; By default, when you open any file, and a tags file can be found for it,
;; `citre-mode' is automatically enabled.  If you only want this to work for
;; certain modes (like `prog-mode'), set it like this.
;; citre-auto-enable-citre-mode-modes '(prog-mode)


(use-package! go-translate
  :defer t
  :config
  (setq gts-translate-list '(("en" "ru") ("ru" "en") ("en" "de")))
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-google-engine) (gts-google-rpc-engine))
         :render (gts-buffer-render))))

(after! quickrun
  ;; quickrun--language-alist
  (when IS-LINUX
    (quickrun-set-default "c++" "c++/g++")))

(after! projectile
  (setq compilation-read-command nil)   ; no prompt in projectile-compile-project
  ;; . -> Build
  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                    :configure "cmake %s"
                                    :compile "cmake --build build --config Debug --target all -j 14 --"
                                    :test "ctest -j14 -C Debug -T test --output-on-failure"))

(when (modulep! :completion vertico)
  ;; Fix jump issue for vertico, https://github.com/hlissner/doom-emacs/issues/5386
  (dolist (func '(+default/search-project))
    (advice-add func :around #'doom-set-jump-a)))

(after! eshell
  ;; eshell-mode imenu index
  (add-hook! 'eshell-mode-hook (setq-local imenu-generic-expression '(("Prompt" " λ \\(.*\\)" 1))))

  (defun eshell/l (&rest args) (eshell/ls "-l" args))
  (defun eshell/e (file) (find-file file))
  (defun eshell/md (dir) (eshell/mkdir dir) (eshell/cd dir))
  (defun eshell/ft (&optional arg) (treemacs arg))

  (defun eshell/up (&optional pattern)
    (let ((p (locate-dominating-file
              (f-parent default-directory)
              (lambda (p)
                (if pattern
                    (string-match-p pattern (f-base p))
                  t)))))

      (eshell/pushd p))))

(after! term
  (add-hook! 'term-mode-hook (setq-local imenu-generic-expression '(("Prompt" "➜\\(.*\\)" 1)))))

(use-package! super-save
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package! hackernews
  :commands (hackernews))

(use-package! tokei
  :commands (tokei))

(use-package! auto-sudoedit
  :config
  (auto-sudoedit-mode 1))
