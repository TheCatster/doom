;;; +prog.el -*- lexical-binding: t; -*-

(use-package! which-func
  :defer t
  :commands which-function)

(after! company
  ;; (setq company-idle-delay 0.2)
  (setq company-format-margin-function #'company-detect-icons-margin))

(use-package! graphql-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.graphqls\\'" . graphql-mode)))

(use-package! protobuf-mode
  :defer t)

(use-package! gn-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.gni?\\'" . gn-mode)))

(use-package! bazel-mode
  :defer t)

;; Style is very important when contributing to the kernel
(setq c-default-style "linux")

(add-to-list 'auto-mode-alist '("\\.inl\\'" . +cc-c-c++-objc-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . +cc-c-c++-objc-mode))

(defun +cc/copy-lldb-breakpoint-of-current-line ()
  "Copy a pdb like breakpoint on the current line."
  (interactive)
  (kill-new
   (concat "b " (file-name-nondirectory (buffer-file-name))
           " : " (number-to-string (line-number-at-pos)))))

(add-hook! '(web-mode-hook html-mode-hook) (setq-local format-all-formatters '(("HTML" prettier))))
(add-hook! 'typescript-mode-hook (setq-local format-all-formatters '(("TypeScript" prettier))))
(add-hook! 'rjsx-mode-hook (setq-local format-all-formatters '(("JavaScript" prettier))))

(after! web-mode
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show))

(after! quickrun
  (quickrun-add-command "c++/c1z"
    '((:command . "clang++")
      (:exec    . ("%c -std=c++1z %o -o %e %s"
                   "%e %a"))
      (:remove  . ("%e")))
    :default "c++"))

(when (modulep! :tools debugger)
  (defun +my/dap-start ()
    (interactive)
    (dap-mode 1)
    (call-interactively #'dap-debug))

  (defun +my/dap-delete-output-and-stderr-buffers ()
    (doom/kill-matching-buffers " stderr*" (buffer-list))
    (doom/kill-matching-buffers " out*" (buffer-list)))

  (add-hook! dap-mode-hook ((tooltip-mode 1)))

  (after! dap-mode
    (setq dap-auto-configure-features '(sessions locals expressions controls tooltip))
    (setq lsp-enable-dap-auto-configure nil)

    ;; use M-u to exit dap-hydra
    (after! dap-hydra
      (defhydra+ dap-hydra () ("M-u" nil)))))

(defvar cspell-base-program "cspell")
(defvar cspell-config-file-path (concat "'" (expand-file-name  "~/.config/cspell/cspell.json") "'"))
(defvar cspell-args (string-join `("--config" ,cspell-config-file-path) " "))
(defun cspell-check-buffer ()
  (interactive)
  (if cspell-base-program
      (let* ((file-name (concat "'" (file-name-nondirectory (buffer-file-name)) "'"))
             (command (string-join `(,cspell-base-program ,cspell-args ,file-name) " ")))
        (compilation-start command 'grep-mode))
    (message "Cannot find cspell, please install with `npm install -g cspell`")))

(defun cspell-check-diff-from-HEAD ()
  (interactive)
  (if cspell-base-program
      (let* ((default-directory (doom-project-root))
             (command (string-join `("git diff --name-only HEAD | xargs -I{}" ,cspell-base-program ,cspell-args "'{}'") " ")))
        (compilation-start command 'grep-mode))
    (message "Cannot find cspell, please install with `npm install -g cspell`")))

(after! flycheck
  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")

  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

  (require 'flycheck-google-cpplint)
  (setq flycheck-c/c++-googlelint-executable "cpplint")
  (flycheck-add-next-checker 'c/c++-gcc '(t . c/c++-googlelint))

  (setq flycheck-c/c++-gcc-executable "gcc"
        flycheck-gcc-include-path '("/usr/local/include"))

  (add-hook! c++-mode-hook
    (setq flycheck-gcc-language-standard "c++11"
          flycheck-clang-language-standard "c++11")))

(use-package! lsp-rust
  :defer t
  :custom
  (lsp-rust-analyzer-cargo-watch-enable t)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-proc-macro-enable t)
  (lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (lsp-rust-analyzer-inlay-hints-mode t)
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-parameter-hints t))

;; Fixes crazy Poetry errors
(after! poetry
  (remove-hook 'python-mode-hook #'poetry-tracking-mode)
  (add-hook 'python-mode-hook 'poetry-track-virtualenv))
