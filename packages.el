;; -*- no-byte-compile: t; -*-

;; ;; disabled packages
(disable-packages! solaire-mode
                   osx-trash
                   realgud
                   realgud-trepan-ni
                   ccls
                   tide
                   swiper
                   forge
                   code-review
                   anaconda-mode
                   company-anaconda
                   lsp-python-ms
                   pyimport)

;; text
(package! adoc-mode)
(package! tldr)
(package! edit-indirect)
(package! link-hint)
(package! symbol-overlay)
(package! pomm)
(package! org-appear)
(package! caddyfile-mode)
(package! org-cliplink)
(package! org-web-tools)
(package! flycheck-inline)
(package! obsidian)

;; Org Roam
(unpin! org-roam)
(package! org-roam-ui)

;; misc
(package! format-all)
(package! keycast)
(package! all-the-icons-ibuffer)
(package! dired-narrow)
(package! git-link)
(package! magit-delta)
(package! citre)
(package! imenu-list)
(package! go-translate)
(package! super-save)
(package! diredfl)
(package! tokei)
(package! hackernews)
(package! calctex :recipe (:host github :repo "johnbcoughlin/calctex"
                           :files ("*.el" "calctex/*.el" "calctex-contrib/*.el" "org-calctex/*.el" "vendor"))
  :pin "67a2e76847a9ea9eff1f8e4eb37607f84b380ebb")
(package! laas :recipe (:local-repo "lisp/LaTeX-auto-activating-snippets"))
(package! auto-sudoedit)

;; programming
(package! bazel-mode :recipe (:host github :repo "bazelbuild/emacs-bazel-mode"))
(package! flycheck-google-cpplint :recipe (:host github :repo "flycheck/flycheck-google-cpplint"))
(package! graphql-mode)
(package! protobuf-mode)
(package! gn-mode)
(package! live-py-mode)
(when (not (modulep! :tools lsp +eglot))
  (package! lsp-docker))
(package! py-isort)
(package! flycheck-rust)
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
