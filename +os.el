;;; ~/.doom.d/+os.el -*- lexical-binding: t; -*-

;; delete to trash
(setq delete-by-moving-to-trash t)

;; Nice features when I am on my MacBook Pro 16", which is essentially always.
(when (featurep :system 'macos)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'super))

;; Keep files up to date.
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; Subword mode is nice.
(global-subword-mode 1)

;; I get it, some deprecation messages will always be there.
(setq inhibit-startup-message t)

;; Workaround for SVG error
(setq image-types (cons 'svg image-types))
