;;; config.el -*- lexical-binding: t; -*-

(load! "+ui")
(load! "+os")
(load! "+git")
(load! "+misc")
(load! "+text")
(load! "+prog")
(load! "+keys")
(load! "+latex")
(cond
 ((modulep! :tools lsp +eglot) (load! "+eglot"))
 ((modulep! :tools lsp) (load! "+lsp"))
 ((modulep! :os macos) (load! "+mac")))

(setq user-full-name "Daniil Rose"
      user-mail-address "daniil.rose@posteo.org")

(setq tramp-verbose 6)

(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode
      confirm-kill-emacs nil)

(setq-default fill-column 120
              delete-trailing-lines t)

;; Delete the selection when pasting
(delete-selection-mode 1)

;; Silence non-interactive output
(when noninteractive
  (shut-up-silence-emacs))

;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

(set-popup-rules! '(("^\\*helpful" :size 0.35)
                    ("^\\*Ibuffer\\*$" :size 0.35)
                    ("^\\*info.*" :size 80 :side right)
                    ("^\\*Man.*" :size 80 :side right)
                    ("^\\*keycast.*" :size 50 :side right)
                    ("^\\*Customize" :actions display-buffer)
                    ("^\\*edit-indirect" :size 0.6)
                    ("^\\*YASnippet Tables\\*$" :size 0.35)
                    ("^\\*grep\\*$" :size 0.35)
                    ("^\\*pytest\\*" :size 0.35)
                    ("^\\*Anaconda\\*$" :size 0.35)
                    ("\\*.*server log\\*$" :side top :size 0.20 :select nil)
                    ((lambda (buf _) (with-current-buffer buf (eq major-mode 'forge-topic-mode))) :size 0.35)))

(custom-set-variables
 '(warning-suppress-log-types '((lsp-mode) (iedit)))
 '(warning-suppress-types '((iedit))))
