;;; init.el -*- lexical-binding: t; -*-

;; add to $DOOMDIR/init.el
(defvar native-comp-deferred-compilation-deny-list nil)

(doom! :completion
       (company)
       (vertico +icons)           ; the search engine of the future

       :ui
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       (emoji +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       ligatures         ; ligatures and symbols to make your code pretty again
       (modeline +light)          ; snazzy, Atom-inspired modeline, plus API
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe

       :editor
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       multiple-cursors  ; editing in many places at once
       snippets          ; my elves. They type so I don't have to

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; the elisp shell that works everywhere
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       ;(spell +flyspell) ; tasing you for misspelling mispelling
       ;grammar           ; tasing grammar mistake every you make

       :tools
       ansible
       debugger          ; FIXME stepping through code, to help you add bugs
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       lookup              ; navigate your code and its documentation
       lsp               ; M-x vscode
       magit             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       pdf               ; pdf enhancements
       rgb               ; creating color strings

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS

       :lang
       (cc +lsp)                ; C > C++ == 1
       (clojure +lsp)           ; java with a lisp
       data              ; config/data formats
       (dart +lsp +flutter)   ; paint ui and not much else
       emacs-lisp        ; drown in parentheses
       (go +lsp)         ; the hipster dialect
       (haskell +lsp)    ; a language that's lazier than I am
       hy                ; readability of scheme w/ speed of python
       (json +lsp)              ; At least it ain't XML
       (java +meghanada) ; the poster child for carpal tunnel syndrome
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       (julia +lsp)             ; a better, faster MATLAB
       (kotlin +lsp)            ; a better, slicker Java(Script)
       (latex +lsp +cdlatex +fold)             ; writing papers in Emacs has never been so fun
       (lua +lsp +fennel +moonscript) ; one-based indices I guess
       (markdown +grip)          ; writing docs for people to ignore
       (org +dragndrop +gnuplot +jupyter +pandoc +present +pretty +roam2)               ; organize your plain life in plain text
       (php +lsp)               ; perl's insecure younger brother
       (python +lsp)            ; beautiful is better than ugly
       qt                ; the 'cutest' gui framework ever
       (racket +lsp)            ; a DSL for DSLs
       rest              ; Emacs as a REST client
       rst               ; ReST in peace
       (rust +lsp)              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       (scala +lsp)             ; java, but good
       (scheme +guile +chicken +racket)   ; a fully conniving family of lisps
       (sh +lsp +powershell)                ; she sells {ba,z,fi}sh shells on the C xor
       swift             ; who asked for emoji variables?
       (web +html +css)               ; the tubes
       (yaml +lsp)              ; JSON, but readable
       (zig +lsp)               ; C, but simpler

       :app
       calendar
       emms
       everywhere        ; *leave* Emacs!? You must be joking
       (rss +org)        ; emacs as an RSS reader
       twitter           ; twitter client https://twitter.com/vnought

       :config
       (default +bindings +smartparens))
