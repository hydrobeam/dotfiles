;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)

(package! visual-fill-column) ;; center org mode
(package! s)                  ;; string operations, mainly just for snake_case/camelCase
(package! rmsbolt)            ;; see how a program is decoded
(package! insert-kaomoji)     ;; emoji, but cute ╚(•⌂•)╝ (◕‿◕)♡
(package! rust-playground)    ;; allows spawning and running quick demo rust environments on the fly
(package! numpydoc)           ;; add numpy-style docs to python code

;; good-ass screenshots
(package! screenshot :recipe (:host github :repo "tecosaur/screenshot" :branch "master" :build (:not compile)))

;; org
(package! org-drill)                ;; anki, but org
(package! org-special-block-extras) ;; custom org-blocks
(package! org-pandoc-import         ;; why ever leave org mode?
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))
(package! orgdiff :recipe (:host github :repo "tecosaur/orgdiff"))
(package! engrave-faces :recipe (:host github :repo "tecosaur/engrave-faces")) ;; pretty code block output without minted
(package! ox-chameleon :recipe (:host github :repo "tecosaur/ox-chameleon"))  ;; make latex output look like your buffer
(package! org-modern) ;;  faster org-prettification
(package! org-transclusion) ;; put file in other file
(package! laas) ;; latex auto activating snippets


;; end-org

(package! company-emoji) ;; company backend to make typing emojis more convenient => :flu
(package! tmr) ;; setting timers using a convenient notatio

(package! magit-delta) ;; better diffs for magit

;; themes
(package! ef-themes :recipe (:host github :repo "protesilaos/ef-themes" :branch "main")) ;; really pretty themes
;; agda2 mode highlighting is only on later versions of modus
(package! modus-themes  :pin "ef71c8efb17930b2de3cca8b15bc85a44691452d" :recipe (:host github :repo "protesilaos/modus-themes"  ) )

(package! protobuf-mode) ; ugh

(package! evil-escape :disable t) ;; i don't use this:  jk to get out of evil mode

(package! org-present) ;; it's in the name
(package! impatient-mode) ; the live server for html

;; the best typing you'll ever have
(package! selectric-mode :recipe (:host github :repo "hydrobeam/selectric-mode"))
(package! org-reveal)       ;; presentations that are cool
(package! org-transclusion) ;; put stuff from other files in your file

;; fucks up my indentation settings for no reason
(package! dtrt-indent :disable t)

(package! lean4-mode :pin "d1c936409ade7d93e67107243cbc0aa55cda7fd5"
        :recipe (:host github :repo "leanprover/lean4-mode" :files ("*.el" "data")))

;; (unpin! org-mode)
;; (unpin! lsp-mode)
;; (unpin! treemacs)
