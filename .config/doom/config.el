;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.

(set-file-template! "\\.org$" :trigger "__org" :mode 'org-mode)


(setq user-full-name "Laith Bahodi"
      user-mail-address "laithbahodi@gmail.com")

(setq auth-sources '("~/.authinfo.gpg"))

(setq auto-save-default t)
(setq make-backup-files t)

(visual-line-mode)
;; for spell-fu
(setq ispell-dictionary "en_CA")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; get rid of those little things on the side
;; helps space out the buffer but idc :cold_face:
(setq fringe-styles "no-fringes")


;;; drag and drop
;; allows you to drag the modeline name and yank the file
(setq mouse-drag-mode-line-buffer t)
(setq mouse-drag-and-drop-region t)


;; save on window switch
;; https://stackoverflow.com/questions/1413837/emacs-auto-save-on-switch-buffer
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-frame (before other-frame-now activate)
  (when buffer-file-name (save-buffer)))

;; nice font + big font size
(setq doom-font (font-spec :family "Jetbrains Mono" :size 13.5))

;;; modus theme config

(setq modus-themes-bold-constructs t)

;; from https://protesilaos.com/emacs/modus-themes#h:df1199d8-eaba-47db-805d-6b568a577bf3
(setq modus-themes-common-palette-overrides
      '(
        ;; colours for the modeline
        (bg-mode-line-active bg-blue-subtle)
        (fg-mode-line-active fg-main)
        (border-mode-line-active bg-blue-subtle)

        ;; makes highlighting background less dark
        (bg-region bg-dim)

        ;; makes highlighting still show colour
        (fg-region unspecified)

        ;; intense (more contrast) numbers for line numbers
        (fg-line-number-inactive fg-main)
        (fg-line-number-active fg-main)

        ;; invisible background for line numbers
        (bg-line-number-inactive unspecified)
        (bg-line-number-active unspecified)

        ;; invisible fringe:
        (fringe unspecified)
        ))

;; gives org-mode blocks a coloured background
(setq modus-themes-org-blocks 'tinted-background)



;; make every background pitch black
(setq modus-themes-vivendi-color-overrides
      '(
        (bg-dim . "#000000")  ; default is #f8f8f8
        (bg-alt . "#000000")  ; default is #f0f0f0
        ))

(setq-default pixel-scroll-precision-mode t)
(use-package! rainbow-mode
  :defer t
  :hook (prog-mode text-mode))

(setq doom-theme 'modus-vivendi)


;; full screen emacs on start (fullboth makes the daemon startup in full too)
;; combined with kwin to make it start above other windows with kwin
;;
;; these both do the job but this one works on daemon and is also less shitty overall imo
;; toggle-frame-fullscreen sometimes gets freaky
;; caused issues with screenshot.el maybe?
(add-to-list 'default-frame-alist `(fullscreen . maximized))
;; (toggle-frame-fullscreen)

(use-package! doom-modeline
  :defer t
  :config
  (display-time-mode 1)
  (setq doom-modeline-time-icon t)
  (setq doom-modeline-height 62)
  )

(use-package! vterm
  :defer t
  :config
  (map! :leader
        (:prefix ("o" . "open")
         :desc "Vterm here, but actually here as in where we are atm"
         "T" #'vterm))
  )

(use-package! treemacs
  :defer t
  :config
  ;; allows you to C-w C-w to treemacs
  (setq treemacs-is-never-other-window nil)
  ;; alters file icons to be more vscode-esque (better)
  ;; https://github.com/doomemacs/themes/wiki/Extension:-Treemacs
  (setq doom-themes-treemacs-theme "doom-colors")
  )

(map!
 :mode treemacs-mode
 :desc "yeah" "D" #'treemacs-remove-project-from-workspace)

(use-package! screenshot
  :defer t
  :config
  (setq screenshot-shadow-radius 12)
  (setq screenshot-line-numbers-p t)
  (setq screenshot-relative-line-numbers-p t)
  (setq screenshot-truncate-lines-p t)
  )



(+ 12 2)
(defun znc-register ()
  (interactive)
  (setq znc-servers
        `((,(+pass-get-secret "csc-znc-server") 6697 t
           ((libera "lbahodi" ,(+pass-get-secret "csc-znc-password"))))))
  )

(use-package! notifications
  :after erc
  :config
  (defun my/erc-send-notif (_proc parsed)
    "Notifies of every incoming PRIVMSG (aka regular channel message)"

    (let* (
           (sender (car (erc-parse-user (erc-response.sender parsed))))
           (msg (erc-response.contents parsed))
           (channel (car (erc-response.command-args parsed)))
           (cmd-args (erc-response.command-args parsed))
           )

      ;; c|m is just what the irc-bridge seems to send over from discord
      (when (string= sender "c|m")
        ;; msg format is: <user> rest of message
        ;; so we extract user and remove it from rest of message
        (setq msg-index (cl-search ">" msg))

        (setq sender (format! "(discord) %s" (substring msg 1 msg-index)))
        (setq msg (substring message-contents (+ msg-index 1)))
        )

      (notifications-notify
       :body (xml-escape-string msg t)
       :app-name "Emacs ERC"
       :title (format! "%s in %s" sender channel)
       :urgency normal)
      )

    ;; return nil to continue processing
    nil
    )
  )

(defun my/erc-enable-notifications ()
  (interactive)
  (add-hook 'erc-server-PRIVMSG-functions 'my/erc-send-notif)
  )

(defun my/erc-disable-notifications ()
  (interactive)
  (remove-hook 'erc-server-PRIVMSG-functions 'my/erc-send-notif)
  )
;; (add-hook 'erc-insert-modify-hook 'my/erc-match-message)
;; (add-hook 'erc-insert-modify-hook 'erc-global-notify)


(use-package! erc
  :defer t
  :config
  (setq erc-server "irc.libera.chat"
        erc-nick "aquabeam"
        erc-user-full-name "Laith Bahodi"
        erc-prompt-for-password 'nil
        erc-password (+pass-get-secret "irc-aquabeam-libera-password")
        )
  (setq erc-match-exclude-server-buffer t)
  (setq erc-track-visibility nil) ; Only use the selected frame for visibility
  (setq erc-fill-column 100
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 20)

  (setq erc-keywords '("aqua" "awua" "beam" "kde" "emacs"))

  (custom-set-faces!
    '(erc-my-nick-face :foreground "azure2")
    '(erc-prompt-face :foreground "thistle1"))
  )


(use-package! cpp-mode
  :defer t
  :config
  (add-to-list 'lsp-clients-clangd-args "-std=c++14")
  )

(use-package! erc-hl-nicks
  :after erc
  :config
  (add-to-list 'erc-modules 'hl-nicks)
  )

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(use-package! markdown-mode
  :defer t
  :config
  (setq markdown-fontify-code-blocks-natively t)
  )

(use-package! pdf-view
  :hook (pdf-view-mode . hide-mode-line-mode))

;; ORG MODE SETUP STARTS HERE

(use-package! auto-capitalize
  :defer t
  :config
  (setq auto-capitalize-words `("I" "English"))
  )

(use-package! laas
  :hook ((LaTeX-mode org-mode) . laas-mode)
  :config
  ;; (setq laas-use-unicode t) ;; unicode >>
  (aas-set-snippets 'laas-mode
    :cond #'texmathp ; expand only while in math
    ";1" "⊢"
    ";2" "⊥"
    "Sum" (lambda () (interactive)
            (yas-expand-snippet "\\sum_{$1^{$2} $0"))
    )
  )



;; this is from teco to make ox-chameleon work
;; https://github.com/tecosaur/emacs-config/blob/master/config.org#class-templates
(after! ox-latex

  ;; deletes generated .tex files
  (add-to-list 'org-latex-logfiles-extensions "tex")


  ;; stolen from tecosaur to make ox-chameleon work
  (let* ((article-sections '(("\\section{%s}" . "\\section*{%s}")
                             ("\\subsection{%s}" . "\\subsection*{%s}")
                             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                             ("\\paragraph{%s}" . "\\paragraph*{%s}")
                             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
         (book-sections (append '(("\\chapter{%s}" . "\\chapter*{%s}"))
                                article-sections))
         (hanging-secnum-preamble "
\\renewcommand\\sectionformat{\\llap{\\thesection\\autodot\\enskip}}
\\renewcommand\\subsectionformat{\\llap{\\thesubsection\\autodot\\enskip}}
\\renewcommand\\subsubsectionformat{\\llap{\\thesubsubsection\\autodot\\enskip}}
")
         (big-chap-preamble "
\\RedeclareSectionCommand[afterindent=false, beforeskip=0pt, afterskip=0pt, innerskip=0pt]{chapter}
\\setkomafont{chapter}{\\normalfont\\Huge}
\\renewcommand*{\\chapterheadstartvskip}{\\vspace*{0\\baselineskip}}
\\renewcommand*{\\chapterheadendvskip}{\\vspace*{0\\baselineskip}}
\\renewcommand*{\\chapterformat}{%
  \\fontsize{60}{30}\\selectfont\\rlap{\\hspace{6pt}\\thechapter}}
\\renewcommand*\\chapterlinesformat[3]{%
  \\parbox[b]{\\dimexpr\\textwidth-0.5em\\relax}{%
    \\raggedleft{{\\large\\scshape\\bfseries\\chapapp}\\vspace{-0.5ex}\\par\\Huge#3}}%
    \\hfill\\makebox[0pt][l]{#2}}
"))
    (setcdr (assoc "article" org-latex-classes)
            `(,(concat "\\documentclass{scrartcl}" hanging-secnum-preamble)
              ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("report" ,(concat "\\documentclass{scrartcl}" hanging-secnum-preamble)
                   ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("book" ,(concat "\\documentclass[twoside=false]{scrbook}"
                                   big-chap-preamble hanging-secnum-preamble)
                   ,@book-sections))
    (add-to-list 'org-latex-classes
                 `("blank" "[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                   ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("bmc-article" "\\documentclass[article,code,maths]{bmc}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                   ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("bmc" "\\documentclass[code,maths]{bmc}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                   ,@book-sections)))

  (setq org-latex-tables-booktabs t
        org-latex-hyperref-template "
\\providecolor{url}{HTML}{0077bb}
\\providecolor{link}{HTML}{882255}
\\providecolor{cite}{HTML}{999933}
\\hypersetup{
  pdfauthor={%a},
  pdftitle={%t},
  pdfkeywords={%k},
  pdfsubject={%d},
  pdfcreator={%c},
  pdflang={%L},
  breaklinks=true,
  colorlinks=true,
  linkcolor=link,
  urlcolor=url,
  citecolor=cite\n}
\\urlstyle{same}
%% hide links styles in toc
"
        org-latex-reference-command "\\cref{%s}")
  )


(use-package! org-pandoc-import :after org)

;; https://stackoverflow.com/questions/17435995/paste-an-image-on-clipboard-to-emacs-org-mode-file-without-saving-it
(defun insert-clipboard-image (&optional file)
  "Asks for a file to paste & link the contents of the clipboard"
  (interactive "F")
  (async-shell-command (concat "xclip -selection clipboard -t image/jpg -o > " file ".jpg"))
  (insert
   (cond
    ((derived-mode-p 'org-mode)(concat "[[" file ".jpg]]") )
    ((derived-mode-p 'markdown-mode) (concat "[](" file ".jpg)"))
    (t (user-error "Invalid/unsupported mode"))))
  (org-display-inline-images))


(defun dw/org-mode-setup ()
  (setq visual-fill-column-center-text t)
  (setq visual-fill-column-width 110)
  (visual-fill-column-mode 1)
  ;; (org-indent-mode)
  ;; (setq evil-auto-indent nil))
  )

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IDEA(i)" "HOLD(h)" "|" "DONE(d)" "CANCELLED(c)" "FAILED(f)" "REVIEW(r)")
          ;; (sequence "[ ](T)" "[-](S)" "|" "[X](D)")
          ;; (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")
          ))


  (setq org-todo-keyword-faces
        (append org-todo-keywords
                '(
                  ("IDEA" . (:foreground "cyan" :weight "bold"))
                  ("FAILED" . (:foreground "red" :weight "bold"))
                  ("CANCELlED" . (:foreground "red" :weight "bold"))
                  ("REVIEW" . (:foreground "#b6a0fb" :weight "bold"))
                  ))))


(use-package! org-pomodoro
  :defer t
  :config
  (setq org-pomodoro-length 30)
  (setq org-pomodoro-short-break-length 7)
  (setq org-pomodoro-long-break-length 14)
  )

(use-package! org-journal
  :defer t
  :config
  (setq org-journal-date-format "%a, %Y-%m-%d")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-enable-agenda-integration t)
  )

(use-package! org-agenda
  :defer t
  :config
  ;; copied from prot
  ;; https://protesilaos.com/codelog/2021-12-09-emacs-org-block-agenda/
  (setq org-agenda-custom-commands
        `(("A" "Daily agenda and top priority tasks"
           ((tags-todo "*"
                       ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                        (org-agenda-skip-function
                         `(org-agenda-skip-entry-if
                           'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                        (org-agenda-block-separator nil)
                        (org-agenda-overriding-header "Important tasks without a date\n")))
            (agenda "" ((org-agenda-overriding-header "\nToday's agenda\n")
                        (org-agenda-span 1)
                        (org-deadline-warning-days 0)
                        (org-agenda-block-separator nil)
                        (org-scheduled-past-days 0)
                        ;; We don't need the `org-agenda-date-today'
                        ;; highlight because that only has a practical
                        ;; utility in multi-day views.
                        (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                        (org-agenda-format-date "%A %-e %B %Y")))
            (agenda "" ((org-agenda-overriding-header "\nNext seven days\n")
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+1d")
                        (org-agenda-span 7)
                        (org-deadline-warning-days 0)
                        (org-agenda-block-separator nil)
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
            (agenda "" ((org-agenda-overriding-header "\nUpcoming Deadlines/Schedules (+14d)\n")
                        (org-agenda-time-grid nil)
                        (org-agenda-start-on-weekday nil)
                        ;; We don't want to replicate the previous section's
                        ;; three days, so we start counting from the day after.
                        (org-agenda-start-day "+4d")
                        (org-agenda-span 14)
                        (org-agenda-show-all-dates nil)
                        (org-deadline-warning-days 0)
                        (org-agenda-block-separator nil)
                        (org-agenda-entry-types '(:deadline :scheduled))
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done)))))))))

;; lets you do something like "SPC\ h" when searching

(use-package! orderless
  :defer t
  :config
  (setq orderless-component-separator #'orderless-escapable-split-on-space)
  )

(use-package! ox-pollen
  :after ox
  )
(after! 'ox
  (org-export-define-backend 'msc
                             '((link . ,(lambda (code contents info) (format "code: %s\n contents: %s\n info: %s\n" code contents info)))
                               (paragraph         . (lambda (x) x))
                               )
                             ;; :menu-entry
                             ;; '(?x "Export to Pollen"
                             ;;  ((?P "As pollen buffer"
                             ;;       (lambda (a s v b)
                             ;;         (org-export-to-buffer 'msc "*pollen buffer exports*")))))
                             )

  )
(use-package! ox-chameleon
  :after ox-latex)

(after! org-src
  (setq org-highlight-latex-and-related '(native script entities))
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
  (add-to-list 'org-src-block-faces '("rust" modus-themes-nuanced-yellow))
  )

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-todo nil)
  (setq org-modern-variable-pitch nil)
  (setq org-modern-todo-faces nil)
  (setq org-modern-checkbox nil)
  (setq org-modern-table nil)
  )


(use-package! org
  :hook (org-mode . dw/org-mode-setup)
  :defer t
  :init
  ;; must be initialized early
  (setq org-directory "~/org/")
  :config
  (setq org-ellipsis " ▾")

  ;; latex config
  ;; lualatex preview

  (setq org-latex-pdf-process
        '("lualatex -shell-escape -interaction nonstopmode %f"
          "lualatex -shell-escape -interaction nonstopmode %f"
          ))

  ;;; stolen from somewhere🤷
  (setq luamagick '(luamagick :programs ("lualatex" "convert")
                    :description "pdf > png"
                    :message "you need to install lualatex and imagemagick."
                    :use-xcolor t
                    :image-input-type "pdf"
                    :image-output-type "png"
                    :image-size-adjust (1.0 . 1.0)
                    :latex-compiler ("lualatex -interaction nonstopmode -output-directory %o %f")
                    :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O")))

  (add-to-list 'org-preview-latex-process-alist luamagick)
  (setq org-preview-latex-default-process 'luamagick) ;; lowkey no idea

  (setq org-src-fontify-natively t)
  (setq org-latex-src-block-backend 'engraved)

  ;; makes latex preview bigger
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))



  )

(use-package! engrave-faces-latex
  :after ox-latex
  :config
  (add-to-list 'org-latex-engraved-options '("linenos" "true"))
  ;; (setq org-latex-engraved-theme "t") REVIEW: doesn't work atm
  )

(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode))
;; ORG MODE CONFIG ENDS HERE

;; copied from https://github.com/ianyepan/yay-evil-emacs/
(use-package! mwheel
  :defer t
  :config (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
                mouse-wheel-progressive-speed nil))

(use-package! latex-preview-pane-mode
  :defer t
  :config
  (setq pdf-latex-command "lualatex")
  (setq shell-escape-mode t)
  ;; (add-hook 'latex-mode-hook (lambda () 'TeX-clean))
  )

(use-package! rustic
  :defer t
  :config
  ;; add rustup rust-analyzer and pipe err to devnull in case of booboos
  (add-to-list 'exec-path
               (file-name-directory (shell-command-to-string "rustup which --toolchain nightly rust-analyzer 2>/dev/null")))
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-server 'rust-analyzer)

  (setq rustic-test-arguments "-- --nocapture")
  )

(use-package! racket-mode
  :defer t
  :config
  (setq racket-shell-or-terminal-function 'racket-vterm)
  )
;; (setq lsp-eslint-server-command '("node" "/usr/bin/vscode-eslint-language-server" "--stdio"))
;; (setq lsp-eslint-server-command '("deno" "lsp"))

;; (after! eglot
;; (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))

;;   (defclass eglot-deno (eglot-lsp-server) ()
;;     :documentation "A custom class for deno lsp.")

;;   (cl-defmethod eglot-initialization-options ((server eglot-deno))
;;     "Passes through required deno initialization options"
;;     (list :enable t
;;     :lint t))
;;   )

(set-company-backend! 'prog-mode 'company-capf 'company-files)
(set-company-backend! '(text-mode org-journal-mode) 'company-capf 'company-files 'company-emoji)
;; (set-company-backend! 'text-mode 'company-capf 'company-files 'company-emoji)
(delete 'company-yasnippet +lsp-company-backends)

;; makes it so that you can page through the preview that pops when you write
;; a command with <C-h>
(after! which-key
  (setq which-key-use-C-h-commands t
        prefix-help-command #'which-key-C-h-dispatch)

  (defadvice! fix-which-key-dispatcher-a (fn &rest args)
    :around #'which-key-C-h-dispatch
    (let ((keys (this-command-keys-vector)))
      (if (equal (elt keys (1- (length keys))) ?\?)
          (let ((keys (which-key--this-command-keys)))
            (embark-bindings (seq-take keys (1- (length keys)))))
        (apply fn args)))))

;;(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)


;; gamer setup
;; (eval '(progn
;; (run-with-timer
;;  0 1.0
;;  (let ((color '#0=("violet" "indigo" "blue" "green"
;;                    "yellow" "orange" "red" . #0#)))
;;    (lambda ()
;;      (face-spec-set 'mode-line `((t :background ,(pop color)))))))) t)

(map!
 :leader
 :desc "Kaomoji"
 "i k" #'insert-kaomoji)
(visual-line-mode)

(map!
 :desc "Move up visual line"
 :nv
 "j" #'evil-next-visual-line)

(map!
 :desc "Move down visual line"
 :nv
 "k" #'evil-previous-visual-line)

(map!
 :desc "Move to end of visual line"
 :nv
 "$" #'evil-end-of-visual-line)

(map!
 :desc "Move to beginning of visual line"
 :nv
 "0" #'evil-beginning-of-visual-line)


(evil-visual-line)

(map!
 :leader
 (:prefix ("e" . "execute")
  :desc "Switch to ERC Buffer"
  "e" #'erc-switch-to-buffer))


;; compile and run cpp file in active buffer
(map! :leader
      (:prefix ("e" . "execute")
       :desc "C/C++"
       "c" #'compileandrun))

(defun compileandrun()
  "Also run it comint mode"
  (interactive)
  (let* ((src (file-name-nondirectory (buffer-file-name)))
         (exe (file-name-sans-extension src)))
    (compile (concat "g++ " src  " -Wall -g " " -std=c++17 " " -o " exe ".out && ./" exe ".out" )
             t)))

;; these don't work very well since they eliminate non-text characters
(defun snake_case (start end)
  "Change selected text to snake case format"
  (interactive "r")
  (if (use-region-p)
      (let ((given-string (buffer-substring start end)))
        (delete-region start end)
        (insert (s-snake-case given-string)))
    (message "No region selected")))

(defun lowerCamelCase (start end)
  "Change selected text to snake case format"
  (interactive "r")
  (if (use-region-p)
      (let ((given-string (buffer-substring start end)))
        (delete-region start end)
        (insert (s-lower-camel-case given-string)))
    (message "No region selected")))

(defun UpperCamelCase (start end)
  "Change selected text to snake case format"
  (interactive "r")
  (if (use-region-p)
      (let ((given-string (buffer-substring start end)))
        (delete-region start end)
        (insert (s-upper-camel-case given-string)))
    (message "No region selected")))

(defun kebab-case (start end)
  "Change selected text to snake case format"
  (interactive "r")
  (if (use-region-p)
      (let ((given-string (buffer-substring start end)))
        (delete-region start end)
        (insert (s-dashed-words given-string)))
    (message "No region selected")))

;;; config.el ends here


;; https://tecosaur.github.io/emacs-config/config.html#smerge
(defun smerge-repeatedly ()
  "Perform smerge actions again and again"
  (interactive)
  (smerge-mode 1)
  (smerge-transient))

(after! transient
  (transient-define-prefix smerge-transient ()
    [["Move"
      ("n" "next" (lambda () (interactive) (ignore-errors (smerge-next)) (smerge-repeatedly)))
      ("p" "previous" (lambda () (interactive) (ignore-errors (smerge-prev)) (smerge-repeatedly)))]
     ["Keep"
      ("b" "base" (lambda () (interactive) (ignore-errors (smerge-keep-base)) (smerge-repeatedly)))
      ("u" "upper" (lambda () (interactive) (ignore-errors (smerge-keep-upper)) (smerge-repeatedly)))
      ("l" "lower" (lambda () (interactive) (ignore-errors (smerge-keep-lower)) (smerge-repeatedly)))
      ("a" "all" (lambda () (interactive) (ignore-errors (smerge-keep-all)) (smerge-repeatedly)))
      ("RET" "current" (lambda () (interactive) (ignore-errors (smerge-keep-current)) (smerge-repeatedly)))]
     ["Diff"
      ("<" "upper/base" (lambda () (interactive) (ignore-errors (smerge-diff-base-upper)) (smerge-repeatedly)))
      ("=" "upper/lower" (lambda () (interactive) (ignore-errors (smerge-diff-upper-lower)) (smerge-repeatedly)))
      (">" "base/lower" (lambda () (interactive) (ignore-errors (smerge-diff-base-lower)) (smerge-repeatedly)))
      ("R" "refine" (lambda () (interactive) (ignore-errors (smerge-refine)) (smerge-repeatedly)))
      ("E" "ediff" (lambda () (interactive) (ignore-errors (smerge-ediff)) (smerge-repeatedly)))]
     ["Other"
      ("c" "combine" (lambda () (interactive) (ignore-errors (smerge-combine-with-next)) (smerge-repeatedly)))
      ("r" "resolve" (lambda () (interactive) (ignore-errors (smerge-resolve)) (smerge-repeatedly)))
      ("k" "kill current" (lambda () (interactive) (ignore-errors (smerge-kill-current)) (smerge-repeatedly)))
      ("q" "quit" (lambda () (interactive) (smerge-auto-leave)))]]))


(after! smartparens
  (sp-local-pair '(c++-mode objc-mode)
                 "<" ">"
                 :actions :rem)
  (sp-local-pair '(org-cdlatex-mode)
                 "$" "$")
  )


(use-package! langtool
  :defer t
  :init  (setq langtool-default-language "en-CA")
  :config
  (setq langtool-bin (executable-find "languagetool"))
  (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")
  )


;; from teco
(after! evil
  (setq evil-ex-substitute-global t     ; I like my s/../.. to by global by default
        evil-kill-on-visual-paste nil)) ; Don't put overwritten text in the kill ring

(use-package typst-mode)

(after! lsp-julia
  (setq lsp-julia-default-environment "~/.julia/environments/v1.6"))

(use-package! simple-httpd
  :defer t
  :config
  (setq httpd-host "localhost")
  (setq httpd-port 8079)
  )
