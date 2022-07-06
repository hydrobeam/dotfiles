;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Laith Bahodi"
      user-mail-address "laithbahodi@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-font (font-spec :family "Jetbrains Mono" :size 13.5))
;; must set alternate font too
;; or else the font goes to shit

(setq auto-save-default t)
(setq make-backup-files t)

;; save on window switch https://stackoverflow.com/questions/1413837/emacs-auto-
;; save-on-switch-buffer
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-frame (before other-frame-now activate)
  (when buffer-file-name (save-buffer)))

;; :pleading_face: do you really want to kill emacs :sob: :cry:
;;(setq confirm-kill-emacs nil)

;; cool theme
(setq doom-theme 'doom-old-hope)

;; full screen emacs on start
;; combined with initially make it start above other windows with kwin
(add-to-list 'default-frame-alist `(fullscreen))
(add-hook 'window-setup-hook 'toggle-frame-maximized t) ; may or may not be necessary, god knows



;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

;; better syntax highlighting.
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(require 'org)
(require 'ox-latex)

;; lualatex preview
(setq org-latex-pdf-process
      '("lualatex -shell-escape -interaction nonstopmode %f"
        "lualatex -shell-escape -interaction nonstopmode %f"))

;; :stolen from somewhere🤷:
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
;; would set bgcolor here but idk how
(setq org-latex-minted-options
      '(("linenos=true") ("breaklines" "true") ("breakanywhere" "true") ("numbersep=5pt")
        ))

;; use minted for code rendering
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

;; makes latex preview bigger
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((racket . T)
   (rust . T)
   (python . T)
   (C . t) (haskell . T)))


(setq org-pomodoro-length 30)
(setq org-pomodoro-short-break-length 7)
(setq org-pomodoro-long-break-length 14)


(defun dw/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode)
  ;; (variable-pitch-mode) turns off source code mode
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . dw/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        ))

;; Nice bullets in org
(use-package org-superstar
  :config
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))

(require 'org-indent)

;; center text in an orgmode buffer
(defun efs/org-mode-visual-fill()
  (setq
   visual-fill-column-center-text t)
  (setq visual-fill-column-width 110)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))
;;

;; adds time to modeline
(display-time-mode 1)

;; removes .tex files after they're rendered
(add-to-list 'org-latex-logfiles-extensions "tex")

(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

;;(setq org-export-preserve-breaks t)

;; github integration and shit
(setq auth-sources '("~/.authinfo"))


;; https://stackoverflow.com/questions/17435995/paste-an-image-on-clipboard-to-emacs-org-mode-file-without-saving-it
(defun org-insert-clipboard-image (&optional file)
  "Asks for a file to paste the contents of the clipboard to, then links to it in the org file."
  (interactive "F")
  (shell-command (concat "xclip -selection clipboard -t image/jpg -o > " file ".jpg"))
  (insert (concat "[[" file ".jpg]]"))
  (org-display-inline-images))

;; allows you to C-w C-w to treemacs
(setq treemacs-is-never-other-window nil)
(setq doom-themes-treemacs-theme "doom-colors")

;; get rid of those little things on the side
;; helps space out the buffer but idc :cold_face:
(setq fringe-styles "no-fringes")
;; just looks kinda bad with certain themes

;; (setq org-highlight-latex-and-related '(latex script entities))

(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
;;(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)


;; tbh not sure if this is needed, but fuck it
;; makes emacs use local emoji to render stuff
;; *might* need emacs >28.1
(set-fontset-font t 'emoji
                  '("Twemoji" . "iso10646-1") nil 'prepend)

(setq use-default-font-for-symbols nil)



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



(map!
 :leader
 :desc "Kaomoji"
 "i k" #'insert-kaomoji)

;; okay uhh, you gotta do someshit like
;; untoggling lines-tail actual effect to make this work as intended
(setq whitespace-style '(face tabs tab-mark spaces space-mark))

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
;;; config.el ends here

(setq lsp-rust-analyzer-server-display-inlay-hints t)

(after! rustic
  (setq lsp-rust-server 'rust-analyzer))


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
    (compile (concat "g++ " src  " -Wall " " -std=c++20 " " -o " exe ".out && ./" exe ".out" )
             t)))
