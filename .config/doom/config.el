;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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

(setq doom-font (font-spec :family "JetBrainsMono" :size 13.5))
(setq auto-save-default t)
(setq make-backup-files t)
(setq confirm-kill-emacs nil)

(setq doom-theme 'doom-one)


(add-to-list 'default-frame-alist `(fullscreen))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

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
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package! lsp-rust
  :config
  (setq! lsp-rust-analyzer-server-display-inlay-hints t
         lsp-rust-analyzer-inlay-hints-mode t
         ))

(setq tramp-default-method "ssh")
(require 'org)
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


(setq org-src-fontify-natively t)
(setq org-latex-minted-options
      '(("frame" "lines") ("linenos=true") ("breaklines" "true") ("breakanywhere" "true")
        ))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5))
(setq org-latex-listings 'minted)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((racket . T)
   (rust . T)
   (python . T)
   (C . t)))

;;; config.el ends here


(setq org-pomodoro-length 30)
(setq org-pomodoro-short-break-length 7)
(setq org-pomodoro-long-break-length 14)


(defun dw/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode)
  ;; (variable-pitch-mode)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . dw/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        ))
;; Nice bullets
(use-package org-superstar
  :config
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))

;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-]\\) "
;;                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(require 'org-indent)


(defun efs/org-mode-visual-fill()
  (setq
        visual-fill-column-center-text t)
  (setq visual-fill-column-width 110)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package blamer
  :ensure t
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))
