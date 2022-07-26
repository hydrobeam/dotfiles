;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Laith Bahodi"
      user-mail-address "laithbahodi@gmail.com")

;; github integration and shit
(setq auth-sources '("~/.authinfo"))


(setq auto-save-default t)
(setq make-backup-files t)


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; get rid of those little things on the side
;; helps space out the buffer but idc :cold_face:
(setq fringe-styles "no-fringes")


;; save on window switch
;; https://stackoverflow.com/questions/1413837/emacs-auto-save-on-switch-buffer
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-frame (before other-frame-now activate)
  (when buffer-file-name (save-buffer)))

(setq doom-font (font-spec :family "Jetbrains Mono" :size 13.5))

;; 🥺 do you really want to kill emacs 😭 😿
;;(setq confirm-kill-emacs nil)

(setq modus-themes-bold-constructs t)
(setq modus-themes-mode-line '(borderless (padding . 0) (height . 0.0)))
(setq modus-themes-hl-line '())
(setq modus-themes-region '(bg-only))
(setq modus-themes-org-blocks 'gray-background)

(setq doom-theme 'modus-vivendi)

;; full screen emacs on start (fullboth makes the daemon startup in full too)
;; combined with kwin to make it start above other windows with kwin
(add-to-list 'default-frame-alist `(fullscreen . fullboth))
(add-hook 'window-setup-hook 'toggle-frame-maximized t) ; may or may not be necessary, god knows


;; adds time to modeline
;; not deferring because this is always on
(use-package! doom-modeline
  :config
  (display-time-mode 1)
  )

(use-package! treemacs
  :defer t
  :config
  ;; allows you to C-w C-w to treemacs
  (setq treemacs-is-never-other-window nil)
  (setq doom-themes-treemacs-theme "doom-colors")
  )

;; not sure why init.el doesn't do this
;; REVIEW check if this is necessary
(use-package tree-sitter
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


;; ORG MODE SETUP STARTS HERE

;; https://stackoverflow.com/questions/17435995/paste-an-image-on-clipboard-to-emacs-org-mode-file-without-saving-it
(defun org-insert-clipboard-image (&optional file)
  "Asks for a file to paste the contents of the clipboard to, then links to it in the org file."
  (interactive "F")
  (shell-command (concat "xclip -selection clipboard -t image/jpg -o > " file ".jpg"))
  (insert (concat "[[" file ".jpg]]"))
  (org-display-inline-images))


(defun dw/org-mode-setup ()
  (setq visual-fill-column-center-text t)
  (setq visual-fill-column-width 110)
  (visual-fill-column-mode 1)
  (org-indent-mode)
  (visual-line-mode)
  (setq evil-auto-indent nil))

;; after! loading because variables don't exist yet
(after! 'org-mode
  ;; removes .tex files after they're rendered
  (add-to-list 'org-latex-logfiles-extensions "tex")
  )

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
            (agenda "" ((org-agenda-overriding-header "\nNext three days\n")
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+1d")
                        (org-agenda-span 3)
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


;; Nice bullets in org
(use-package! org-superstar
  :after org
  :config
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))

(use-package! org
  :hook (org-mode . dw/org-mode-setup)
  :init
  ;; must be initialized early
  (setq org-directory "~/org/")
  :config

  (setq org-ellipsis " ▾")

  ;; latex config

  ;;; lualatex preview
  (setq org-latex-pdf-process
        '("lualatex -shell-escape -interaction nonstopmode %f"
          "lualatex -shell-escape -interaction nonstopmode %f"))

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
  ;;; would set bgcolor here but idk how

  ;;; use minted for code rendering in org block export
  (setq org-src-fontify-natively t)
  (setq org-latex-src-block-backend 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  (setq org-latex-minted-options
        '(("linenos=true") ("breaklines" "true") ("breakanywhere" "true") ("numbersep=5pt")
          ))

  ;; makes latex preview bigger
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  )

;; ORG MODE CONFIG ENDS HERE

;; copied from https://github.com/ianyepan/yay-evil-emacs/
(use-package! mwheel
  :config (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
                mouse-wheel-progressive-speed nil))

(use-package! rustic
  :defer t
  :config
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-server 'rust-analyzer)
  )

;; tbh not sure if this is needed, but fuck it
;; makes emacs use local emoji to render stuff
;; *might* need emacs >28.1
(set-fontset-font t 'emoji
                  '("Twemoji" . "iso10646-1") nil 'prepend)

;; enables emoji under emacs --daemon
;; might be a bad solution
(set-fontset-font t 'unicode "Twemoji")

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
 :desc "Increment a number by 1"
 "C-a" #'evil-numbers/inc-at-pt)

;;(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)


(map!
 :leader
 :desc "Kaomoji"
 "i k" #'insert-kaomoji)

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
