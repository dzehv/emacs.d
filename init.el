;; -----------------------------------------------------------------------------
;; block 1: core & system initialization
;; -----------------------------------------------------------------------------

;; temporary increase garbage collection threshold for faster startup
(setq gc-cons-threshold (* 50 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1024 1024))))

;; ensure emacs environment matches your shell (crucial for macos)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :demand t ;; load immediately to provide PATH for other packages
  :config
  (exec-path-from-shell-initialize))

;; package archives & use-package bootstrap
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; bootstrap use-package for modern declarative configuration
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;; backup settings (keep working directories clean)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ;; don't delink hardlinks
      version-control t      ;; use version numbers on backups
      delete-old-versions t  ;; automatically delete excess backups
      kept-new-versions 20   ;; how many of the newest versions to keep
      kept-old-versions 5)   ;; and how many of the old

;; run server mode for gui session unconditionally
(require 'server)
(setq server-name "server" ;; name of the server
      server-host "localhost" ;; server ip
      server-socket-dir "~/.emacs.d/server"
      server-use-tcp nil
      server-port 9999)
(server-start) ;; comment out when using 'emacs --daemon'

;; confirm exit
(setq confirm-kill-emacs 'yes-or-no-p)

;; separate custom file to isolate system-generated garbage
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; -----------------------------------------------------------------------------
;; block 2: ui, macos keys & nightmare mode
;; -----------------------------------------------------------------------------

;; disable gui components (spartan mode)
(tooltip-mode      -1)
(menu-bar-mode     -1) ;; disable graphical menu
(tool-bar-mode     -1) ;; disable tool-bar
(scroll-bar-mode   -1) ;; disable scroll-line
(blink-cursor-mode -1) ;; disable cursor flashing
(setq use-dialog-box nil) ;; no graphic dialogs and windows
(setq redisplay-dont-pause t) ;; better buffer rendering
(setq ring-bell-function 'ignore) ;; disable sound signals

;; modern emacs 30 ui features
(global-display-line-numbers-mode 1) ;; native fast line numbers
(fido-vertical-mode 1)               ;; modern built-in vertical completion
(savehist-mode 1)                    ;; remember minibuffer history
(recentf-mode 1)                     ;; remember recently opened files

;; -----------------------------------------------------------------------------
;; font settings (bulletproof for server mode and new frames)
;; -----------------------------------------------------------------------------
(set-face-attribute 'default nil :font "Monaco-16")
(add-to-list 'default-frame-alist '(font . "Monaco-16"))

;; display datetime at status bar
(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-format "%a %d %b %Y %H:%M:%S %Z"
      display-time-interval 1)
(display-time)

;; show cursor position within line
(column-number-mode 1)

;; display the name of the current buffer in the title bar
(setq frame-title-format "GNU Emacs: %b")

;; macos modifier keys bindings
(setq mac-command-modifier 'meta)
(setq mac-right-command-modifier 'super)
(setq mac-option-modifier 'super)
(setq mac-control-modifier 'control)
(setq ns-function-modifier 'hyper)

;; unbreakable macos shortcuts (overriding minor mode)
(defvar my-mac-override-map (make-sparse-keymap))
(define-key my-mac-override-map (kbd "M-h") 'ns-do-hide-emacs)
(define-minor-mode my-mac-override-mode
  "a minor mode to force mac-specific keybindings to override major modes."
  :global t
  :init-value t
  :keymap my-mac-override-map)
(my-mac-override-mode 1)

;; automatic day/night theme switching (pure vanilla fix)
(when (boundp 'ns-system-appearance-change-functions)
  (add-hook 'ns-system-appearance-change-functions
            (lambda (appearance)
              (mapc #'disable-theme custom-enabled-themes)
              (pcase appearance
                ('light
                 (set-face-attribute 'default nil :background "white" :foreground "black")
                 (setq frame-background-mode 'light))
                ('dark
                 (set-face-attribute 'default nil :background "black" :foreground "white")
                 (setq frame-background-mode 'dark)))
              (mapc #'frame-set-background-mode (frame-list)))))

;; nightmare mode (disables arrow keys completely)
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))

;; resize splitted windows binds
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; -----------------------------------------------------------------------------
;; block 3: editor behavior & global modes
;; -----------------------------------------------------------------------------

;; mouse settings
(require 'mouse)
(xterm-mouse-mode t)
(mouse-wheel-mode t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; hilight parent brackets
(show-paren-mode 1)

;; global indent and whitespace settings
(setq whitespace-line 0)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset tab-width)
(setq-default show-trailing-whitespace t)

;; disable tramp version control to avoid delays
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; ibuffer basic settings
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-expert t)

;; ibuffer: use human readable size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline nil)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; ibuffer: modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only locked " "
              (name 30 30 :left :elide)
              " " (size-h 9 -1 :right)
              " " (mode 16 16 :left :elide)
              " " filename-and-process)))

;; ibuffer gnus-style grouping (your full list preserved)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("prog" (or (mode . c-mode) (mode . c++-mode) (mode . go-mode)
                           (mode . go-ts-mode) (mode . go-dot-mod-mode)
                           (mode . java-mode) (mode . groovy-mode) (mode . js-mode)
                           (mode . js-json-mode) (mode . lisp-mode) (mode . rainbow-mode)
                           (mode . python-mode) (mode . ruby-mode) (mode . rust-mode)
                           (mode . php-mode) (mode . css-mode) (mode . html-mode)
                           (mode . mhtml-mode) (mode . csharp-mode) (mode . lua-mode)
                           (mode . xml-mode) (mode . nxml-mode) (mode . swift-mode)
                           (mode . objc-mode) (mode . scala-mode) (mode . erlang-mode)
                           (mode . coffee-mode) (mode . typescript-mode) (mode . sql-mode)
                           (mode . visual-basic-mode) (mode . vba-mode) (mode . matlab-mode)
                           (mode . asm-mode) (mode . haskell-mode) (mode . sh-mode)
                           (mode . shell-script-mode) (mode . yaml-mode) (mode . perl-mode)
                           (mode . protobuf-mode) (mode . arduino-mode) (mode . cperl-mode)))
               ("org" (or (name . "\\.org$") (name . "\\*Org")
                          (mode . org-agenda-mode) (mode . org-mode)))
               ("make" (or (mode . makefile-bsdmake-mode) (mode . makefile-mode)))
               ("docker" (or (mode . dockerfile-mode)))
               ("conf" (or (mode . conf-mode) (mode . conf-unix-mode) (mode . conf-space-mode)))
               ("shell" (or (mode . term-mode) (mode . shell-mode) (mode . eshell-mode)))
               ("emacs" (or (name . "^\\.emacs$") (name . "^\\.emacs\\.el$")
                            (name . "^\\.init\\.el$") (name . "^\\*GNU Emacs\\*$")
                            (name . "^\\*Edit Formulas\\*$") (name . "^\\*WoMan-Log\\*$")
                            (name . "^\\*scratch\\*$") (name . "^\\*Messages\\*$")
                            (name . "^\\*Quail Completions\\*$") (name . "^\\*emacs\\*$")
                            (name . "^\\*ielm\\*$") (name . "^\\*vc\\*$")
                            (name . "^\\*Backtrace\\*$") (name . "^\\*ediff-diff\\*$")
                            (name . "^\\*ediff-errors\\*$") (name . "^\\*comment-tags\\*$")
                            (name . "^\\*Gofmt Errors\\*$") (name . "^\\*Shell Command Output\\*$")
                            (mode . command-history-mode) (mode . emacs-lisp-mode)
                            (mode . inferior-emacs-lisp-mode) (mode . emacs-lisp-compilation-mode)
                            (mode . scheme-mode) (mode . package-menu-mode)
                            (mode . compilation-mode) (mode . messages-buffer-mode)
                            (mode . lisp-interaction-mode) (mode . debugger-mode)
                            (mode . Buffer-menu-mode) (mode . ediff-mode)
                            (mode . ediff-meta-mode) (mode . speedbar-mode)
                            (mode . special-mode) (mode . Custom-mode)
                            (mode . completion-list-mode)))
               ("dired" (mode . dired-mode))
               ("tramp" (name . "^\\*tramp"))
               ("markdown" (or (name . "\\.md$") (mode . markdown-mode)
                               (mode . gfm-mode) (mode . gfm-view-mode)))
               ("magit" (or (name . "^\\*?magit") (name . "^magit[-:]")))
               ("help" (or (mode . Info-mode) (mode . apropos-mode) (mode . help-mode)))
               ("planner" (or (name . "^\\*Calendar\\*$") (name . "^diary$") (mode . muse-mode)))
               ("browse" (or (name . "^\\*eww\\*$") (mode . eww-mode)))
               ("calc" (or (mode . calc-mode) (mode . calculator-mode) (mode . calc-trail-mode)))
               ("TeX" (or (name . "\\.tex$") (name . "\\.cls$") (name . "\\.clo$")
                          (name . "\\.bib$") (name . "\\.bst$") (name . "\\.bbx$")
                          (name . "\\.cbx$") (mode . tex-mode) (mode . latex-mode)
                          (mode . bibtex-mode) (mode . plain-tex-mode) (mode . doctex-mode)
                          (mode . slitex-mode) (mode . tex-shell)))
               ("gnus" (or (mode . message-mode) (mode . bbdb-mode) (mode . mail-mode)
                           (mode . gnus-group-mode) (mode . gnus-summary-mode)
                           (mode . gnus-article-mode) (name . "^\\.bbdb$")
                           (name . "^\\.newsrc-dribble")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;; org mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-startup-truncated t) ;; no lines wrap
(setq org-agenda-files (list "~/.emacs.d/org/work.org"
                             "~/.emacs.d/org/home.org"))
(setq org-log-done t)
(setq org-support-shift-select t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN PROGRESS(p)" "|" "PROLONGED(l)" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "FEEDBACK(e)" "VERIFY(v)" "|" "DELEGATED(g)")
        (sequence "|" "MOVED(m)" "CANCELED(c)")))

;; org capture templates
(setq org-capture-templates
      '(("t" "TODO" entry (file "~/.emacs.d/org/work.org")
         "* TODO %? %^G \n  %U" :empty-lines 0)
        ("s" "Scheduled TODO" entry (file+headline "~/.emacs.d/org/work.org" "HEADLINE")
         "* TODO %? %^G \nSCHEDULED: %^t\n  %U" :empty-lines 0)
        ("d" "Deadline" entry (file+headline "~/.emacs.d/org/work.org" "HEADLINE")
         "* TODO %? %^G \n  DEADLINE: %^t" :empty-lines 0)
        ("p" "Priority" entry (file+headline "~/.emacs.d/org/work.org" "HEADLINE")
         "* TODO [#A] %? %^G \n  SCHEDULED: %^t")
        ("a" "Appointment" entry (file+headline "~/.emacs.d/org/work.org" "HEADLINE")
         "* %? %^G \n  %^t")
        ("l" "Link" entry (file+headline "~/.emacs.d/org/work.org" "HEADLINE")
         "* TODO %a %? %^G\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
        ("n" "Note" entry (file+headline "~/.emacs.d/org/work.org" "HEADLINE")
         "* %? %^G\n%U" :empty-lines 0)
        ("j" "Journal" entry (file+datetree "~/.emacs.d/org/home.org")
         "* %? %^G\nEntered on %U\n")))

(setq org-src-fontify-natively 't)

;; folding (hideshow)
(require 'hideshow)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'cperl-mode-hook      'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)
;; (add-hook 'go-mode-hook         'hs-minor-mode)
;; (add-hook 'go-ts-mode-hook      'hs-minor-mode) ;; added for new tree-sitter mode

;; comment tags settings (using use-package to manage external package)
(use-package comment-tags
  :defer t
  :init
  (setq comment-tags-keymap-prefix (kbd "C-c t"))
  :config
  (setq comment-tags-keyword-faces
        `(("TODO" . ,(list :weight 'bold :foreground "#28ABE3"))
          ("TBD" . ,(list :weight 'bold :foreground "#28ABE3"))
          ("FIXME" . ,(list :weight 'bold :foreground "#DB3340"))
          ("BUG" . ,(list :weight 'bold :foreground "#DB3340"))
          ("DEBUG" . ,(list :weight 'bold :foreground "#AB0BE2"))
          ("HACK" . ,(list :weight 'bold :foreground "#E8B71A"))
          ("KLUDGE" . ,(list :weight 'bold :foreground "#E8B71A"))
          ("XXX" . ,(list :weight 'bold :foreground "#F7EAC8"))
          ("INFO" . ,(list :weight 'bold :foreground "#F7EAC8"))
          ("NOTE" . ,(list :weight 'bold :foreground "#1FDA9A"))
          ("DONE" . ,(list :weight 'bold :foreground "#1FDA9A"))))
  (setq comment-tags-comment-start-only t
        comment-tags-require-colon nil
        comment-tags-case-sensitive t
        comment-tags-show-faces t
        comment-tags-lighter nil))
(add-hook 'prog-mode-hook 'comment-tags-mode)

;; -----------------------------------------------------------------------------
;; block 4: programming languages (the toolbox)
;; -----------------------------------------------------------------------------

;; asynchronous auto-formatting on save
(use-package apheleia
  :ensure t
  :defer 1
  :config
  (apheleia-global-mode +1))

;; classic go-mode (rock solid, no hangs, perfect highlighting)
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (setq-default tab-width 8 standard-indent 8)
  (setq indent-tabs-mode t)
  ;; formatting is handled by apheleia
  (add-hook 'go-mode-hook
	    (lambda ()
	      (setq-local tab-width 8))))

;; auto modes for system files
(setq filemodes
      '(("\\.env*" . conf-mode)
        ("\\.rc" . conf-mode)))
(dolist (fmode filemodes)
  (add-to-list 'auto-mode-alist fmode))

;; c mode settings (k&r + kernel styles)
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (setq c-default-style '((c-mode . "linux")
                                    (c++-mode . "linux")
                                    (objc-mode . "linux")))
            (setq indent-tabs-mode t)
            (setq show-trailing-whitespace t)
            (setq tab-width 8)
            (setq c-basic-offset tab-width)
            (c-set-offset 'comment-intro 0)
            (setq backward-delete-char-untabify-method nil)
            (c-toggle-comment-style)))

;; c++ mode settings
(add-hook 'c++-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq show-trailing-whitespace t)
            (setq tab-width 8)
            (setq c-basic-offset tab-width)
            (setq backward-delete-char-untabify-method nil)))

;; cperl mode settings
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook
          (lambda()
            (setq indent-tabs-mode t
                  tab-width 8
                  standard-indent 8
                  cperl-indent-level 8
                  cperl-close-paren-offset -8
                  cperl-continued-statement-offset 8
                  cperl-indent-parens-as-block t
                  cperl-tab-always-indent t
                  cperl-indent-subs-specially nil
                  cperl-extra-newline-before-brace nil
                  cperl-merge-trailing-else t)))

(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))
(add-to-list 'auto-mode-alist '("\\.psgi$" . perl-mode))
(add-hook 'cperl-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-t") 'my-perltidy)))

;; rust settings
(use-package rust-mode
  :defer t
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq rust-format-on-save t))))

;; groovy settings
(add-hook 'groovy-mode-hook
          (lambda()
            (setq tab-width 4)))

;; yaml
(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; nxml
(setq nxml-child-indent 4
      nxml-attribute-indent 4
      nxml-slash-auto-complete-flag t)

;; python settings
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 4))))

;; arduino mode
(use-package arduino-mode
  :mode "\\.\\(pde\\|ino\\)$")

;; dockerfile mode
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;; markdown mode
(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

;; json reformat utility
(use-package json-reformat
  :defer t)

;; rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; makefile mode settings
(defun my-makefile-indent-line ()
  (save-excursion
    (forward-line 0)
    (cond
     ((looking-at "\t") t)
     ((and (not (bobp)) (= (char-before (1- (point))) ?\\))
      (delete-horizontal-space)
      (indent-to 8))
     ((looking-at "\\s-+") (replace-match "")))))

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq show-trailing-whitespace t)
            (setq tab-width 8)))

;; -----------------------------------------------------------------------------
;; block 5: custom functions (defuns)
;; -----------------------------------------------------------------------------

;; org capture at point
(defun org-capture-at-point ()
  "Insert an org capture template at point."
  (interactive)
  (org-capture 0))

;; run perltidy on the current region or buffer
(defun my-perltidy ()
  "Run perltidy on the current region or buffer."
  (interactive)
  (let ((orig-point (point)))
    (unless (mark) (mark-defun))
    (shell-command-on-region (point-min) (point-max) "perltidy -q" nil t)
    (goto-char orig-point)
    (message "perltidy executed")))

;; py epc con <N> buffers hack (deprecated defadvice syntax converted for historical accuracy)
(defadvice epc:make-procbuf (around foo activate)
  ad-do-it
  (with-current-buffer ad-return-value
    (rename-buffer (concat " " (buffer-name)))
    (setq ad-return-value (buffer-name))))

;; create empty buffer
(defun xah-new-empty-buffer ()
  "Open a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (setq buffer-offer-save t)))

;; ansi-term line and char modes
(require 'term)
(defun jnm/term-toggle-mode ()
  "Toggles term between line mode and char mode"
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(define-key term-mode-map (kbd "C-c C-j") 'jnm/term-toggle-mode)
(define-key term-mode-map (kbd "C-c C-k") 'jnm/term-toggle-mode)
(define-key term-raw-map (kbd "C-c C-j") 'jnm/term-toggle-mode)
(define-key term-raw-map (kbd "C-c C-k") 'jnm/term-toggle-mode)

;; ansi-term yank
(defun my-term-mode-hook ()
  (define-key term-raw-map (kbd "C-y") 'term-paste)
  (define-key term-raw-map (kbd "C-k")
              (lambda ()
                (interactive)
                (term-send-raw-string "\C-k")
                (kill-line))))
(add-hook 'term-mode-hook 'my-term-mode-hook)

;; goto specified percent of buffer
(defun goto-percent (percent)
  "Goto PERCENT of buffer."
  (interactive "nGoto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

;; rename file and buffer
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; unix line endings
(defun dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

;; kill matching buffers with no ask
(defun kill-matching-buffers-just-do-it ()
  "Kill buffers whose names match REGEXP, without asking."
  (interactive)
  (cl-letf (((symbol-function 'kill-buffer-ask) #'kill-buffer))
    (call-interactively #'kill-matching-buffers)))

;; sudo edit file
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (read-file-name "Find file (as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; sudo find file
(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

;; delete file and buffer
(defun delete-file-and-buffer ()
  "Kills current buffer and deletes file it is visiting."
  (interactive)
  (if (yes-or-no-p "Really kill buffer and delete file? ")
      (let ((filename (buffer-file-name)))
        (when filename
          (if (vc-backend filename)
              (vc-delete-file filename)
            (progn
              (delete-file filename)
              (message "Deleted file: %s" filename)
              (kill-buffer)))))))

;; vi style of % jumping to matching brace
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; works when point is adjacent to parenthesis
(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

;; load all elisp files from dir
(defun my-load-all-in-directory (dir)
  "'load' all elisp libraries in directory DIR."
  (interactive "D")
  (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                  (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))

;; kill lines matching by regex
(defun kill-matching-lines (regexp &optional rstart rend interactive)
  "Kill lines containing matches for REGEXP."
  (interactive
   (keep-lines-read-args "Kill lines containing match for regexp"))
  (let ((buffer-file-name nil))
    (with-current-buffer (clone-buffer nil nil)
      (let ((inhibit-read-only t))
        (keep-lines regexp rstart rend interactive)
        (kill-region (or rstart (line-beginning-position))
                     (or rend (point-max))))
      (kill-buffer)))
  (unless (and buffer-read-only kill-read-only-ok)
    (flush-lines regexp rstart rend interactive)))

;; kill by regex matching buffers filenames
(defun yba-kill-buffers-regexp (regexp)
  "Kill buffers related to a file, whose filename match against the regexp."
  (interactive "sRegexp? ")
  (let ((count-killed-buffers
         (length (mapcar
                  #'kill-buffer
                  (cl-remove-if-not
                   (lambda (x)
                     (and
                      (buffer-file-name x)
                      (string-match regexp (buffer-file-name x))))
                   (buffer-list))))))
    (if (zerop count-killed-buffers)
        (message "No buffer matches. ")
      (message "A result of %i buffers has been killed. " count-killed-buffers))))

;; revert all buffers
(defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      (when (and filename (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          (let (kill-buffer-query-functions)
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

;; find file at line (file:line)
(defun find-file--line-number (orig-fun filename &optional wildcards)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename)))
      (apply orig-fun (list filename wildcards))
      (when line-number
        (goto-char (point-min))
        (forward-line (1- line-number))))))
(advice-add 'find-file :around #'find-file--line-number)

;; close ibuffer filtered group
(defun close-ibuffer-filtered-group (group-name)
  "Close buffers of specified ibuffer filter group."
  (interactive "sGroup name: ")
  (ibuffer)
  (ibuffer-jump-to-filter-group group-name)
  (ibuffer-mark-forward 0 0 0)
  (ibuffer-do-delete)
  (message "Killed buffers from group %s" group-name))

;; launch separate emacs instances
(defun launch-separate-emacs-in-terminal ()
  (suspend-emacs "fg ; emacs -nw"))

(defun launch-separate-emacs-under-x ()
  (if (eq system-type 'darwin)
      (call-process "/Applications/Emacs.app/Contents/MacOS/Emacs")
    (call-process "sh" nil nil nil "-c" "emacs &")))

(defun restart-emacs ()
  (interactive)
  (let ((kill-emacs-hook (append kill-emacs-hook
                                 (list
                                  (if (display-graphic-p)
                                      #'launch-separate-emacs-under-x
                                    #'launch-separate-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))

;; unfill paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; shutdown emacs server
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; hideshow universal code folding
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))

;; event-apply-control-meta-modifiers hacks for query-replace-regexp on mac os
(defun event-apply-control-meta-modifiers (ignore-prompt)
  (vector
   (event-apply-modifier
    (event-apply-modifier (read-event) 'control 26 "C-") 'meta 27 "M-")))
(define-key function-key-map (kbd "C-x @ &") 'event-apply-control-meta-modifiers)

(defun event-apply-meta-modifiers (ignore-prompt)
  (vector (event-apply-modifier (read-event) 'meta 27 "M-")))
(define-key function-key-map (kbd "C-x @ %") 'event-apply-meta-modifiers)

;; trim whitespaces functions
(defun trim-buffer-whitespaces ()
  "Trim whitespaces in whole buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[ \s\t\r\n]+$" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "[ \s\t\r\n]+$" nil t)
      (replace-match ""))))

(defun trim-region-whitespaces (begin end)
  "Trim whitespaces in region"
  (interactive "r")
  (if (region-active-p)
      (replace-regexp "[ \s\t\r\n]+$" "" nil begin end)))

;; move current buffer's file to new location
(defun move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name (file-name-nondirectory (buffer-name))
                                                          default-directory))))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (message "Old file is %s and new file is %s" old-location new-location)
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))

;; themes disable hook
(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))
(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

;; does json reformat back (unindent)
(defun json-to-single-line (beg end)
  "Collapse prettified json in region between BEG and END to a single line"
  (interactive "r")
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "\\s-+\\|\n" nil t)
            (replace-match ""))))
    (print "This function operates on a region")))

;; same for any text, but saving one space
(defun text-to-single-line (beg end)
  "Collapse text in region between BEG and END to a single line"
  (interactive "r")
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "\\s-+\\|\n|\t|\r" nil t)
            (replace-match " "))))
    (print "This function operates on a region")))

;; docker-tramp with autocomplete containers names
(defadvice tramp-completion-handle-file-name-all-completions
    (around dotemacs-completion-docker activate)
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
    ad-do-it))

;; load local libs safely if needed
(condition-case nil
    (my-load-all-in-directory "~/.emacs.d/lisp/")
  (error nil))

;; -----------------------------------------------------------------------------
;; block 6: global keybindings
;; -----------------------------------------------------------------------------

;; aliases
(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)
(defalias 'ru (lambda ()
                "Use the russian-computer input method."
                (interactive)
                (set-input-method 'russian-computer)))
(defalias 'ukr (lambda ()
                 "Use the ukrainian-computer input method."
                 (interactive)
                 (set-input-method 'ukrainian-computer)))

;; basic custom overrides
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "M-/") 'comment-line)
(global-set-key (kbd "C-M-/") 'comment-region)
(global-set-key (kbd "C-M-?") 'uncomment-region)

;; buffer management
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; org-mode fast access
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" #'org-capture-at-point)
(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map "\C-ct" (lambda () (interactive) (org-capture nil "t")))

;; hideshow keys
(global-set-key (kbd "<f9>") 'hs-toggle-hiding)
(global-set-key (kbd "C-<f9>") 'hs-hide-all)
(global-set-key (kbd "C-S-<f9>") 'hs-show-all)
(global-set-key (kbd "C-+") 'toggle-hiding)
(global-set-key (kbd "C-|") 'toggle-selective-display)

;; parenthese jumping
(global-set-key (kbd "C-%") 'goto-match-paren)
(global-set-key (kbd "C-x %") 'forward-or-backward-sexp)

;; sudo edit
(global-set-key (kbd "C-x C-r") 'sudo-edit)
(global-set-key (kbd "C-c D") 'delete-file-and-buffer)

;; custom commands bound to s- (super) prefix
(global-set-key (kbd "s-\S-c") 'tramp-cleanup-all-connections)
(global-set-key (kbd "s-\S-l") 'tramp-cleanup-this-connection)
(global-set-key (kbd "s-r") 'revert-buffer)
(global-set-key (kbd "s-.") 'xah-new-empty-buffer)
(global-set-key (kbd "s-j") 'json-reformat-region)
(global-set-key (kbd "s-M-j") 'json-to-single-line)
(global-set-key (kbd "s-M-t") 'text-to-single-line)
(global-set-key (kbd "s-g") 'goto-percent)
(global-set-key (kbd "s-b m") 'rename-file-and-buffer)
(global-unset-key (kbd "s-k"))
(global-set-key (kbd "s-k m") 'kill-matching-buffers-just-do-it)
(global-set-key (kbd "s-k l") 'kill-matching-lines)
(global-set-key (kbd "s-k r") 'yba-kill-buffers-regexp)
(global-unset-key (kbd "s-d"))
(global-set-key (kbd "s-d u") 'dos2unix)
(global-unset-key (kbd "s-w"))
(global-set-key (kbd "s-w d") 'wdired-change-to-wdired-mode)
(global-set-key (kbd "s-R a") 'revert-all-file-buffers)
(global-unset-key (kbd "s-c"))
(global-set-key (kbd "s-c g") 'close-ibuffer-filtered-group)
(global-unset-key (kbd "s-u"))
(global-set-key (kbd "s-u p") 'unfill-paragraph)
(global-unset-key (kbd "s-t"))
(global-set-key (kbd "s-t w") 'trim-buffer-whitespaces)
(global-set-key (kbd "s-t r") 'trim-region-whitespaces)

;; file operations
(global-set-key (kbd "S-s-m f") 'move-file)
