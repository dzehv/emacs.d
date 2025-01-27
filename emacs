;; emacs main configuration file

;; added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5)   ; and how many of the old

;; NOTE: libs loaded with #'my-load-all-in-directory
;; path for emacs lisp libraries to load
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (let ((default-directory  "~/.emacs.d/lisp/"))
  ;; (setq load-path
        ;; (append
         ;; (let ((load-path (copy-sequence load-path))) ;; shadow
           ;; (append
            ;; (copy-sequence (normal-top-level-add-to-load-path '(".")))
            ;; (normal-top-level-add-subdirs-to-load-path)))
         ;; load-path)))

;; run server mode only for GUI session
;; no window mode should be used for local operations with $EDITOR
(if (display-graphic-p)
    ;; then
    (progn
      (require 'server)
      (setq server-name "server" ; name of the server
            server-host "localhost" ; server ip
            server-socket-dir "~/.emacs.d/server"
            server-use-tcp nil
            server-port 9999)
      (server-start) ; comment out when using 'emacs --daemon'
      ;; confirm exit
      (setq confirm-kill-emacs 'yes-or-no-p))
  ;; else
  ;; allow all sorts of modified function keys and other odd keys when running emacs with the -nw option
  ;; (when (string-match "^xterm" (getenv "TERM"))
    ;; (require 'xterm-extras)
    ;; (xterm-extra-keys))
  ;; load min emacs configuration for no window (-nw) mode
  (setq emacs-nw-conf "~/.emacs-nw")
  (if (file-exists-p emacs-nw-conf)
      (progn (load-file emacs-nw-conf)
             ;; don't init other configuration of this file if ~/.emacs-nw exists
             (with-current-buffer " *load*"
               (goto-char (point-max))))))

;; display datetime at status bar
(setq display-time-day-and-date t
      display-time-24hr-format t
      ;; display-time-format "%I:%M:%S"
      display-time-format "%a %d %b %Y %H:%M:%S %Z"
      display-time-interval 1)
(display-time)

;; disable GUI components
(tooltip-mode      -1)
(menu-bar-mode     -1) ;; Disable graphical menu
(tool-bar-mode     -1) ;; disable tool-bar
(scroll-bar-mode   -1) ;; Disable scroll-line
(blink-cursor-mode -1) ;; Disable cursor flashing
(setq use-dialog-box nil) ;; No graphic dialogs and windows
(setq redisplay-dont-pause t) ;; Better buffer rendering
(setq ring-bell-function 'ignore) ;; Disable sound signals
(column-number-mode 1) ;; Show cursor position within line

;; turn on hl-line
;; (global-hl-line-mode 1)
;; set any color as the background face of the current line
;; (set-face-background 'hl-line "#3e4446")
;; to keep syntax highlighting in the current line
;; (set-face-foreground 'highlight nil)

;; hilight parent brackets
(show-paren-mode 1)

;; OS X and Win modifier keys bindings
(cond
 ((eq system-type 'darwin)
  ;; (setq mac-pass-command-to-system nil) ;; disable OS commands by modifier keys
  ;; (set-keyboard-coding-system nil)
  (setq mac-command-modifier 'meta)    ;; sets the Command key to Meta
  (setq mac-control-modifier 'control) ;; sets the Control key to Control
  (setq ns-function-modifier 'hyper)   ;; set Mac's Fn key to Hyper
  (setq mac-option-modifier 'super))   ;; sets the Option key to Super
 ((eq system-type 'windows-nt)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (setq w32-rwindow-modifier 'super)))

;; set font
;; (set-frame-font "Menlo:pixelsize=16")
(when (member "Menlo" (font-family-list))
  (set-face-attribute 'default nil :font "Menlo:pixelsize=16"))

;; set font if emacs running in daemon mode
;; (add-to-list 'default-frame-alist
;; (cons 'font "Menlo:pixelsize=16"))

;; display the name of the current buffer in the title bar
(setq frame-title-format "GNU Emacs: %b")

;; line numbers instead of deprecated linum
(global-display-line-numbers-mode)

;; frame colors
;; (add-to-list 'default-frame-alist '(foreground-color . "white"))
;; (add-to-list 'default-frame-alist '(background-color . "white smoke"))
;; (add-to-list 'default-frame-alist '(cursor-color . "coral"))

;; snippet allows you to conveniently add all its subfolders inside "~/.emacs.d/themes/" to the theme load path
;; (let ((basedir "~/.emacs.d/themes/"))
  ;; (dolist (f (directory-files basedir))
    ;; (if (and (not (or (equal f ".") (equal f "..")))
             ;; (file-directory-p (concat basedir f)))
        ;; (add-to-list 'custom-theme-load-path (concat basedir f)))))

;; load custom theme
;; (load-theme 'dark-green t t)
;; (enable-theme 'dark-green)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; replace list-buffers
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-expert t)

;; ibuffer: Use human readable Size column instead of original one
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
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))

;; ibuffer gnus-style grouping
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("prog" (or
                        (mode . c-mode)
                        (mode . c++-mode)
                        (mode . go-mode)
                        (mode . go-dot-mod-mode)
                        (mode . java-mode)
                        (mode . groovy-mode)
                        (mode . js-mode)
                        (mode . js-json-mode)
                        (mode . lisp-mode)
                        (mode . rainbow-mode)
                        (mode . python-mode)
                        (mode . ruby-mode)
                        (mode . rust-mode)
                        (mode . php-mode)
                        (mode . css-mode)
                        (mode . html-mode)
                        (mode . mhtml-mode)
                        (mode . csharp-mode)
                        (mode . lua-mode)
                        (mode . xml-mode)
                        (mode . nxml-mode)
                        (mode . swift-mode)
                        (mode . objc-mode)
                        (mode . scala-mode)
                        (mode . erlang-mode)
                        (mode . coffee-mode)
                        (mode . typescript-mode)
                        (mode . sql-mode)
                        (mode . visual-basic-mode)
                        (mode . vba-mode)
                        (mode . matlab-mode)
                        (mode . asm-mode)
                        (mode . haskell-mode)
                        (mode . sh-mode)
                        (mode . shell-script-mode)
                        (mode . yaml-mode)
                        (mode . perl-mode)
                        (mode . protobuf-mode)
                        (mode . arduino-mode)
                        (mode . cperl-mode)))
               ("org" (or
                       (name . "\\.org$")
                       (name . "\\*Org")
                       (mode . org-agenda-mode)
                       (mode . org-mode)))
               ("make" (or
                        (mode . makefile-bsdmake-mode)
                        (mode . makefile-mode)))
               ("docker" (or
                          (mode . dockerfile-mode)))
               ("conf" (or
                        (mode . conf-mode)
                        (mode . conf-unix-mode)
                        (mode . conf-space-mode)))
               ("shell" (or
                         (mode . term-mode)
                         (mode . shell-mode)
                         (mode . eshell-mode)))
               ("emacs" (or
                         (name . "^\\.emacs$")
                         (name . "^\\.emacs-nw$")
                         (name . "^\\.emacs\\.el$")
                         (name . "^\\*GNU Emacs\\*$")
                         (name . "^\\*Edit Formulas\\*$")
                         (name . "^\\*WoMan-Log\\*$")
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")
                         (name . "^\\*Quail Completions\\*$")
                         (name . "^\\*emacs\\*$")
                         (name . "^\\*ielm\\*$")
                         (name . "^\\*vc\\*$")
                         (name . "^\\*Backtrace\\*$")
                         (name . "^\\*ediff-diff\\*$")
                         (name . "^\\*ediff-errors\\*$")
                         (name . "^\\*comment-tags\\*$")
                         (name . "^\\*Gofmt Errors\\*$")
                         (name . "^\\*Shell Command Output\\*$")
                         (mode . command-history-mode)
                         (mode . emacs-lisp-mode)
                         (mode . inferior-emacs-lisp-mode)
                         (mode . emacs-lisp-compilation-mode)
                         (mode . scheme-mode)
                         (mode . package-menu-mode)
                         (mode . compilation-mode)
                         (mode . messages-buffer-mode)
                         (mode . lisp-interaction-mode)
                         (mode . debugger-mode)
                         (mode . Buffer-menu-mode)
                         (mode . ediff-mode)
                         (mode . ediff-meta-mode)
                         (mode . speedbar-mode)
                         (mode . special-mode)
                         (mode . completion-list-mode)))
               ("dired" (mode . dired-mode))
               ;; ("erc" (mode . erc-mode))
               ("tramp" (name . "^\\*tramp"))
               ("markdown" (or
                            (name . "\\.md$")
                            (mode . markdown-mode)
                            (mode . gfm-mode)
                            (mode . gfm-view-mode)))
               ("magit" (or
                         (name . "^\\*?magit")
                         (name . "^magit[-:]")))
               ("help" (or
                        (mode . Info-mode)
                        (mode . apropos-mode)
                        (mode . help-mode)))
               ("planner" (or
                           (name . "^\\*Calendar\\*$")
                           (name . "^diary$")
                           (mode . muse-mode)))
               ("browse" (or
                          (name . "^\\*eww\\*$")
                          (mode . eww-mode)))
               ("calc" (or
                        (mode . calc-mode)
                        (mode . calculator-mode)
                        (mode . calc-trail-mode)))
               ("TeX" (or
                         (name . "\\.tex$")
                         (name . "\\.cls$")
                         (name . "\\.clo$")
                         (name . "\\.bib$")
                         (name . "\\.bst$")
                         (name . "\\.bbx$")
                         (name . "\\.cbx$")
                         (mode . tex-mode)
                         (mode . latex-mode)
                         (mode . bibtex-mode)
                         (mode . plain-tex-mode)
                         (mode . doctex-mode)
                         (mode . slitex-mode)
                         (mode . tex-shell)))
               ("gnus" (or
                        (mode . message-mode)
                        (mode . bbdb-mode)
                        (mode . mail-mode)
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)
                        (name . "^\\.bbdb$")
                        (name . "^\\.newsrc-dribble")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;; org mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cb" 'org-iswitchb)
(setq org-startup-truncated t) ; no lines wrap
(setq org-agenda-files (list "~/.emacs.d/org/work.org"
                             "~/.emacs.d/org/home.org"))
(setq org-log-done t)
(setq org-support-shift-select t)
(setq org-todo-keywords
      ;; change of these options requires full emacs restart
      ;; with cycle type
      ;; '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED")))
      ;; with key tagged
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

;; org capture at point
(defun org-capture-at-point ()
  "Insert an org capture template at point."
  (interactive)
  (org-capture 0))

(global-set-key (kbd "\C-cc") #'org-capture-at-point)

;; syntax fontify for literate programming
(setq org-src-fontify-natively 't)

;; shortcut to capture entry
(define-key global-map "\C-ct"
            (lambda () (interactive) (org-capture nil "t")))

;; disable Git backend to speed up sshfs file load among other things
;; bypass tramp vc-registered errors (hangings on remote volume editing)
;; the default list is (RCS CVS SVN SCCS Bzr Git Hg Mtn Arch)
;; (setq vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Hg Mtn Arch)))

;; to disable all vc checking
;; (setq vc-handled-backends nil)

;; disable tramp version control to avoid delays
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; nightmare mode (disables arrow keys)
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

;; enable mouse in no-window mode
(require 'mouse)
(xterm-mouse-mode t)
(mouse-wheel-mode t)

;; mouse settings
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; tramp bindings (old)
;; (cond
;; ((eq system-type 'darwin)
;; (global-set-key (kbd "s-\S-c") (lambda () (interactive) 'tramp-cleanup-all-connections))
;; (global-set-key (kbd "s-\S-l") (lambda () (interactive) 'tramp-cleanup-this-connection)))
;; (t
;; (global-set-key (kbd "s-\S-c") 'tramp-cleanup-all-connections)
;; (global-set-key (kbd "s-\S-l") 'tramp-cleanup-this-connection)))

;; ido mode settings
(require 'ido)
(ido-mode t)
(icomplete-mode t)
(ido-everywhere t)
(setq ido-vitrual-buffers t)
(setq ido-enable-flex-matching t)
(setq ido-file-extensions-order '(".org" ".txt" ".go" ".py" ".pl" ".pm" ".cfg" ".cnf" ".conf"))

;; auto complete everywhere
;; (eval-after-load 'auto-complete '(global-auto-complete-mode t))
(require 'auto-complete)
(global-auto-complete-mode t)

;; imenu autocomplete
(require 'imenu)
(setq imenu-auto-rescan      t) ;; auto update list of elisp functions
(setq imenu-use-popup-menu nil) ;; imenu dialogs only in mini-buffer
;; (global-set-key (kbd "<f6>") 'imenu) ;; call imenu by F6

;; global indent settings
(setq whitespace-line 0)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset tab-width)
;; make tab key always call a indent command
;; (setq-default tab-always-indent t)
;; make tab key call indent command or insert tab character, depending on cursor position
;; (setq-default tab-always-indent nil)
;; make tab key do indent first then completion
;; (setq-default tab-always-indent 'complete)
;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; makefile mode settings (not used)
(defun my-makefile-indent-line ()
  (save-excursion
    (forward-line 0)
    (cond
     ;; keep TABs
     ((looking-at "\t")
      t)
     ;; indent continuation lines
     ((and (not (bobp))
           (= (char-before (1- (point))) ?\\))
      (delete-horizontal-space)
      (indent-to 8))
     ;; delete all other leading whitespace
     ((looking-at "\\s-+")
      (replace-match "")))))

(add-hook 'makefile-mode-hook
          (lambda ()
            ;; (setq-local indent-line-function 'my-makefile-indent-line)
            (setq indent-tabs-mode t)
            (setq show-trailing-whitespace t)
            (setq tab-width 8)))

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
            ;; add linux kernel style
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
            ;; indent with tabs and show them as 8 chars
            (setq indent-tabs-mode t)
            (setq show-trailing-whitespace t)
            (setq tab-width 8)
            (setq c-basic-offset tab-width)
            (c-set-offset 'comment-intro 0) ;; align comments
            ;; no untabify with spaces while using backspace
            (setq backward-delete-char-untabify-method nil)
            ;; (setq align-indent-before-aligning t)
            ;; indent first, then completion
            ;; (setq c-tab-always-indent 'complete)
            ;; (setq electric-indent-inhibit t)
            ;; (setq c-indent-line tab-width)
            ;; (setq c-indent-region tab-width)
            ;; always insert tab char (reduces smart indent functionality)
            ;; (setq indent-line-function 'insert-tab)
            ;; only tab-to-tab-stop using tab (reduces smart indent functionality)
            ;; (define-key c-mode-base-map (kbd "<tab>") 'tab-to-tab-stop)
            ;; (define-key c-mode-base-map [tab] 'tab-to-tab-stop)
            ;; line style commenting (cc minor modes)
            ;; switch to line style instead of block style (C-c C-k)
            (c-toggle-comment-style)))

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
                  ;; cperl-invalid-face nil
                  ;; cperl-invalid-face (quote off)
                  cperl-merge-trailing-else t)))

(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))
(add-to-list 'auto-mode-alist '("\\.psgi$" . perl-mode))

;; golang indent settings
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq-default)
            (setq tab-width 8)
            (setq standard-indent 8)
            (setq indent-tabs-mode t)))

;; Rust settings
;; (use-package rust-mode
  ;; :init
  ;; (setq rust-mode-treesitter-derive t))

(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)
            ;; (prettify-symbols-mode)
            (setq rust-format-on-save t)))

;; Treesitter addidional settings
;; (use-package treesit-auto
  ;; :custom
  ;; (treesit-auto-install 'prompt)
  ;; :config
  ;; (treesit-auto-add-to-auto-mode-alist 'all)
  ;; (global-treesit-auto-mode))

;; (setq treesit-auto-langs '(python rust go))

;; packages repo settings
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; to update package lists: M-x package-refresh-contents <RET>

;; list the packages to install
(setq package-list '(
                     auto-complete
                     json-reformat
                     magit
                     rainbow-delimiters
                     jedi
                     markdown-mode))

;; activate installed packages
(package-initialize)
;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; groovy settings
(add-hook 'groovy-mode-hook
          (lambda()
            (setq tab-width 4)))

;; yaml
(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(setq nxml-child-indent 4
      nxml-attribute-indent 4
      nxml-slash-auto-complete-flag t)

;; python settings
;; (add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 4))))
;; (add-hook 'python-mode-hook 'jedi:ac-setup)

(setq jedi:complete-on-dot t)
(add-to-list 'auto-mode-alist '("\\.jython\\'" . python-mode))
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)
(setq-default py-shell-name "ipython"
              python-shell-interpreter "ipython"
              py-which-bufname "IPython"
              python-shell-interpreter-args "-i")

;; defadvice is deprecated since Emacs 25
;; py epc con <N> buffers hack
(defadvice epc:make-procbuf (around foo activate)
  ad-do-it
  (with-current-buffer ad-return-value
    (rename-buffer (concat " " (buffer-name)))
    (setq ad-return-value (buffer-name))))

;; arduino mode
(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86" "b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374" "9a3c51c59edfefd53e5de64c9da248c24b628d4e78cc808611abd15b3e58858f" default))
 '(package-selected-packages
   '(rust-mode dockerfile-mode arduino-mode protobuf-mode php-mode docker-tramp jedi-direx jedi rainbow-delimiters markdown-mode magit lua-mode json-reformat javap-mode auto-complete yaml-mode tt-mode tabbar spacegray-theme perl-completion nlinum neotree kolon-mode json-mode groovy-mode goto-last-change go-mode ensime edts))
 '(speedbar-show-unknown-files t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; comment tags settings (see also ~/.emacs.d/lisp/comment-tags.el to add custom tags)
(autoload 'comment-tags-mode "comment-tags-mode")
(setq comment-tags-keymap-prefix (kbd "C-c t"))
(with-eval-after-load "comment-tags"
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
        comment-tags-require-colon nil ;; e.g. TODO:
        comment-tags-case-sensitive t
        comment-tags-show-faces t
        comment-tags-lighter nil))
(add-hook 'prog-mode-hook 'comment-tags-mode)

;; auto modes
(setq filemodes
      '(("\\.env*" . conf-mode)
        ("\\.rc" . conf-mode)
        ("\\.emacs-nw\\'" . emacs-lisp-mode)))
(dolist (fmode filemodes)
  (add-to-list 'auto-mode-alist fmode))

;; dockerfile
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Markdown mode settings
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; rainbow delimiters hook
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; aliases
(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)
(defalias 'ru (lambda ()
                "Use the russian-computer input method."
                (interactive)
                (set-input-method 'russian-computer)))

;; EMMS: The Emacs Multimedia System
;; https://wikemacs.org/wiki/Media_player
(when (require 'emms-setup nil t)
  (emms-all)
  (emms-default-players)

  (define-emms-simple-player mplayer '(file url)
    (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
                  ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
                  ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
    "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen"))

;; folding
(require 'hideshow)
;; (load-library "hideshow")
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'cperl-mode-hook      'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)
(add-hook 'go-mode-hook         'hs-minor-mode)

;; some custom useful keybindings
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "M-/") 'comment-line)
(global-set-key (kbd "C-M-/") 'comment-region)
(global-set-key (kbd "C-M-?") 'uncomment-region)

;; tramp
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
(global-unset-key (kbd "s-d"))
(global-set-key (kbd "s-d u") 'dos2unix)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(global-set-key (kbd "C-x C-r") 'sudo-edit)
(global-set-key (kbd "C-c D") 'delete-file-and-buffer)
(global-set-key (kbd "C-%") 'goto-match-paren)
(global-set-key (kbd "C-x %") 'forward-or-backward-sexp)
(global-unset-key (kbd "s-w"))
(global-set-key (kbd "s-w d") 'wdired-change-to-wdired-mode)
(global-set-key (kbd "s-k l") 'kill-matching-lines)
(global-set-key (kbd "s-k r") 'yba-kill-buffers-regexp)
(global-set-key (kbd "s-R a") 'revert-all-file-buffers)
(global-unset-key (kbd "s-c"))
(global-set-key (kbd "s-c g") 'close-ibuffer-filtered-group)
(global-unset-key (kbd "s-u"))
(global-set-key (kbd "s-u p") 'unfill-paragraph)

;; hideshow binds
(global-set-key (kbd "<f9>") 'hs-toggle-hiding)
(global-set-key (kbd "C-<f9>") 'hs-hide-all)
(global-set-key (kbd "C-S-<f9>") 'hs-show-all)
(global-set-key (kbd "C-+") 'toggle-hiding)
(global-set-key (kbd "C-|") 'toggle-selective-display)

;; trim whitespaces bind
(global-unset-key (kbd "s-t"))
(global-set-key (kbd "s-t w") 'trim-buffer-whitespaces)
(global-set-key (kbd "s-t r") 'trim-region-whitespaces)

;; file operations
(global-set-key (kbd "S-s-m f") 'move-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  FUNCTIONS defun below  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; create empty buffer
(defun xah-new-empty-buffer ()
  "Open a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    ;; (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

;; ansi-term line and char modes
;; in the line mode ansi term buffer acts more like a normal text-buffer
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

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
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

;; sudo edit file or reload current buffer with sudo (set to C-x C-r)
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file (as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; another one sudo find file func
(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

;; as it says...
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

;; this code from http://emacro.sourceforge.net/ gives a vi-like way of moving over parenthesis groups. I bind it to C-%, from vi heritage
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; this modification of the above code works when the point is either right before or right after a parenthesis character, and also works with shift-selection
(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

;; load all elisp files from specified dir
(defun my-load-all-in-directory (dir)
  "`load' all elisp libraries in directory DIR which are not already loaded."
  (interactive "D")
  (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                  (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))

;; load lisp libs from specified dir
(my-load-all-in-directory "~/.emacs.d/lisp/")

;; kill lines matching by regex
(defun kill-matching-lines (regexp &optional rstart rend interactive)
  "Kill lines containing matches for REGEXP.

See `flush-lines' or `keep-lines' for behavior of this command.

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)"
  (interactive
   (keep-lines-read-args "Kill lines containing match for regexp"))
  (let ((buffer-file-name nil)) ;; HACK for `clone-buffer'
    (with-current-buffer (clone-buffer nil nil)
      (let ((inhibit-read-only t))
        (keep-lines regexp rstart rend interactive)
        (kill-region (or rstart (line-beginning-position))
                     (or rend (point-max))))
      (kill-buffer)))
  (unless (and buffer-read-only kill-read-only-ok)
    ;; delete lines or make the "Buffer is read-only" error.
    (flush-lines regexp rstart rend interactive)))

;; kill by regex matching buffers filenames
(defun yba-kill-buffers-regexp (regexp)
  "Kill buffers related to a file, whose filename match against the regexp."
  (interactive "sRegexp? ")
  (let ((count-killed-buffers
         (length (mapcar
                  #'kill-buffer
                  (remove-if-not
                   (lambda (x)
                     (and
                      (buffer-file-name x)
                      (string-match regexp (buffer-file-name x))))
                   (buffer-list))))))
    (if (zerop count-killed-buffers)
        (message "No buffer matches. ")
      (message "A result of %i buffers has been killed. " count-killed-buffers))))

(defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; revert only buffers containing files, which are not modified
      ;; do not try to revert non-file buffers like *Messages*
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; if the file exists and is readable, revert the buffer
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; otherwise, kill the buffer
          (let (kill-buffer-query-functions) ; no query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

;; should be used without ido (if configured)
;; type C-x C-f C-f file:line
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
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-number))))))

(advice-add 'find-file :around #'find-file--line-number)

(defun close-ibuffer-filtered-group (group-name)
  "Close buffers of specified ibuffer filter group."
  (interactive "sGroup name: ")
  (ibuffer)
  (ibuffer-jump-to-filter-group group-name)
  (ibuffer-mark-forward 0 0 0)
  (ibuffer-do-delete)
  ;; (switch-to-prev-buffer)
  (message "Killed buffers from group %s" group-name))

;; launch and restart emacs with elisp
(defun launch-separate-emacs-in-terminal ()
  (suspend-emacs "fg ; emacs -nw"))

(defun launch-separate-emacs-under-x ()
  (if (eq system-type 'darwin)
      (call-process "/Applications/Emacs.app/Contents/MacOS/Emacs")
    (call-process "sh" nil nil nil "-c" "emacs &")))

(defun restart-emacs ()
  (interactive)
  ;; we need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (let ((kill-emacs-hook (append kill-emacs-hook
                                 (list
                                  (if (display-graphic-p)
                                      #'launch-separate-emacs-under-x
                                    #'launch-separate-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;; after usage spaces can be removed with Query replace
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; this would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; define function to shutdown emacs server instance
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

;; hideshow toggle
(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))

;; cargo cult adaptation of event-apply-control-modifier
;; adopt to use query-replace-regexp functions bindings on mac os
;; now, you can type Control-x @ Shift-7 Shift-5, emacs will see C-x @ & %,
;; interpret it as C-M-%, and run finally query-replace-regexp
(defun event-apply-control-meta-modifiers (ignore-prompt)
  (vector
   (event-apply-modifier
    (event-apply-modifier (read-event)
                          'control 26 "C-")
    'meta 27 "M-")))

(define-key function-key-map (kbd "C-x @ &") 'event-apply-control-meta-modifiers)

;; same, but apply only M- modifier
;; Control-x @ Shift-5 Shift-5 -> C-x @ % % -> M-%
;; so call query-replace function
(defun event-apply-meta-modifiers (ignore-prompt)
  (vector
   (event-apply-modifier (read-event)
                         'meta 27 "M-")))

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
    (message "Old file is %s and new file is %s"
             old-location
             new-location)
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))


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
  "Collapse prettified json in region between BEG and END to a single line"
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
