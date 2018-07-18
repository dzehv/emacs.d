;; Emacs main configuration file

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; Separate emacs graphical and terminal mode configurations template
;; (if (display-graphic-p)
;;     (progn
;;     ;; if graphic
;;       (your)
;;       (code))
;;     ;; else (optional)
;;     (your)
;;     (code))

;; Confirm exit
(setq confirm-kill-emacs 'yes-or-no-p)

;; Backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; Path for Emacs lisp librares to load
(add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((default-directory  "~/.emacs.d/lisp/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append 
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; Run server mode only for GUI session
;; No window mode should be used for local operations with $EDITOR
(if (display-graphic-p)
    (progn
      (require 'server)
      (setq server-name "server" ; name of the server
            server-host "localhost" ; server ip
            server-socket-dir "~/.emacs.d/server"
            server-use-tcp nil
            server-port 9999)
      (server-start) ; comment out when using 'emacs --daemon'
      )
  ;; else
  (when (string-match "^xterm" (getenv "TERM")) ;; Allow all sorts of modified function keys and other odd keys when running emacs with the -nw option
    (require 'xterm-extras)
    (xterm-extra-keys))
  )

;; Auto install lib
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/auto-install/"))

;; OS X and Win modifier keys bindings
(cond
 ((eq system-type 'darwin)
  ;(setq mac-pass-command-to-system nil) ; Disable OS commands by modifier keys
  ;(set-keyboard-coding-system nil)
  ;(setq ns-alternate-modifier 'super)
  ;(setq ns-right-alternate-modifier 'none)
  ;(setq ns-command-modifier 'meta)
  ;(setq ns-right-command-modifier 'left)
  ;(setq ns-control-modifier 'control)
  ;(setq ns-right-control-modifier 'left)
  ;(setq ns-function-modifier 'hyper)
  (setq mac-command-modifier 'meta) ; sets the Command key to Meta
  (setq mac-control-modifier 'control) ; sets the Control key to Control
  (setq ns-function-modifier 'hyper) ; set Mac's Fn key to Hyper
  (setq mac-option-modifier 'super)) ; sets the Option key to Super
 ((eq system-type 'windows-nt)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (setq w32-rwindow-modifier 'super)))

;; Set font
;; (set-frame-font "Menlo:pixelsize=16")
(when (member "Menlo" (font-family-list))
  (set-face-attribute 'default nil :font "Menlo:pixelsize=16"))

;; Navigation
;; (global-hl-line-mode 1) ; highlight current line

;; Set font if emacs running in daemon mode
;; (add-to-list 'default-frame-alist
;; (cons 'font "Menlo:pixelsize=16"))

;; Emacs window size on start
;; (add-to-list 'default-frame-alist '(left . 0))
;; (add-to-list 'default-frame-alist '(top . 0))
;; (add-to-list 'default-frame-alist '(height . 50))
;; (add-to-list 'default-frame-alist '(width . 180))

;; Display the name of the current buffer in the title bar
(setq frame-title-format "GNU Emacs: %b")

;; Org mode settings
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-files (list "~/.emacs.d/org/work.org"
                             "~/.emacs.d/org/home.org"))
(setq org-log-done t)
(setq org-support-shift-select t)
(setq org-todo-keywords
      ;; Change of these options requires full emacs restart
      ;; With cycle type
      ;; '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED")))
      ;; With key tagged
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" "CANCELED(c)")))

;; Disable GUI components
(tooltip-mode      -1)
(menu-bar-mode     -1) ;; Disable graphical menu
(tool-bar-mode     -1) ;; disable tool-bar
(scroll-bar-mode   -1) ;; Disable scroll-line
(blink-cursor-mode -1) ;; Disable cursor flashing
(setq use-dialog-box nil) ;; No graphic dialogs and windows
(setq redisplay-dont-pause t)  ;; Better buffer renrering
(setq ring-bell-function 'ignore) ;; Disable sound signals

;; Disable Git backend to speed up sshfs file load among other things
;; Bypass tramp vc-registered errors (hangings on remote volume editing)
;; The default list is (RCS CVS SVN SCCS Bzr Git Hg Mtn Arch)
;; (setq vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Hg Mtn Arch)))

;; To disable all vc checking
;; (setq vc-handled-backends nil)

;; Disable tramp version control to avoid delays
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; Nightmare mode (disables arrow keys)
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

;; Resize splitted windows binds
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; Enable mouse in no-window mode
(require 'mouse)
(xterm-mouse-mode t)
(mouse-wheel-mode t)

;; Mouse settings
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Tramp bindings
(cond
 ((eq system-type 'darwin)
  (global-set-key (kbd "s-\S-c") (lambda () (interactive) 'tramp-cleanup-all-connections))
  (global-set-key (kbd "s-\S-l") (lambda () (interactive) 'tramp-cleanup-this-connection)))
 (t
  (global-set-key (kbd "s-\S-c") 'tramp-cleanup-all-connections)
  (global-set-key (kbd "s-\S-l") 'tramp-cleanup-this-connection)))

;; Ido mode settings
(require 'ido)
(ido-mode t)
(icomplete-mode t)
(ido-everywhere t)
(setq ido-vitrual-buffers t)
(setq ido-enable-flex-matching t)
(setq ido-file-extensions-order '(".org" ".txt" ".py" ".pl" ".pm" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf" ".conf" ".groovy"))
(eval-after-load 'auto-complete '(global-auto-complete-mode t))

;; Iswitch
;; (iswitchb-mode 1)

;; Imenu autocomplete
(require 'imenu)
(setq imenu-auto-rescan      t) ;; auto update list of elisp functions
(setq imenu-use-popup-menu nil) ;; imenu dialogs only in mini-buffer
;;(global-set-key (kbd "<f6>") 'imenu) ;; call imenu by F6

;; Linum mode
(require 'linum)
(add-hook 'nlinum-mode-hook
          (lambda ()
            (setq nlinum--width
                  (length (number-to-string
                           (count-lines (point-min) (point-max)))))))
(global-linum-mode)

;; CPerl mode settings
(defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-level 4
          cperl-close-paren-offset -4
          cperl-continued-statement-offset 4
          cperl-indent-parens-as-block t
          cperl-tab-always-indent t)

(setq cperl-indent-subs-specially nil)
(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))

(setq cperl-extra-newline-before-brace nil
      cperl-brace-offset -4
      cperl-merge-trailing-else nil)

;; Packages repo settings
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; To update package lists: M-x package-refresh-contents RET

;; List the packages to install
(setq package-list '(
                     auto-complete
                     anything
                     json-reformat
                     magit
                     ;; helm
                     ;; helm-swoop
                     ))
;; Activate installed packages
(package-initialize)
;; Fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Auto complete mode
(add-hook 'cperl-mode-hook
          (lambda ()
            (when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
              (auto-complete-mode t)
              (make-variable-buffer-local 'ac-sources)
              (setq ac-sources
                    '(ac-source-perl-completion)))))

;; CPerl advanced auto complete
(add-hook 'cperl-mode-hook
          (lambda()
            (when (require 'perl-completion nil t)
              (perl-completion-mode t))))

;; Groovy settings
(add-hook 'groovy-mode-hook
          (lambda()
            (auto-complete t)
            (setq tab-width 4)))

(setq nxml-child-indent 4
      nxml-attribute-indent 4
      nxml-slash-auto-complete-flag t)

;; Tidy settings
(setq whitespace-line 0)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit rust-mode lua-mode json-reformat javap-mode anything auto-complete php-mode yaml-mode tt-mode tabbar spacegray-theme perl-completion nlinum neotree multiple-cursors kolon-mode json-mode groovy-mode goto-last-change go-mode ensime edts))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Hilight parent brackets
(show-paren-mode 1)

;; (add-to-list 'auto-mode-alist '("\\.jython\\'" . python-mode))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; Some custom useful keybindings
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "M-/") 'comment-line) ;; emacs25+
(global-set-key (kbd "C-M-/") 'comment-region)
(global-set-key (kbd "C-M-?") 'uncomment-region)
(global-set-key (kbd "s-r") 'revert-buffer)
(global-set-key (kbd "s-.") 'xah-new-empty-buffer)
(global-set-key (kbd "s-j") 'json-reformat-region)
(global-set-key (kbd "s-g") 'goto-percent)
(global-set-key (kbd "s-b m") 'rename-file-and-buffer)
(global-set-key (kbd "s-k m") 'kill-matching-buffers-just-do-it)
(global-set-key (kbd "s-d u") 'dos2unix)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-x C-r") 'sudo-edit)
(global-set-key (kbd "C-c D") 'delete-file-and-buffer)
(global-set-key (kbd "C-%") 'goto-match-paren)
(global-set-key (kbd "C-x %") 'forward-or-backward-sexp)

;; helm bindings (Helm disabled becase of low productivity of helm-swoop on large files)
;; (require 'helm)
;; (require 'helm-config)
;; (require 'helm-swoop)

;; (with-eval-after-load 'helm
  ;; (define-key helm-map (kbd "C-c p") 'ignore)
  ;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  ;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  ;; (define-key helm-find-files-map (kbd "C-<backspace>") 'helm-find-files-up-one-level)
  ;; (define-key helm-map (kbd "C-z")  'helm-select-action))

;; (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
;; (global-set-key (kbd "C-x r b") 'helm-bookmarks)
;; (global-set-key (kbd "C-x m") 'helm-M-x)
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files) ;; Leave commented if ido is preferable
;; (global-set-key (kbd "M-i") 'helm-swoop)
;; (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;; (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

;; Include latex configuration
(load-file "~/.emacs.d/lisp/latex.el")

;; Comment tags settings
(load-file "~/.emacs.d/lisp/comment-tags.el")
(autoload 'comment-tags-mode "comment-tags-mode")
(setq comment-tags-keymap-prefix (kbd "C-c t"))
(with-eval-after-load "comment-tags"
  (setq comment-tags-keyword-faces
        `(("TODO" . ,(list :weight 'bold :foreground "#28ABE3"))
          ("TBD" . ,(list :weight 'bold :foreground "#28ABE3"))
          ("FIXME" . ,(list :weight 'bold :foreground "#DB3340"))
          ("BUG" . ,(list :weight 'bold :foreground "#DB3340"))
          ("HACK" . ,(list :weight 'bold :foreground "#E8B71A"))
          ("KLUDGE" . ,(list :weight 'bold :foreground "#E8B71A"))
          ("XXX" . ,(list :weight 'bold :foreground "#F7EAC8"))
          ("INFO" . ,(list :weight 'bold :foreground "#F7EAC8"))
          ("DONE" . ,(list :weight 'bold :foreground "#1FDA9A"))))
  (setq comment-tags-comment-start-only t
        comment-tags-require-colon t
        comment-tags-case-sensitive t
        comment-tags-show-faces t
        comment-tags-lighter nil))
(add-hook 'prog-mode-hook 'comment-tags-mode)

;; Auto conf mode for Dockerfile
(add-to-list 'auto-mode-alist '("Dockerfile" . conf-mode))

;; Spell checking settings
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
;; (dolist (hook '(text-mode-hook))
  ;; (add-hook hook (lambda () (flyspell-mode 1))))
;; (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  ;; (add-hook hook (lambda () (flyspell-mode -1))))

;; Functions defun below

;; Create empty buffer
(defun xah-new-empty-buffer ()
  "Open a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    ;; (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

;; ansi-term line and char modes
;; In the line mode ansi term buffer acts more like a normal text-buffer
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

;; Goto specified percent of buffer
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

;; Kill matching buffers with no ask
(defun kill-matching-buffers-just-do-it ()
  "Kill buffers whose names match REGEXP, without asking."
  (interactive)
  (cl-letf (((symbol-function 'kill-buffer-ask) #'kill-buffer))
    (call-interactively #'kill-matching-buffers)))

;; Sudo edit file or reload current buffer with sudo (set to C-x C-r)
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

;; As it says...
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
              (message "Deleted file %s" filename)
              (kill-buffer)))))))

;; This code from http://emacro.sourceforge.net/ gives a vi-like way of moving over parenthesis groups. I bind it to C-%, from vi heritage
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; This modification of the above code works when the point is either right before or right after a parenthesis character, and also works with shift-selection
(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))
