;; Emacs main configuration file

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(require 'server)
(setq server-name "server" ; name of the server
      server-host "localhost" ; server ip
      server-socket-dir "~/.emacs.d/server"
      server-use-tcp nil
      server-port 9999)
(server-start) ; comment out when using 'emacs --daemon'

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
         (let ((load-path  (copy-sequence load-path))) ;; Shadow
           (append 
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

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

;; Emacs window size on start
(add-to-list 'default-frame-alist '(left . 0))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 180))

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
(setq vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Hg Mtn Arch)))

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

;; Enable mouse in no-window mode
(require 'mouse)
(xterm-mouse-mode t)
(mouse-wheel-mode t)

;; Mouse settings
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

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
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; List the packages to install
(setq package-list '(auto-complete anything))
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
(load "perl-completion") ; To be placed in ~/.emacs.d/lisp/perl-completion.el
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
    (auto-complete anything iedit php-mode yaml-mode tt-mode tabbar spacegray-theme perl-completion nlinum neotree multiple-cursors kolon-mode json-mode groovy-mode goto-last-change go-mode ensime edts))))
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

;; Functions defun

;; Create empty buffer
(defun xah-new-empty-buffer ()
  "Open a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

;; ansi-term yank
(defun my-term-mode-hook ()
  (define-key term-raw-map (kbd "C-y") 'term-paste)
  (define-key term-raw-map (kbd "C-k")
    (lambda ()
      (interactive)
      (term-send-raw-string "\C-k")
      (kill-line))))
(add-hook 'term-mode-hook 'my-term-mode-hook)
