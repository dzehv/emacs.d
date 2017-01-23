;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; (require 'server)
;; (and (>= emacs-major-version 23)
;;     (defun server-ensure-safe-dir (dir) "Noop" t))

;; Initializing emacs server
;; (server-start)
;; (setq server-socket-dir "~/.emacs.d/server")
;; (setq server-name "server")

;; Initializing emacs server using TCP
;; (require 'server)
;; (setq server-use-tcp t
;;       server-socket-dir "~/.emacs.d/server")
;; (unless (server-running-p)
;;   (server-start))

;; Initializing emacs server using port
(require 'server)
(setq server-name "server" ; name of the server
      server-host "localhost" ; server ip
      server-socket-dir "~/.emacs.d/server"
      server-use-tcp nil
      server-port 9999)
(server-start) ; comment out when using --daemon

;; ####
;; ~/.ssh/config (e.g. connection params)
;; Host <host_name>
;;   HostName <ip>
;;   User <user_name>
;;   ControlMaster auto
;;   ControlPath ~/.ssh/<host_name>.sock
;;   RemoteForward 9999 localhost:9999
;; ####

;; Enable mouse (no-window mode)
(require 'mouse)
(xterm-mouse-mode t)
(mouse-wheel-mode t)

;; OS X and Windows modifier keys bindings
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

;; Emacs window size on start v1
;; (add-to-list 'default-frame-alist '(height . 40))
;; (add-to-list 'default-frame-alist '(width . 170))

;; Emacs window size on start v2
; (when (window-system) (set-frame-size (selected-frame) 190 50))

;; Emacs window size on start v3
(add-to-list 'default-frame-alist '(left . 0))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 170))

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

(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Some useful keybindings
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "M-/") 'comment-line) ;; emacs25+
(global-set-key (kbd "C-M-/") 'comment-region)
(global-set-key (kbd "C-M-?") 'uncomment-region)
(global-set-key (kbd "s-r") 'revert-buffer)

;; Tramp bindings
(cond
 ((eq system-type 'darwin)
  (global-set-key (kbd "s-\S-c") (lambda () (interactive) 'tramp-cleanup-all-connections))
  (global-set-key (kbd "s-\S-l") (lambda () (interactive) 'tramp-cleanup-this-connection)))
 (t
  (global-set-key (kbd "s-\S-c") 'tramp-cleanup-all-connections)
  (global-set-key (kbd "s-\S-l") 'tramp-cleanup-this-connection)))

;; (add-to-list 'load-path "~/.emacs.d/lisp/")

;; ansi-term yank
(defun my-term-mode-hook ()
  (define-key term-raw-map (kbd "C-y") 'term-paste)
  (define-key term-raw-map (kbd "C-k")
    (lambda ()
      (interactive)
      (term-send-raw-string "\C-k")
      (kill-line))))
(add-hook 'term-mode-hook 'my-term-mode-hook)

;; (add-to-list 'load-path "/path/to/edit-server.el/")
;; (require 'edit-server)
;; (edit-server-start)

;; (add-to-list 'load-path  "/usr/local/lib/erlang/lib/tools-2.8.4/emacs")
;; (add-to-list 'load-path  "/usr/local/lib/erlang/lib/tools-2.7.2/emacs")
;; (add-to-list 'load-path "~/.emacs.d/gitlisp/sublimity")
;; (add-to-list 'load-path "~/.emacs.d/elpa/spacegray-theme-20131230.1127")

(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))

;; (global-set-key (kbd "s-/") 'comment-dwim)
;; (global-set-key (kbd "C-z") 'undo)
;; (global-set-key (kbd "C-M-z") 'whitespace-mode)
;; (setq erlang-root-dir "/usr/local/lib/erlang")
;; (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))

(show-paren-mode 1)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
  )
(defun xah-new-empty-buffer ()
  "Open a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(global-set-key (kbd "s-.") 'xah-new-empty-buffer)
(require 'linum)
;; (require 'sublimity)
;; (sublimity-mode)
;; (require 'perl-completion)
;; (require 'erlang-start)
(require 'ido)
(ido-mode t)
(icomplete-mode                t)
(ido-everywhere                t)
(setq ido-vitrual-buffers      t)
(setq ido-enable-flex-matching t)
(setq ido-file-extensions-order '(".org" ".txt" ".py" ".pl" ".pm" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf" ".groovy"))
(global-linum-mode)
(eval-after-load 'auto-complete '(global-auto-complete-mode t))
;; (eval-after-load 'auto-complete '(add-to-list 'ac-modes 'erlang-mode))
;; (eval-after-load 'spacegray-theme '(load-theme 'spacegray t))
;; Imenu
(require 'imenu)
(setq imenu-auto-rescan      t) ;; auto update list of elisp functions
(setq imenu-use-popup-menu nil) ;; imenu dialogs only in mini-buffer
;;(global-set-key (kbd "<f6>") 'imenu) ;; call imenu by F6

(add-hook 'nlinum-mode-hook
          (lambda ()
            (setq nlinum--width
              (length (number-to-string
                       (count-lines (point-min) (point-max)))))))

;;(add-to-list 'ac-modes 'erlang-mode)

(defalias 'perl-mode 'cperl-mode)

;; (load-library "cperl-mode")
;; (defun cperl-backward-to-start-of-continued-exp (lim)
;;   (if (memq (preceding-char) (append ")]}\"'`" nil))
;;       (forward-sexp -1))
;;   (beginning-of-line)
;;   (if (or (<= (point) lim) (< 0 cperl-continued-statement-offset))
;;       (goto-char (1+ lim)))
;;   (skip-chars-forward " \t"))
;; (setq cperl-indent-level 4)

(setq cperl-indent-level 4
          cperl-close-paren-offset -4
          cperl-continued-statement-offset 4
          cperl-indent-parens-as-block t
          cperl-tab-always-indent t)

(setq cperl-indent-subs-specially nil)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;;load cperl, then work around indent issue
;;;; (load-library "cperl-mode")
;;;; (defun cperl-backward-to-start-of-continued-exp (lim)
;;;;   (if (memq (preceding-char) (append ")]}\"'`" nil))
;;;;       (forward-sexp -1))
;;;;   (beginning-of-line)
;;;;   (if (or (<= (point) lim) (< 0 cperl-continued-statement-offset))
;;;;       (goto-char (1+ lim)))
;;;;   (skip-chars-forward " \t"))

;; (add-hook 'cperl-mode-hook
;;	  (lambda()
;;	    (require 'perl-completion)
;;	    (auto-complete t)
;;	    (custom-set-variables
;;	     '(cperl-indent-parens-as-block t))
;;	    (perl-completion-mode t)))

;; (add-hook 'erlang-mode-hook
;;	  (lambda()
;; 	    (auto-complete t)))

(add-hook 'groovy-mode-hook
	  (lambda()
 	    (auto-complete t)
        (setq tab-width 4)))
(setq 
    nxml-child-indent 4
    nxml-attribute-indent 4
    nxml-slash-auto-complete-flag t)
;; (defun spacegray-theme-init ()
;;   (load-theme 'spacegray)
;; )

;; (add-hook 'after-init-hook 'spacegray-theme-init)
;; (add-hook 'after-init-hook 'tabbar-mode 1)



(setq whitespace-line 0)

(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode tt-mode tabbar spacegray-theme perl-completion nlinum neotree multiple-cursors kolon-mode json-mode groovy-mode goto-last-change go-mode ensime edts))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (add-to-list 'auto-mode-alist '("\\.jython\\'" . python-mode))
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
