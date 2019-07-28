;; About Emacs lisp

;; lisp -- List processing language
;; emacs lisp -- one of lisp dialect for emacs to process text
;; all expressions can be executes with C-x C-e (eval) as region

;; There are a number of ways to execute (evaluate, in Lisp lingo) an Emacs Lisp form:

;; If you want it evaluated every time you run Emacs, put it in a file named .emacs in your home directory. This is known as “your .emacs file,” and contains all of your personal customizations.
;; You can type the form in the *scratch* buffer, and then type <LFD> (or C-j) after it. The result of evaluating the form will be inserted in the buffer.
;; In emacs-lisp-mode, typing C-M-x evaluates a top-level form before or around point.
;; Typing C-x C-e in any buffer evaluates the Lisp form immediately before point and prints its value in the echo area.
;; Typing M-: or M-x eval-expression allows you to type a Lisp form in the minibuffer which will be evaluated once you press <RET>.
;; You can use M-x load-file to have Emacs evaluate all the Lisp forms in a file. (To do this from Lisp use the function load instead.)
;; The functions load-library, eval-region, eval-buffer, require, and autoload are also useful; see Emacs Lisp documentation, if you want to learn more about them.
;; e.g
(print "Hello!") ;; <-- C-x C-e before comment

;; conventions and coding style

(defvar *some-global-var* 1) ;; global vars should have names begin from '*'

;; Data types

;; t -- true
;; nil, '() -- false

;; 1, -2, 3,0 -- numbers
;; a?, ?b, ?\n -- characters
;; "abc" -- strings
;; 'abc -- symbol

(cons 1 2) ;; cons pair/point pair
(cons 1 (cons 2 (cons 3 nil))) ;; create list from cons pairs
'(1 2 3) ;; list

;; ' <-- before object means object itself, not elisp function call
(list 1 2 3) ;; same list creation ( element access speed O(n) )
(list 1 "abc" ?a) ;; list with different element's types
(car (list 1 2 3)) ;; car gets first element in list (head)
(cdr (list 1 2 3)) ;; cdr get all except first element (tail)

;; names 'car'  and 'cdr' were got from Symbolics company lisp machines registers

[1 2 3] ;; array ( element access spped O(const) )

;; point -- where cursor is places in buffer (obj)
;; mark -- C-<SPACE> is set to mark beginnig of region (select operation)
;; region -- selected area obj
;; buffer -- emacs universal objects container (displayed content)

;; basic language constructs

(message "Hello!") ;; send string to minibuf, returns same string as ret value
'(message "Hello!");; list of two elements: word and string

;; define function
(defun my-fun1 ()
  (message "Hello!"))

(my-fun1) ;; call our function

;; callable fun interactively from Emacs
;; (interactive "sSome string: ") -- get arg 'any string' from user with text...
(defun my-fun2 ()
  (interactive) ;; make fun callable by M-x
  (message "Hello!"))

;; if statement
(if (>= 2 1)
    (message "then")
  (message "else"))

(defvar a 17) ;; set 1st value for variable (not mandatory)
;; a -- see var val (evaluate)
;; 'a -- just symbol 'a'

(setq a 18) ;; set new (quoted first arg) value for a
;; to use single 'set' expr will be:
(set 'a 18) ;; a is not quoted and used as symbol

;; let (define local variable)
(let ((x 3))
  (print x))

;; let body in function
(defun add-using-let ()
  (let ((x 3) (y 4)) ; creating variables x and y
    (+ x y)))

(add-using-let)

;; override local define (we can use previously defined 'x' in define 'z' in let args)
(let* ((x 3) (z (+ x 10)))
  (message (number-to-string z)))

;; create side effects (execute few things)
(if (< 2 1)
    (message "then")
  (progn
    (message "Hello!") ;; see *Messages* buffer
    (message "else"))) ;; than "else" will appear in minibuf, log in *Messages*

;; cycle e.g. returns nil, prints 0 - 9
(let ((i 0))
  (while (< i 10)
    (message (number-to-string i))
    (setq i (1+ i))))

;; Some functions

(point) ;; return buffer's symbol number where cursor is
(search-forward "point") ;; search text forward (returns point number)
(search-backward "point") ;; search text forward (same, but backward)
(evenp 100) ;; check number is even, returns bool
(oddp 11) ;; check number is odd, returns bool

;; functions, that ends on 'p' are predicate (bool functions)

(insert "Hello!") ;; insert string after cursor

;; operations with cursor movings, save position
(defvar *cur-style-num* 0)
(defun stylize-list ()
  (interactive)
  (save-excursion ;; will save cursor position after func exec
    (search-forward "li")
    (if (evenp *cur-style-num*)
        (insert " class=\"even-class\"")
      (insert " class=\"odd-class\""))
    (setq *cur-style-num* (1+ *cur-style-num*))))

(global-set-key (kbd "<f9>") 'stylize-list)

;; Core Elisp

'(1 2 3 4 5) ;; list, ok
(length '(1 2 3 4 5)) ;; get list length
(nth 1 '(1 2 3 4 5)) ;; get 'nth' elem of list
(nthcdr 2 '(1 2 3 4 5)) ;; without two fist elements
(last '(1 2 3 4 5)) ;; last elem of list in list context '(5)
(car (last '(1 2 3 4 5))) ;; last elem of list as ret val
(butlast '(1 2 3 4 5)) ;; everything except last elem of list

(listp '(1 2 3 4 5)) ;; check arg is list
(listp 1) ;; nil
(listp nil) ;; nil equals '() so true
(listp '()) ;; true

(atom 1) ;; is obj indivisible
(atom '(1 2 3 4 5)) ;; nil
(atom nil) ;; true
(atom '()) ;; true, emoty list is as 1 val

(null 1) ;; is arg nil
(null nil) ;; true

;; anonymous function (define and throw args)
((lambda (a b)
   (+ a b)) 1 2) ;; returns 3

;; closure e.g. with anon fun
(setq a 10)
;; a is free var, b - closured
(setq f1 (lambda (b)
           (+ a b)))

;; call fun by symbol with params
(funcall f1 1) ;; returns 11
(funcall '+ 1 2 3) ;; rets 6

(let ((a 20))
  (funcall f1 1)) ;; returns 21 because is not lexical closure

;; lexical closure e.g.
(setq lexical-binding t) ;; setting this var to true enables some rules to defun for this buffer, nil by default

;; call again
(let ((a 20))
  (funcall f1 1)) ;; returns 11 again because remembers it's free variables (a) before defun

;; alternative way to set lexical binding
;; Write in file header:
;; ####################################
;; # ;;;  -*- lexical-binding: t -*-  #
;; ####################################

;; will remeber it until we'll change it in the function

;; funcall and apply
(funcall '+ 1 2 3) ;; rets 6
(apply '+ '(1 2 3)) ;; rets 6 with arg as list

;; apply function for earch element in list
(mapcar (lambda (a) (1+ a)) ;; 1st arg is anon fun or closure with one arg
        '(1 2 3 4 5))

;; reduce...
(reduce
 (lambda (m o) (concat m " " o))
 '("foo" "bar" "baz"))

;; reduce acts:
;; ""
;; "" "foo"
;; "foo" "bar"
;; "foo bar" "baz"
;; "foo bar baz"

;; Buffers modes

;; Global mode                       global-map      (global-set-key)
;; Major-mode (only one for buffer)  local-map       ()
;; Minor-mode (a lot of ones 4 buf)  minor-mode-maps (additional functionality for major mode)

(defun my-hello-func ()
  (interactive)
  (message "Hello, world!"))

;; bind custom func to current major mode
(defun my-keybindings ()
  (interactive)
  (let ((my-key-map current-local-map)) ;; ret local map of major mode
    (local-unset-key (kbd "<f9>"))
    (define-key my-key-map (kbd "<f9>") 'my-hello-func)))

;; add hook for specified mode 4 above function (activate f9 call of func for web-mode only)
(add-hook 'web-mode-hook 'my-keybindings)

;; wrapper e.g. split region by "\n" and save kill region, insert new modified strings
(defun my-wrapper ()
  (interactive)
  (let* ((txt (buffer-substring (mark) (point)))
         (lines (split-string txt "\n")))
    (message (car (cdr lines)))
    (kill-region (mark) (point))
    (insert
     ;; as mapcar but concatenate results 2 one str
     (mapconcat (lambda (s) (concat "<li>" s "</li>")
                  lines ;; second arg
                  "\n"))))) ;; third -- separator

;; now we can bind it to modes as with my-keybindings function to web-mode-hook
