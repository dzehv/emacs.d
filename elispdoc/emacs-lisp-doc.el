;; About Emacs lisp

;; lisp -- List processing language
;; emacs lisp -- one of lisp dialect for emacs to process text
;; all expressions can be executes with C-x C-e (eval) as region



;; Emacs Lisp debugging

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

;; to execute strings with C-j we need to activate: M-x lisp-interaction-mode
;; buffer *scratch* is activated with List Interaction major mode by default
(print "Hello!") ;; <-- C-j in Lisp Interaction
;; to execute elisp file as script, we also can interpret it with: emacs --script <file.el>

;; litable.el (can be found on github) plugin implements immediate eval result while typing elisp code in buffer
;; *Backtrace* buffer is called automatically on errors
;; we can add breakpoints in code to enter debugger, as following

(let ((a 10))
  (while (> a 0)
    (debug) ;; breakpoint - stop eval here and enter debugger
    (message (int-to-string a))
    (setq a (1- a))))
;; while breakpoint is activated in *Backtrace* buffer we can play with it
;; e - evaluate in *Backtrace* buf, c - continue
;; there will be 10 breakpoints because of while loop
;; d - step into (trace by commands)

;; another one method to call breakpoint on enry with
;; M-x debug-on-entry -- <FUNC_NAME> <RET>

;; M-x eval-defun (C-M-x) - Evaluate the top-level form containing point, or after point
;; as (M-x eval-defun) there is (M-x edebug-defun) debugger
;; when called on region and then executed by C-x C-e
;; ? - see keybindings of edebug
;; <SPC> - next step
;; i - step into
;; f - step forward
;; e - eval
;; b - set breakpoint
;; g - go till breakpoint



;; Elisp REPL

;; M-x ielm
;; M-x eshell -- is also a REPL and can eval elisp expressions as terminal commands

;; Conventions and coding style

(defvar *some-global-var* 1) ;; global vars should have names begin from '*'



;; Elisp data types

;; t -- true
;; nil, '() -- false

;; 1, -2, 3,0 -- numbers
;; a?, ?b, ?\n -- characters
;; "abc" -- strings
;; 'abc -- symbol
;; #'abc -- 'abc equivalent, is preferred when (abc) is a function...
;; ...as it documents the fact that it is intended to be funcalled. (funcall abc ...)

;; func arguments list
;; (defun my-fun (a b &optional c d &rest e)) ; mandatory, optional, rest of args in list

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
;; mark -- C-<SPC> is set to mark beginnig of region (select operation)
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

(defun my-fun3 (name)
  (interactive "sType your name: ") ;; callable with string arg
  (message "Hello, %s!" name))

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

;; loop e.g. returns nil, prints 0 - 9
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



;; Buffer modes

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



;; Libs

(add-to-list 'load-path "~/.emacs.d/lisp/") ;; add path to search lisp files there (load-path ~ @INC for Perl)
;; e.g. we have file '~/.emacs.d/lisp/test.el'
;; after adding to list we can call (require 'test)


;; Macroses, classes, CEDET package

;; marcoses -- compile time functions, which are replace like C define or constants
(defmacro macro1 (name)
  name)

(defmacro macro2 (name)
  '(name))

(defmacro macro3 (name)
  `(,name))

(defmacro macro4 (name)
  `(,@name))

;; see what macro replaces
(macroexpand '(macro2 123)) ;; returns -- (macro2 123)
(macroexpand '(macro3 123)) ;; returns -- (123)
(macroexpand '(macro4 123)) ;; returns -- 123

;; define vars example with macro
(defmacro macro5 (&rest body)
  (let ((res nil))
    (dolist (elem body)
      (setq res (cons (list 'setq (nth 1 elem) (nth 0 elem))
                      res)))
    `(progn ,@res)))

(macro5 (1 a) (2 b)) ;; -- defvars with macro5
a ;; -- 1
b ;; -- 2

;; #123456789012345678901234567890123456789012345678901234567890
;; SVCLFOWLER         10101MS0120050313.........................
;; SVCLHOHPE          10201DX0320050315........................
;; SVCLTWO           x10301MRP220050329..............................
;; USGE10301TWO          x50214..7050329...............................

;; mapping SVCL dsl.ServiceCall
;;   4-18: CustomerName
;;   19-23: CustomerID
;;   24-27 : CallTypeCode
;;   28-35 : DateOfCallString

;; mapping  USGE dsl.Usage
;;   4-8 : CustomerID
;;   9-22: CustomerName
;;   30-30: Cycle
;;   31-36: ReadDate

(defclass ServiceCall ()
  ((Code :initarg :Code :initform "SVCL")
   (CustomerName :initarg :CustomerName)
   (CustomerID :initarg :CustomerID)
   (CallTypeCode :initarg :CallTypeCode)
   (DateOfCallString :initarg :DateOfCallString)))

(defmethod parse-line-for-class ((obj ServiceCall) line)
  (oset obj CustomerName (substring line 4 18))
  (oset obj CustomerID (substring line 19 23))
  (oset obj CallTypeCode (substring line 24 27))
  (oset obj DateOfCallString (substring line 28 35)))

(defmacro defmappings (class-name code &rest body)
  `(progn
     (defclass ,class-name ()
       ,(let ((res (list (list 'Code :initarg :Code :initform code))))
          (dolist (elem body)
            (setq res (cons (list (nth 2 elem)
                                  :initarg
                                  (make-symbol (concat ":" (symbol-name (nth 2 elem)))))
                            res)))
          res))

     (defmethod parse-line-for-class ((obj ,class-name) line)
       ,@(let ((res nil))
           (dolist (elem body)
             (setq res (cons (list 'oset 'obj (nth 2 elem)
                                   (list 'substring 'line (nth 0 elem) (nth 1 elem)))
                             res)))
           res))))

(defmappings ServiceCall "SVCL"
  (4 18 CustomerName)
  (19 23 CustomerID)
  (24 27 CallTypeCode)
  (28 35 DateOfCallString))

(defmappings Usage "USGE"
  (4 8 CustomerID)
  (9 22 CustomerName)
  (30 31 Cycle)
  (32 36 ReadDate))

(let ((line "USGE123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890")
      (o nil))
  (cond
   ((string= "SVCL" (substring line 0 4)) (setq o (ServiceCall "1")))
   ((string= "USGE" (substring line 0 4)) (setq o (Usage "2"))))
  (parse-line-for-class o line)
  o)
