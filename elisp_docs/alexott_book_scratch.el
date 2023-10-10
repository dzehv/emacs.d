;; alexott book examples (Emacs 18, 19)
;; https://alexott.net/en/index.html

;; Below examples mostly demonstrate elisp stdlib
;; Not a complete list of examples, but the most basic.


;; 'symbol - quote and use symbol as it is (do not eval)
;; #'function - same quoting, but for functions

;; - comments
;;; - no indentation comments

;; basic operations
(+ 2 2)
fill-column
(concat  (+ 2 fill-column) " red foxes")
(+ 2 2.23)
(set 'var '(1 2 3))
(setq var '(1 2 3))
(current-buffer)
(other-buffer)
(buffer-size)
(point)

;; car, cdr, setcar, setcdr, nthcdr
(car '(1 2 3 4))
(cdr '(1 2 3 4))
(cons '1 '(2 3 4))
(length '(1 2 3 4))
(nthcdr 2 '(1 2 3 4))
(setq numlist '(5 2 3 4))
numlist ;; (5 2 3 4)
(setcar numlist '1)
numlist ;; (1 2 3 4)
(setcar numlist '5)
(setcdr numlist '(6 7 8))
numlist ;; (5 6 7 8)

(min 3 4 6 5 7 3)
(max 3 4 6 5 7 3)

;; apply args to func in different forms
(apply 'max 3 4 7 3 '(4 8 5))
;; or just like this
(apply 'max '(4 8 5))

(sort '(4 8 21 17 33 7 21 7) '<)

;; define variable with documentation
;; instead of set/q assigns only if new var was not defined yet
(defvar new-test-var 10 "Var documentation string.")

;; non-interactive funcs usage
(insert-rectangle '("first" "second" "third"))


(defun multiply-by-seven (number)
  "Multiply NUMBER by seven."
  (interactive "p")
  (message (* 7 number)))


(defun test-print-number (number)
  "Prints coming variable"
  (interactive "p")
  (message number))

(test-print-number 10)

;; conditions
(if 4
    'true
  'false)

(if nil
    'true
  'false)

(let ((foo (buffer-name))
      (bar (buffer-size)))
  (message
   "This buffer %s has %d characters"
   foo bar))

(message "We're at the range of %d from the beginning of buffer"
         (- (point)
            (save-excursion
              (goto-char (point-min)) (point))))

(if (string= (int-to-string 19)
             (substring (emacs-version) 10 12))
    (message "This is Emacs 19")
  (message "This is not Emacs 19"))


(defun simplified-beginning-of-buffer ()
  "Move the cursor to the beginning of the buffer,
by placing a mark at the previous cursor position."
  (interactive)
  (push-mark)
  (goto-char (point-min)))


(defun mark-whole-buffer ()
  "Place a dot at the beginning and a mark at the end of the buffer."
  (interactive)
  (push-mark (point))
  (push-mark (point-max))
  (goto-char (point-min)))


(defun append-to-buffer (buffer start end)
  "Append text in the region to the specified buffer.
The text is inserted into the buffer before the period.

When called, it takes three arguments:
buffer or its name, and two numbers specifying the region in
current buffer."
  (interactive "BAppend to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (insert-buffer-substring oldbuf start end))))


(defun append-to-buffer (buffer start end)
  "Insert into buffer..."
  (interactive "BAppend to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (insert-buffer-substring oldbuf start end))))


;; copy-to-buffer
(defun copy-to-buffer (buffer start end)
  "..."
  (interactive "BCopy to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (erase-buffer)
      (save-excursion
        (insert-buffer-substring oldbuf start end)))))


(defun insert-buffer (buffer)
  "Insert the contents of BUFFER after the period.
Place a checkmark after the inserted text.
BUFFER can be a buffer or the name of a buffer."
  (interactive "*bInsert buffer: ")
  (or (bufferp buffer)
      (setq buffer (get-buffer buffer)))
  (let (start end newmark)
    (save-excursion
      (save-excursion
        (set-buffer buffer)
        (setq start (point-min) end (point-max)))
      (insert-buffer-substring buffer start end)
      (setq newmark (point)))
    (push-mark newmark)))


(or nil nil 'rock t) ;; will return rock (as first true)

(or nil
    (let ((rock "Hello"))
      (message rock))) ;; if false - assign var and print to minibuf

(and t t nil) ;; nil as not all the args true


(defun beginning-of-buffer (&optional arg)
  "Move the point to the beginning of the buffer;
leaving the mark at the previous cursor position.
With arg N, move point N/10 from start
buffer.
Do not use this function in your programs!
\(goto-char (point-min)) is faster and does not install
label."
  (interactive "P")
  (push-mark)
  (goto-char
   (if arg
       (if (> (buffer-size) 10000)
           ;; Avoid overflow for large buffer sizes!
           (* (prefix-numeric-value arg)
              (/ (buffer-size) 10))
         (/ (+ 10 (* (buffer-size)
                     (prefix-numeric-value arg)))
            10))
     (point-min)))
  (if arg (forward-line 1)))


(defun what-line ()
  "Print the number of the current line in the buffer."
  (interactive)
  (save-restriction ;; save and narrow back if was narrowed
    (widen) ;; expand buffer if was narrowed
    (save-excursion ;; save point and mark
      (beginning-of-line) ;; counts, etc.
      (message "Line %d"
               (1+ (count-lines 1 (point)))))))


;; zap-to-char: Emacs 19 func implementation
(defun zap-to-char-e19 (arg char)
  "Removes up to and including the ARG's appearance of the CHAR
Goes backward if ARG is negative; signals an error if CHAR is not
found"
  (interactive "*p\ncZap to char: ")
  (kill-region (point) ;; start kill point
               (progn
                 (search-forward
                  (char-to-string char) nil nil arg)
                 (point)))) ;; point after search (region between start and end)

;; zap-to-char: Emacs 18 func implementation
(defun zap-to-char-e18 (arg char)
  "Same, but earlier func of Emacs 18"
  (if (search-forward (char-to-string char) nil t arg)
      (progn (goto-char
              (if (> arg 0) (1- (point)) (1+ (point))))
             (point))
    (if (> arg 0)
        (point-max)
      (point-min))))


;; Also Emacs 19 func implementation
(defun copy-region-as-kill (beg end)
  "Save deleted area to kill-ring."
  (interactive "r") ;; means beg and end region area

  (if (eq last-command 'kill-region)
      ;; then part: Add newly deleted text to previous deleted text.
      (kill-append (buffer-substring beg end) (< end beg))

    ;; else-part: Add newly deleted text as a separate element
    ;; to the kill ring and shorten it if necessary.
    (setq kill-ring
          (cons (buffer-substring beg end) kill-ring))
    (if (> (length kill-ring) kill-ring-max)
        (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil)))

  (setq this-command 'kill-region)
  (setq kill-ring-yank-pointer kill-ring))


;; append string to previous killed one
(defun kill-append (string before-p)
  (setcar kill-ring
          (if before-p
              (concat string (car kill-ring))
            (concat (car kill-ring) string))))


;; print elements by while
(setq numlist '(1 2 3 4))

(defun print-elements-of-list (list)
  "Print each LIST element on a separate line."
  (while list
    (print (car list))
    (setq list (cdr list))))

(print-elements-of-list numlist)


;; Version with incrementation
(defun count-stones-number (num-lines)
  "Counts the number of stones in a triangle.
The first line is one stone, the second is two stones,
third line three stones, and so on.
Argument---ROW-NUMBER."
  (let ((result 0)
        (current-line 1))
    (while (<= current-line num-lines)
      (setq result (+ result current-line))
      (setq current-line (1+ current-line)))
    result))


;; Decrement version
(defun count-stones-number (num)
  "Returns the sum of numbers from 1 to NUMBER inclusive."
  (let ((result 0))
    (while (> num 0)
      (setq result (+ result num))
      (setq num (1- num)))
    result))


(setq nums '(1 2 3 4))

;; Recursive func
(defun recursive-print-elements (list)
  "Prints each LIST item on a separate line.
Uses recursion."
  (print (car list))                  ;; body
  (if list                            ;; recursive check
      (recursive-print-elements       ;; recirsive call
       (cdr list))))                  ;; next call expression

(recursive-print-elements nums)


(defun recursive-count-stones-number (num)
  "Returns the sum of numbers from 1 to NUMBER, inclusive.
Uses recursion."
  (if (= num 1)                               ;; рекурсивная-проверка
      1                                       ;; then-часть
    (+ num                                    ;; else-часть
       (recursive-count-stones-number         ;; рекурсивный вызов
        (1- num)))))                          ;; выражение-следующего-вызова


;; Same using cond special form
(defun count-stones-number-cond (num)
  (cond ((<= num 0) 0)
        ((= num 1) 1)
        ((> num 1)
         (+ num (count-stones-number-cond (1- num))))))


;; forward-sentence example from Emacs19
(defun forward-sentence-e19 (&optional arg)
  "Moves the period to the end of the sentence. The argument is number-of-repeat.
When given a negative argument, moves the point to the beginning
offers. The end of a sentence is considered a regular expression
meaning sententce-end. The search also ends when it reaches
end of paragraph."
  (interactive "p")
  (or arg (setq arg 1))
  (while (< arg 0)
    (let ((par-beg
           (save-excursion (start-of-paragraph-text) (point))))
      (if (re-search-backward
           (concat sentence-end "[^ \t\n]") par-beg t)
          (goto-char (1- (match-end 0)))
        (goto-char par-beg)))
    (setq arg (1+ arg)))
  (while (> arg 0)
    (let ((par-end
           (save-excursion (end-of-paragraph-text) (point))))
      (if (re-search-forward sentence-end par-end t)
          (skip-chars-backward " \t\n")
        (goto-char par-end)))
    (setq arg (1- arg))))


;; (global-set-key "\C-c=" 'count-words-region)
;; Final version: while
(defun count-words-region (beginning end)
  "Reports the number of words in the area.
A word is a text in which, although we have one character included in
the composition of the word is followed by at least one character not included in the words.
The current buffer syntax table defines what these should be
symbols."
  (interactive "r")
  (message "Counting words in the region")
  (save-excursion
    (goto-char beginning)
    (let ((count 0))
      (while (and (< (point) end))
        (re-search-forward "\\w+\\W*" end t)
        (setq count (1+ count)))
      (cond ((zerop count)
             (message
              "Region has no words."))
            ((= 1 count)
             (message
              "Region has 1 word."))
            ((and (< 1 count) (> 5 count))
	         (message
              "Region has %d words." count))
            (t
             (message
              "Region has %d words." count))))))


;; recursive version
(defun count-words-region-recursive (beginning end)
  "Prints the number of words in the area.
A word is considered to be text in which at least one character is included in
the composition of the word is followed by at least one character not included in the words.
The current buffer syntax table defines what these should be
symbols."
  (interactive "r")
  ;; 1. Preparations.
  (message "Counting words in region ... ")
  (save-excursion
    (goto-char beginning)
    ;; 2. Count words.
    (let ((count (recursive-count-words end)))
      ;; 3. View results.
      (cond
       ((zerop count)
	(message
	 "Region has no words."))
       ((= 1 count)
	(message
	 "Region has 1 word."))
       ((and (< 1 count) (> 5 count))
	(message
	 "Region has %d words." count))
       (t
	(message
	 "Region has %d words." count))))))

;; recursive count func helper
(defun recursive-count-words (region-end)
  "Documentation..."
  ;; 1. recursive check
  (if (and (< (point) region-end)
           (re-search-forward "\\w+\\W*" region-end t))
      ;; 2. then-part: recurisve call
      (1+ (recursive-count-words region-end))
    ;; 3. else-part
    0))


;; Count words project
(defun count-words-in-defun ()
  "Returns the number of Lisp words and characters in a function."
  (beginning-of-defun)
  (let ((count 0)
        (end (save-excursion (end-of-defun) (point))))
    (while
        (and (< (point) end)
             (re-search-forward
              "\\(\\w\\|\\s_\\)+[^ \t\n]*[ \t\n]*"
              end t))
      (setq count (1+ count)))
    count))


;; Interactive wrapper for count-words-in-defun
(defun count-words-defun ()
  "Number of Lisp words and symbols in a function definition."
  (interactive)
  (message
   "Counting Lisp words and symbols in the function definition...")
  (let ((count (count-words-in-defun)))
    (cond
     ((zerop count)
      (message
	   "Area contains NO words."))
     ((= 1 count)
      (message
	   "Area contains 1 word."))
     ((or (< 1 count) (> 5 count))
      (message
	   "Area contains %d words." count))
     (t
      (message
	   "Area contains %d words." count)))))


;; Count func lengths in file
(defun lengths-list-file (filename)
  "Returns a list of function lengths contained in FILE.
The returned list is a list of numbers.
Each number is the number of words or Lisp characters
in one function definition."
  (message "Analyzing '%s' ... " filename)
  (save-excursion
    (let ((buffer (find-file-noselect filename))
          (lengths-list))
      (set-buffer buffer)
      (setq buffer-read-only t)
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "^.defun" nil t)
        (setq lengths-list
              (cons (count-words-in-defun) lengths-list)))
      (kill-buffer buffer)
      lengths-list)))

;; open file and count defun words
(lengths-list-file "./emacs-lisp-doc.el")

;; For list of files (based on while)
(defun lengths-list-many-files (list-of-files)
  "Возвращает список длин функций в LIST-OF-FILES."
  (let (lengths-list)
    ;; true-false check
    (while list-of-files
      (setq lengths-list
            (append
             lengths-list
             ;; generate defun list length
             (lengths-list-file
              (expand-file-name (car list-of-files)))))
      ;; shifting list
      (setq list-of-files (cdr list-of-files)))
    ;; return result: the value of a list of function lengths.
    lengths-list))

(lengths-list-many-files '("./emacs-lisp-doc.el"))

;; Recursive version
(defun recursive-lengths-list-many-files (list-of-files)
  "Returns a list of function lengths from LIST-OF-FILES."
  (if list-of-files ; recursive check
      (append
       (lengths-list-file
        (expand-file-name (car list-of-files)))
       (recursive-lengths-list-many-files
        (cdr list-of-files)))))

;; usage immediately together with sorting
(defvar sorted-lengths (sort
 (recursive-lengths-list-many-files
  '("./emacs-lisp-doc.el"
    "./org_mode_basics.org"))
 '<))

;; see amount of elisp files in dir, using full path
(length
 (directory-files "../lisp" t "\\.el$"))

;; list for further chart
(defvar top-of-ranges
 '(10  20  30  40  50
   60  70  80  90 100
  110 120 130 140 150
  160 170 180 190 200
  210 220 230 240 250
  260 270 280 290 300)
 "List specifying ranges for 'defuns-per-range'.")


(defun defuns-per-range (sorted-lengths top-of-ranges)
  "Number of functions in SORTED-LENGTHS in each range TOP-OF-RANGES."
  (let ((top-of-range (car top-of-ranges))
        (number-within-range 0)
        defuns-per-range-list)
    ;; External loop.
    (while top-of-ranges
      ;; Internal loop.
      (while (and
              ;; numeric value to check is required.
              (car sorted-lengths)
              (< (car sorted-lengths) top-of-range))
        ;; Find the number of functions whose length falls within the current range.
        (setq number-within-range (1+ number-within-range))
        (setq sorted-lengths (cdr sorted-lengths)))
      ;; Exit internal loop, we're still inside external.
      (setq defuns-per-range-list
            (cons number-within-range defuns-per-range-list))
      (setq number-within-range 0) ; reset counter
      ;; Let's move on to the next range.
      (setq top-of-ranges (cdr top-of-ranges))
      ;; Set the next upper limit of the range.
      (setq top-of-range (car top-of-ranges)))
    ;; Exit the outer loop and count the number of functions whose length is greater
    ;; the boundary of the largest range from top-of-range.
    (setq defuns-per-range-list
          (cons
           (length sorted-lengths)
           defuns-per-range-list))
    ;; Return a list, where each number is the number of functions whose length
    ;; ranges from smallest to largest.
    (nreverse defuns-per-range-list)))

(defuns-per-range sorted-lengths top-of-ranges)

;; example of calculation height value from given list
(setq max-graph-height (apply 'max numbers-list))

;; variables for positioning chars
(defvar graph-symbol "*"
  "A string used as a character in a graphic, usually an asterisk.")

(defvar graph-blank " "
  "The string used as space in a graphic is usually a space.
graph-blank must be the same length as graph-symbol.")

;; .
(defun column-of-graph (max-graph-height actual-height)
  "Returns a list of MAX-GRAPH-HEIGHT strings;
ACTUAL-HEIGHT determines the number of graph-symbols.
graph-symbol are contiguous entries at the end of the list.
The list will be inserted as a single column graph.
Strings are either graph-blank or graph-symbol."
  (let ((insert-list nil)
        (number-of-top-blanks
         (- max-graph-height actual-height)))
    ;; Fill in graph-symbols.
    (while (> actual-height 0)
      (setq insert-list (cons graph-symbol insert-list))
      (setq actual-height (1- actual-height)))
    ;; Fill out graph-blanks.
    (while (> number-of-top-blanks 0)
      (setq insert-list (cons graph-blank insert-list))
      (setq number-of-top-blanks
            (1- number-of-top-blanks)))
    ;; return the complete list.
    insert-list))

;; usage: including step by step evaluating previous expressions
(column-of-graph 6 3)

;; And finally print the chart
(defun graph-body-print (numbers-list)
  "Produces a bar graph based on NUMBERS-LIST.
The list of numbers consists of the values along the Y axis."
  (let ((height (apply 'max numbers-list))
        (symbol-width (length graph-blank))
        from-position)

    (while numbers-list
      (setq from-position (point))
      (insert-rectangle
       (column-of-graph height (car numbers-list)))
      (goto-char from-position)
      (forward-char symbol-width)
      ;; Display column by column.
      (sit-for 0)
      (setq numbers-list (cdr numbers-list)))
    ;; Place label points along the X axis.
    (forward-line height)
    (insert "\n")
    ))

;; usage, or with M-:
(graph-body-print '(1 2 3 4 6 4 3 5 7 6 5 2 3))


;; Recursive versions
(defun recursive-graph-body-print (numbers-list)
  "Outputs a graph from a list of numbers NUMBERS-LIST.
A list of numbers consists of the Y axis values."
  (let ((height (apply 'max numbers-list))
        (symbol-width (length graph-blank))
        from-position)
    (recursive-graph-body-print-internal
     numbers-list
     height
     symbol-width)))


(defun recursive-graph-body-print-internal
    (numbers-list height symbol-width)
  "Prints the chart.
Used inside the recursive-graph-body-print function."
  (if numbers-list
      (progn
        (setq from-position (point))
        (insert-rectangle
         (column-of-graph height (car numbers-list)))
        (goto-char from-position)
        (forward-char symbol-width)
        (sit-for 0) ; Displays the graph column by column.
        (recursive-graph-body-print-internal
         (cdr numbers-list) height symbol-width))))

;; evaluate this
(recursive-graph-body-print '(3 2 5 6 7 5 3 4 6 4 3 2 1))
