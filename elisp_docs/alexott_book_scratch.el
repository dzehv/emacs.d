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

;; define variable with documentation
;; instead of set/q assigns only if new var was not defined yet
(defvar new-test-var 10 "Var documentation string.")


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
