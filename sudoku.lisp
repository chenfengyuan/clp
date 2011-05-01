#!/usr/bin/sbcl --script

;;; copy from http://www.frank-buss.de/lisp/sudoku.html
(defun print-sudoku (sudoku)
  (loop for y from 0 below 9
        finally (terpri)
        do (loop for x from 0 below 9 finally (terpri) do
                 (format t "~A" (aref sudoku y x)))) )

(defun digits-in-region (sudoku x y)
  (loop
   with x0 = (* 3 (truncate x 3))
   with y0 = (* 3 (truncate y 3))
   with x1 = (+ x0 2)
   with y1 = (+ y0 2)
   for x from x0 to x1
   append (loop for y from y0 to y1
                for digit = (aref sudoku y x)
                when (/= digit 0) collect digit)))

(defun digits-in-row (sudoku y)
  (loop for x from 0 below 9
        for digit = (aref sudoku y x)
        when (/= digit 0) collect digit))

(defun digits-in-column (sudoku x)
  (loop for y from 0 below 9
        for digit = (aref sudoku y x)
        when (/= digit 0) collect digit))

(defun create-missing (list)
  (loop for i from 1 to 9
        with result = '()
        finally (return result) do
        (unless (find i list) (push i result))))

(defun possible-digits (sudoku x y)
  (create-missing
   (union
    (digits-in-region sudoku x y)
    (union (digits-in-row sudoku y)
           (digits-in-column sudoku x)))))

(defun solve-next (sudoku x y)
  (when (= 9 (incf x))
    (when (= 9 (incf y))
      (print-sudoku sudoku)
      (return-from solve-next))
    (setf x 0))
  (if (/= 0 (aref sudoku y x))
      (solve-next sudoku x y)
    (let ((possible-digits (possible-digits sudoku x y)))
      (when possible-digits
        (dolist (digit possible-digits)
          (setf (aref sudoku y x) digit)
          (solve-next sudoku x y)
          (setf (aref sudoku y x) 0))))))

(defun solve (sudoku)
  (solve-next (make-array '(9 9) :initial-contents sudoku) -1 0))


(defun string->list (str)
	   (loop for i upto (- (length str) 1)collect (string (char str i))))
(defun string->sudoku-solve (str) (mapcar (lambda (x)(mapcar #'parse-integer x)) (mapcar #'string->list ((lambda (str)(loop for i upto 8 collect (subseq str (* i 9) (* (1+ i) 9)))) (substitute #\0 #\. str)))))
#-(or sbcl clisp)
(progn (print "not implement.")(quit))
#+sbcl
(progn
  (cond ((/= (length sb-ext:*posix-argv*) 2)(format t "not enough arguments~%./sudoku.lisp sudoku-string.~%for example: ./sudoku.lisp ~a~%" "\".......12........3..23..4....18....5.6..7.8.......9.....85.....9...4.5..47...6...\"")(quit)))
  (time (solve (string->sudoku-solve (cadr sb-ext:*posix-argv*)))))
#+clisp
(time (solve (string->sudoku-solve (car *args*))))