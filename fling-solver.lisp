(in-package :cl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(and ccl windows)
  (pushnew :hunchentoot-no-ssl *features*)
  (dolist (p '(:hunchentoot :cl-who :alexandria))
    (unless (find-package p)
      (ql:quickload p))))
(declaim (optimize (speed 3)))
(defpackage :fling-solver
  (:nicknames :fs)
  (:use :cl :hunchentoot :cl-who :alexandria)
  (:export :fling-http-solver :main))
(in-package :fling-solver)

(defparameter *row* 8)
(defparameter *column* 7)
(defparameter *log-file* nil)
(defun onboard (board x y)
  (= 1 (aref board x y)))
(defun unput (board x y)
  (setf (aref board x y) 0))
(defun put (board x y)
  (setf (aref board x y) 1))

(defun move (board x y direction)
  (let ((max-x (1- (array-dimension board 0)))
	(max-y (1- (array-dimension board 1))))
    (unput board x y)
    (ecase direction
      ((right)
       (loop
	  for i from (1+ y) to max-y
	  if (onboard board x i)
	  do (progn
	       (unput board x i)
	       (put board x (1- i)))))
      ((left)
       (loop
	  for i from (1- y) downto 0
	  if (onboard board x i)
	  do (progn
	       (unput board x i)
	       (put board x (1+ i)))))
      ((up)
       (loop
	  for i from (1- x) downto 0
	  if (onboard board i y)
	  do (progn
	       (unput board i y)
	       (put board (1+ i) y))))
      ((down)
       (loop
	  for i from (1+ x) to max-x
	  if (onboard board i y)
	  do (progn
	       (unput board i y)
	       (put board (1- i) y)))))
    board))

(defun unmove (board x y direction)
  (let ((max-x (1- (array-dimension board 0)))
	(max-y (1- (array-dimension board 1))))
    (put board x y)
    (ecase direction
      ((right)
       (loop
	  for i from max-y downto (1+ y)
	  if (onboard board x i)
	  do (progn
	       (unput board x i)
	       (put board x (1+ i)))))
      ((left)
       (loop
	  for i from 0 to (1- y)
	  if (onboard board x i)
	  do (progn
	       (unput board x i)
	       (put board x (1- i)))))
      ((up)
       (loop
	  for i from 0 to (1- x)
	  if (onboard board i y)
	  do (progn
	       (unput board i y)
	       (put board (1- i) y))))
      ((down)
       (loop
	  for i from max-x downto (1+ x)
	  if (onboard board i y)
	  do (progn
	       (unput board i y)
	       (put board (1+ i) y)))))
    board))

(let (result)
  (defun walk-init ()
    (setf result nil))
  (defun walk-result ()
    result)
  (defun walk (board n path)
    (if result
	result
	(if (<= n 1)
	    (push (reverse path) result)
	    (let ((max-x (1- (array-dimension board 0)))
		  (max-y (1- (array-dimension board 1))))
	      (loop
		 for x from 0 to max-x
		 do (loop
		       for y from 0 to (- max-y 2)
		       if (and (onboard board x y) (not (onboard board x (1+ y))))
		       do (loop
			     for i from (+ y 2) to max-y
			     if (onboard board x i)
			     do (progn
				  (move board x y 'right)
				  (walk board (1- n) (push (list x y 'right) path))
				  (pop path)
				  (unmove board x y 'right)
				  (move board x i 'left)
				  (walk board (1- n) (push (list x i 'left) path))
				  (pop path)
				  (unmove board x i 'left))
			     until (onboard board x i))))
	      (loop
		 for y from 0 to max-y
		 do (loop
		       for x from 0 to (- max-x 2)
		       if (and (onboard board x y) (not (onboard board (1+ x) y)))
		       do (loop
			     for i from (+ x 2) to max-x
			     if (onboard board i y)
			     do (progn
				  (move board x y 'down)
				  (walk board (1- n) (push (list x y 'down) path))
				  (pop path)
				  (unmove board x y 'down)
				  (move board i y 'up)
				  (walk board (1- n) (push (list i y 'up) path))
				  (pop path)
				  (unmove board i y 'up))
			     until (onboard board i y)))))))))
(defun fling-solver (board)
  (let ((n (loop
	      with n = 0
	      for i from 0 to (1- (array-dimension board 0))
	      do (loop
		    for j from 0 to (1- (array-dimension board 1))
		    if (= 1 (aref board i j))
		    do (incf n))
	      finally (return n))))
    (walk-init)
    (walk (copy-array board) n nil)
    (walk-result)))

(defun output-html-table (board)
  (with-html-output-to-string (*standard-output* nil :indent t)
    (:table 
     (loop
	for i from 0 to (1- (array-dimension board 0))
	do (htm (:tr
		 (loop
		    for j from 0 to (1- (array-dimension board 1))
		    do (htm
			(:td
			 :class
			 (ecase (aref board i j)
			   (0 "off")
			   (1 "on")
			   (right "right")
			   (left "left")
			   (up "up")
			   (down "down")))))))))))
(defun output-solution-table (board solution)
  (let ((board (copy-array board :element-type t)))
    (with-html-output-to-string (*standard-output* nil :indent t)
      (loop
	 for step in (car solution)
	 do (setf (aref board (car step) (cadr step)) (caddr step))
	 do (htm (str (output-html-table board)))
	 do (move board (car step) (cadr step) (caddr step))))))

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     ,@body))
(defun output-html (row column)
  (with-html
    (:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en"
	   (:head
	     (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
	     (:title "fling solver")
	     (:style :type "text/css" "td { width:30px;height:30px;border:1px solid #999;background-color: #fff;}.on {background-color:#060;} .right {background:#006;border-right: 5px solid red;} .left {background:#006;border-left: 5px solid red;} .up {background:#006;border-top: 5px solid red;} .down {background:#006;border-bottom: 5px solid red;} #problem td:hover {background-color: #CCC;}" )
	     (:script :type "text/javascript"
"      function $(id) {
	return document.getElementById(id);
      }
      function setCharacter(row, col) {
	var td = $(\"a\"+ row + \"_\" + col);
        var id = $(\"a\"+ row + \"__\" + col);  
	td.className = td.className == \"on\" ? \"\" : \"on\";
        id.value = td.className == \"on\" ? \"on\" : \"off\";
      }
"))
	   (:body
	    (if (post-parameters*)
		(let ((b (make-array (list *row* *column*) :initial-element 0 :element-type '(unsigned-byte 8)))
 		      start end r)
		  (loop
		     for i from 0 to (1- *row*)
		     do (loop
			   for j from 0 to (1- *column*)
			   if (string= "on" (post-parameter (format nil "~a,~a" i j)))
			   do (setf (aref b i j) 1)))
		  (setf start (get-internal-real-time))
		  ;; (setf r (mapcar (lambda (x) (list (format
		  ;; 				     nil "~a,~a"
		  ;; 				     (1+ (car x))
		  ;; 				     (1+ (cadr x)))
		  ;; 				    (caddr x))) (car (fling-solver b))))
		  (setf r (fling-solver b))
		  (if (and r *log-file*)
		      (with-open-file (out *log-file* :direction :output :if-does-not-exist :create :if-exists :append)
		    (write b :pretty nil :stream out)
		    (princ #\newline out)))
		  (setf end (get-internal-real-time))
		  #+ (and ccl linux x86_64) (htm (:p (str (format nil "execute time:~a us" (- end start)))))
		  #+ (and ccl windows) (htm (:p (str (format nil "execute time:~a ms" (- end start)))))
		  #+ (and ccl linux x86) (htm (:p (str (format nil "execute time:~a ms" (- end start)))))
		  (htm (str (output-solution-table b r)))
		  ;; (htm (:p (str r)))
		  ))
	    (:form
	     :action "fling-solver.lisp"
	     :method "post"
	     (:div :id "problem"
		   (:table
		    (loop for i from 0 to (1- row)
			  do (htm
			      (:tr
			       (loop for j from 0 to (1- column)
				     do (htm
					 (:td :id (format nil "a~a_~a" i j) :onclick (format nil  "setCharacter(~a,~a)" i j)
					      (:input :type "hidden" :name (format nil "~a,~a" i j) :id (format nil "a~a__~a" i j))))))))))
	     (:p (:input :type "submit" :value "Solve!")))
	    (:p "the javascript and css in this page are copied from" (:a :href "http://www.anthonytambrin.com/flingsolve/" "http://www.anthonytambrin.com/flingsolve/"))
	    (:p (:a :href "http://validator.w3.org/check?uri=referer" (:img :src "http://www.w3.org/Icons/valid-xhtml10" :alt "Valid XHTML 1.0 Strict" :height"31" :width "88" )))))))
(defun main-html ()
  (output-html 8 7))

(defun fling-http-solver (&optional (port 6673))
  (start (make-instance 'hunchentoot:easy-acceptor :port port))
  (push (create-prefix-dispatcher "/cl/fling-solver.lisp" #'main-html) *dispatch-table*))

(defun main ()
  #-ccl
  (format t "Sorry,but I do not support ~a yet.~%" (lisp-implementation-type))
  #+ccl
  (let ((1st
	 #+ccl (car ccl:*command-line-argument-list*))
	(2nd
	 #+ccl (cadr ccl:*command-line-argument-list*))
	port)
    (if (search "-h" (string-downcase 1st))
	(format t "Usage: ~a [PORT]~%" 1st)
	(progn
	  (if 2nd
	      (setf port (parse-integer 2nd))
	      (setf port 6673))
	  (format t "http start at port:~d.~%please visit http://127.0.0.1:~:*~d/cl/fling-solver.lisp~%" port)
	  (force-output)
	  (fling-http-solver port)
	  #+ (and ccl linux)
	  (ccl:wait-for-signal 2 nil)
	  #+ (and ccl windows)
	  (read-char)))))
