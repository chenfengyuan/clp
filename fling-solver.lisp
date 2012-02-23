(defpackage :fling-solver
  (:nicknames :fs)
  (:use :cl :hunchentoot :cl-who)
  (:export :fling-http-solver))
(in-package :fling-solver)

(defparameter *row* 8)
(defparameter *column* 7)
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
    (walk board n nil)
    (walk-result)))

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     ,@body))
(defun output-html (row column)
  (with-html
    (:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en"
	   (:head
	     (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
	     (:title "fling solver"))
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
		  (setf r (mapcar (lambda (x) (list (format
						     nil "~a,~a"
						     (1+ (car x))
						     (1+ (cadr x)))
						    (caddr x))) (car (fling-solver b))))
		  (setf end (get-internal-real-time))
		  (htm (:p (str (format nil "execute time:~a" (- end start)))))
		  (htm (:p (str r)))))
	    (:form
	     :action "fling-solver.lisp"
	     :method "post"
	     (:table
	      (loop for i from 0 to (1- row)
		 do (htm
		     (:tr
		      (loop for j from 0 to (1- column)
			 do (htm
			     (:th
			      (:input :type "checkbox" :name (format nil "~a,~a" i j)))))))))
	     (:p (:input :type "submit" :value "submit")))
	    (:p (:a :href "http://validator.w3.org/check?uri=referer" (:img :src "http://www.w3.org/Icons/valid-xhtml10" :alt "Valid XHTML 1.0 Strict" :height"31" :width "88" )))))))
(defun main-html ()
  (output-html 8 7))

(defun fling-http-solver ()
  (start (make-instance 'hunchentoot:easy-acceptor :port 4242))
  (push (create-prefix-dispatcher "/cl/fling-solver.lisp" #'main-html) *dispatch-table*))
