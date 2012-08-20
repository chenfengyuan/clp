(declaim (optimize (debug 3)))
(defpackage :cfy.android_lock_patterns
  (:use :cl)
  (:nicknames :alp))
(in-package :alp)
(defvar *n* nil)
(defun /2 (a b)
  (/ (+ a b) 2))
(defun cornerp (x y)
  (and
   (loop
      for (i j) in '((0 0) (2 0) (0 2) (2 2))
      until (and (= i x) (= j y))
      finally (return (and (= i x) (= j y))))))
(defun t1 (map px py x y)
  (cond
    ((and (cornerp x y)
	  (cornerp px py))
     (aref map (/2 x px) (/2 y py)))
    ((and (= px x) (= 2 (abs (- py y)))) (aref map x (/2 y py)))
    ((and (= py y) (= 2 (abs (- px x)))) (aref map (/2 x px) y))
    (t t)))
(defun dfs (map n px py)
  (if (>= n 4)
      (incf *n*))
  (setf (aref map px py) t)
  (loop
     for x from 0 to 2
     do (loop
	   for y from 0 to 2
	   if (and
	       (<= n 8)
	       (null (aref map x y))
	       (t1 map px py x y))
	   do (dfs map (1+ n) x y)))
  (setf (aref map px py) nil))
(defun search-dfs ()
  (setf *n* 0)
  (loop for x from 0 to 2
     do (loop for y from 0 to 2
	   do (dfs (make-array '(3 3) :initial-element nil) 1 x y)))
  *n*)
