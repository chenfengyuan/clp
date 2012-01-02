;;; Copyright (c) 2012, ChenFengyuan. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; use the algorithm readed from http://cubbi.com/fibonacci.html
(defpackage :cfy.fibonacci
  (:use :cl)
  (:export :fib-naive-binary-recursion :fib-cached-binary-recursion-init :fib-cached-binary-recursion :fib-cached-linear-recursion-init :fib-cached-linear-recursion :fib-matrix-multiplication))
(in-package :cfy.fibonacci)

;; ALGORITHM 1A: NAIVE BINARY RECURSION
(defun fib-naive-binary-recursion (n)
  (cond
    ((>= n 2)
     (+ (fib-naive-binary-recursion (- n 1))
	(fib-naive-binary-recursion (- n 2))))
    (t
     n)))

;; ALGORITHM 1B: CACHED BINARY RECURSION / MEMOIZATION
(let ((a (make-array 2 :element-type 'integer :adjustable t :fill-pointer 2 :initial-contents '(0 1))))
  (defun fib-cached-binary-recursion-a ()
    a)
  (defun fib-cached-binary-recursion-init ()
    (setf a (make-array 2 :element-type 'integer :adjustable t :fill-pointer 2 :initial-contents '(0 1))))
  (defun fib-cached-binary-recursion (n)
    (cond
      ((>= n (fill-pointer a))
       (vector-push-extend (+ (fib-cached-binary-recursion (- n 1))
			      (fib-cached-binary-recursion (- n 2)))
			   a)
       (elt a n))
      (t
       (elt a n)))))

;; ALGORITHM 2A: CACHED LINEAR RECURSION
(let ((a (make-array 2 :element-type 'integer :adjustable t :fill-pointer 2 :initial-contents '(0 1))))
  (defun fib-cached-linear-recursion-init ()
    (setf a (make-array 2 :element-type 'integer :adjustable t :fill-pointer 2 :initial-contents '(0 1))))
  (defun fib-cached-linear-recursion (n)
    (if (< n (fill-pointer a))
	(elt a n)
	(loop for i from (fill-pointer a) to n
	     do (vector-push-extend (+ (elt a (1- i))
				    (elt a (- i 2)))
				 a)
	     finally (return (elt a n))))))

;;; ALGORITHM 3C: BINET'S FORMULA WITH ROUNDING

(let ((a (/ (+ 1
	       (sqrt 5))
	    2)))
  (defun fib-binet (n)
    (floor
     (+ 1/2
	(/ (expt a n)
	   (sqrt 5))))))

;;; ALGORITHM 3A: MATRIX MULTIPLICATION
(defmethod mat-multi ((a simple-array) b)
  (loop
     with c = (make-array '(2 2))
     for i from 0 to 1
     do (loop for j from 0 to 1
	     do (setf (aref c i j)
		      (+ (* (aref a i 0)
			    (aref b 0 j))
			 (* (aref a i 1)
			    (aref b 1 j)))))
       finally (return c)))
(defmethod mat-multi ((a integer) b)
  (loop for i from 0 to 1
     do (loop for j from 0 to 1
	   do (setf (aref b i j) (* a (aref b i j))))
       finally (return b)))
(defmethod fib-matrix-multiplication (n)
  (loop with a = 1 and s = #2A((1 1) (1 0))
       while (> n 0)
       if (oddp n)
       do (setf a (mat-multi a s) n (1- n))
       else do (setf n (/ n 2)
		     s (mat-multi s s))
     finally (return (aref a 0 1))))
