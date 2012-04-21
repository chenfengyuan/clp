(defpackage :cfy.count-time
  (:use :cl)
  (:nicknames :ct)
  (:export :ct-init :ct-count))
(in-package :cfy.count-time)
(defun ct-init ()
  (make-hash-table :test #'equalp))
(defmacro ct-count (ct &body body)
  (let ((str (format nil "~{~a~}" body))
	(start (gensym)))
    `(let (,start)
       (unless (gethash ,str ,ct)
	 (setf (gethash ,str ,ct) (make-array 0 :adjustable t :fill-pointer 0)))
       (setf ,start (get-internal-real-time))
       ,@body
       (vector-push-extend (- (get-internal-real-time) ,start) (gethash ,str ,ct)))))
