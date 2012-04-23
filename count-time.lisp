(defpackage :cfy.count-time
  (:use :cl)
  (:nicknames :ct)
  (:export :ct-init :ct-count))
(in-package :cfy.count-time)
(defun hash-incf (key hash)
  (if (eq nil (gethash key hash))
      (setf (gethash key hash) 1)
      (incf (gethash key hash))))

(defun ct-init ()
  (make-hash-table :test #'equalp))
(defmacro ct-count (ct &body body)
  (let ((str (format nil "~{~a~}" body))
	(start (gensym))
	(result (gensym)))
    `(let (,start ,result)
       (unless (gethash ,str ,ct)
	 (setf (gethash ,str ,ct) (make-hash-table :test #'eql)))
       (setf ,start (get-internal-real-time))
       (setf ,result (progn,@body))
       (hash-incf (- (get-internal-real-time) ,start) (gethash ,str ,ct))
       ,result)))
