(defpackage :cfy.math
  (:use :cl)
  (:export :pow))
(in-package :cfy.math)
(defun pow (a n)
  (loop
       with m = 1
       while (> n 1)
       if (evenp n)
       do (setf n (/ n 2) a (* a a))
       else
       do (progn (decf n)
	      (setf m (* m a)))
       finally (return (* m a))))
