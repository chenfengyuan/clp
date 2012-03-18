(declaim (optimize (speed 3)))
(defpackage :password-generator
  (:nicknames :pg)
  (:use :cl)
  (:export :main :generator-password-to-file))
(in-package :pg)

(defun types(type)
  (ecase type
    (#\d '(0 1 2 3 4 5 6 7 8 9))
    (#\l '(#\a #\b #\c #\e #\d #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
    (#\u '(#\A #\B #\C #\E #\D #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
    (#\c '(#\a #\b #\c #\e #\d #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\A #\B #\C #\E #\D #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))))

(defun generator(n type-list func word-list)
  (if (> n 0)
      (loop for i in (types (car type-list))
	   do (generator (1- n) (cdr type-list) func (append word-list (list i))))
      (funcall func (format nil "~{~a~}" word-list))))

(defun string->type-list(str)
  (let (type-list)
    (loop for i across (reverse str)
	 do (push 
	     i
	     type-list))
    type-list))

(defun generator-password-to-file(file str)
  (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (generator (length str) (string->type-list str) (lambda (x)(write-line x out)) nil)))

(defun main()
  (let ((file (cadr ccl:*command-line-argument-list*))
	(str (caddr ccl:*command-line-argument-list*))
	need-enter)
    (when (not (and file str))
      (write-line "output file:")
      (setf file (read-line))
      (write-line "password syntax(only contian character c,l,u or d):")
      (setf str (read-line))
      (setf need-enter t))
    (write-line "generating.....")
    (generator-password-to-file file str)
    (format t "done.~f ms run time.~%" (/ (get-internal-run-time) 1.0 internal-time-units-per-second))
    (when need-enter
	(write-line "press enter to exit.")
	(read-line))))
