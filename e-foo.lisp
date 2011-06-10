(defun get-args()
  #+ clisp
  ext:*args*
  #- clisp
  (error "not implement"))

(defparameter *foo-pre* "<html>
<head>
<meta http-equiv=\"Refresh\" content=\"0; url=
")

(defparameter *foo-suf* "				    \" />
</head>
</html>")

(with-open-file (out "/home/cfy/temp/vm-share/foo.html" :direction :output :if-exists :supersede :if-does-not-exist :create)
  (format out "~a~{~a~}~a" *foo-pre* (get-args) *foo-suf*))
