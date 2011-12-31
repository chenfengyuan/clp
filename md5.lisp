(defpackage :cfy.md5
  (:use :cl :cffi)
  (:export :md5 :md5-each-line))
(in-package :cfy.md5)
(cffi:load-foreign-library "libssl.so" :search-path "/usr/lib/")
(defun md5 (string)
  (string-downcase
   (with-output-to-string (out)
     (loop for i from 0 
	do (format
	    out "~2,'0x"
	    (cffi:mem-aref
	     (cffi:foreign-funcall
	      "MD5"
	      :string string
	      :unsigned-long (length string)
	      :int 0
	      :pointer )
	     :uchar i))
	repeat 16))))
(defun md5-each-line ()
  (loop
     for i = (read-line nil nil)
     while i
       do (format t "~a ~a~%" (md5 i) i)))