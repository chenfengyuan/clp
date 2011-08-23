#+ccl
(setf ccl:*default-external-format* :utf-8)

(defpackage :cfy.merge-sub
  (:use :common-lisp))
(in-package :cfy.merge-sub)

(defun subrip-parse-time(string)
  (let ((start-end (position #\Space string))
	(end-start (1+ (position #\Space string :from-end t))))
    (values (subseq string 0 start-end)
	    (subseq string end-start))))

(defstruct subrip start end str n)
(defun subrip->vector (file &optional vector)
  (let ((v (or vector (make-array 0 :adjustable t :fill-pointer 0)))
	(temp (make-subrip)))
    (with-open-file (in file)
      (loop for i = (read-line in nil nil)
	 do (cond ((and (string/= "" (subrip-str temp)) (= 0 (length i)))
		   (vector-push-extend (copy-structure temp) v)
		   (setf (subrip-str temp) "" (subrip-start temp) nil))
		  ((and (eq nil (subrip-start temp)) (search "-->" i))
		   (multiple-value-bind (a b) (subrip-parse-time i)
		     (setf (subrip-start temp) a
			   (subrip-end temp) b)))
		  ((and (subrip-start temp) (> (length i) 0))
		   (setf (subrip-str temp) (concatenate 'string (subrip-str temp)i)))
		  ((eq nil i)
		   (return)))))
    v))
(defun subrip-merge(&rest files)
  (let ((v (make-array 0 :adjustable t :fill-pointer 0))
	(nv (make-array 0 :adjustable t :fill-pointer 0))
	(temp))
    (loop for f in files
	 do (subrip->vector f v))
    (loop for i across (stable-sort v (lambda (a b)(string< (subrip-start a) (subrip-start b))))
       if (eq temp nil)
          do (setf temp (copy-structure i))
       else if (string> (subrip-end temp) (subrip-start i))
               do (setf (subrip-str temp) (concatenate 'string (subrip-str temp) #(#\Newline) (subrip-str i))) and
               if (string> (subrip-end i) (subrip-end temp))
                  do (setf (subrip-end temp) (subrip-end i))
               end
            else do (vector-push-extend temp nv) and
	         do (setf temp (copy-structure i))
	    end
       end
       finally (vector-push-extend temp nv))
    nv))

(defun vector->subrip(vector &optional (stream t))
  (loop for i from 1
       for j across vector
       do (format stream "~a~%~a --> ~a~%~a~%~%" i (subrip-start j) (subrip-end j) (subrip-str j))))