;;; Copyright (c) 2011, ChenFengyuan. All rights reserved.

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


;;; use db-init to create a new sqlite db
;;; use (dump2db "/home/cfy/xp2-share/music/www.csdn.net.sql") to dump data to the db
;; #+ccl
;; (setf ccl:*default-external-format* :utf-8)
(in-package :common-lisp)
(declaim (optimize (debug 3)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *load* (mapc #'ql:quickload '("split-sequence" "cl-ppcre" "alexandria" "clsql-sqlite3" "hunchentoot"))))
(defpackage :cfy.csdn-analyse
  (:use :common-lisp :split-sequence :cl-ppcre :alexandria :clsql :hunchentoot))
(in-package :cfy.csdn-analyse)

(defvar *db-file* "/home/cfy/gits/clp/csdn.sqlite")
(defvar *http*  (make-instance 'hunchentoot:acceptor :port 4242))
(defparameter *stat* "nil")

(defun _stat ()
  (princ-to-string *stat*))
(defun stat()
  (_stat))
(defun start-http ()
  (hunchentoot:start *http*)
  (push (hunchentoot:create-prefix-dispatcher "/stat" #'stat) hunchentoot:*dispatch-table*))

(defun db-init ()
  (connect (list *db-file*) :database-type :sqlite3)
  (with-transaction ()
    (execute-command "create table data (i integer primary key,nick text,passwd text,email text);")
    (execute-command "create index if not exists  i_data on data (i,nick,passwd,email);")))
(defun db-clear ()
  (disconnect)
  (delete-file *db-file*))
(defun extract-from-string (string)
  (split-sequence-if
   (lambda (x)
     (member x '(#\space #\#)))
   string
   :remove-empty-subseqs t))
(defun escape (string)
  (regex-replace-all "'" string "''"))
(defun dump2db (file)
  (with-open-file (in file)
    (with-transaction ()
      (loop for i = (read-line in nil nil)
	 for j from 1
	 do (setf *stat* j)
	 while i
	 if (eq 0 (mod j 20000))
	 do (format t "~a " j)
	 unless (find-if (lambda (x) (> (char-code x) 255)) i)
	 do (let* ((extract (extract-from-string (escape i)))
		   (nick (car extract))
		   (passwd (cadr extract))
		   (email (caddr extract)))
	      (execute-command
	       (format
		nil
		"insert into data values(NULL,'~a','~a','~a');"
		nick
		passwd
		email)))))))

(defun hash-incf (key hash)
  (if (eq nil (gethash key hash))
      (setf (gethash key hash) 1)
      (incf (gethash key hash))))
