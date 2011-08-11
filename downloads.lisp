#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(defvar *load-drakma-and-cl-ppcre-p* nil)
(cond ((not *load-drakma-and-cl-ppcre-p*)
       (asdf:oos 'asdf:load-op :drakma)
       (asdf:oos 'asdf:load-op :cl-ppcre)
       (asdf:operate 'asdf:load-op 'cl-fastcgi)))
					;       (asdf:oos 'asdf:load-op :swank)))

(cond ((not *load-drakma-and-cl-ppcre-p*)
       (cl-fastcgi:load-libfcgi "/usr/lib/libfcgi.so.0.0.0")))
(setf *load-drakma-and-cl-ppcre-p* t)
#+ccl
(setf ccl:*default-external-format* :utf-8)

(defpackage cfy.downloads
  (:use :common-lisp :drakma :cl-ppcre)
  (:export :get-115-download-url :get-flash-urls :download-flash :main :fcgi-main))
(in-package :cfy.downloads)
(defvar *load-drakma-and-cl-ppcre-p* nil)
(defvar *115-LOGIN* (make-instance 'drakma:cookie-jar))
(defvar *user-agent* "Opera/9.80 (X11; Linux x86_64; U; en) Presto/2.8.131 Version/11.10")
(defvar *flash-url-example* "http://v.youku.com/v_show/id_XMTE0OTE4MjAw.html")
(defvar *115-url-example* "http://u.115.com/file/b6ntyg8r")

(defun concatenate-strings(&rest strings)
  (apply #'concatenate 'string strings))
(defun join-string-list (string-list &optional (item #\Space))
  "Concatenates a list of strings
and puts spaces between the elements."
  (let ((format-string (format nil "~a" (concatenate-strings "~{~a~^" (list item) "~}"))))
    (format nil format-string string-list)))
(defun 115-login (&key (account "115_down")(passwd "fn4=IplVhkmwtqvjh7dy"))
  (drakma:http-request "http://passport.115.com/?ac=login&goto=http%3A%2F%2Fu.115.com%2Ffile%2Fb6ntyg8r"
		       :method :post
		       :parameters (list (cons "login[account]"  account)
					 (cons "login[passwd]"  passwd))
		       :cookie-jar *115-LOGIN*
		       :user-agent *user-agent*))
(defun test-115-login()
  (and (car (ppcre:all-matches-as-strings "115down@gmail\.com" (drakma:http-request "http://u.115.com/file/b6ntyg8r" :cookie-jar *115-LOGIN* :user-agent *user-agent*))) t))
(defun get-115-content(115-url)
  (ignore-errors
    (drakma:http-request 115-url :cookie-jar *115-LOGIN* :user-agent *user-agent*)))
(defun get-115-download-url(115-content)
  (unless (cookie-jar-cookies *115-LOGIN*) (115-login)))

;; (car (ppcre:all-matches-as-strings "http://(?:\\d*\\.)?bak\\.[^\"]+" 115-content)))
(defun get-115-filename(115-content)
  ;; (string-right-trim

  ;;  (string-left-trim
  ;;   (concatenate-strings "<title>" '(#\CR #\LF))
  ;;   (car (all-matches-as-strings "<title>[\\d\\D].*?</title>"
  ;; 				 115-content)))))
  )
(defun 115-fcgi-put(url)
  (let ((content (get-115-content url)))
    (join-string-list (list
		       (get-115-filename content)
		       (get-115-download-url content)))))
(defun get-flvcd-content(url &optional (quality "high"))
  (let ((get-url (concatenate 'string "http://www.flvcd.com/parse.php?flag=&format=" quality "&kw=" url "&sbt=%BF%AA%CA%BCGO%21")))
    (ignore-errors (drakma:http-request get-url
					:user-agent *user-agent*))))
(defun get-flash-urls(flvcd-content)
  (mapcar
   (lambda (string)
     (subseq string 3))
   (ppcre:all-matches-as-strings "<U>http://.*"
			   flvcd-content)))
;; (defun wget(url &key (dir-pre "/home/cfy/movie/") (output-file nil output-file-given))
;;   (let ((args (if output-file-given (list "-U" *user-agent* "-c" url  "-O" (concatenate 'string dir-pre output-file)) (list "-c" url "-P" dir-pre))))
;;     ;; (format t "~s" args)
;;     #+:sbcl
;;     (sb-ext:run-program "wget" args :input t :output t :search t)
;;     #+:clisp
;;     (apply #'ext:execute "/usr/bin/wget" :arguments args)))
(defun porper-file-name(sum current)
  (format nil "~V,'0d" (ceiling (/ (log (1+ sum))(log 10))) current))
(defun flash-urls->wget-para(urls)
  (loop for i below (length urls) collect
       (list (nth i urls)
	     (concatenate-strings (porper-file-name (length urls) i)
				  ".flv"))))
(defun make-temp-file()
  (block nil
    (let ((output-string (make-string-output-stream)))
      #+sbcl (sb-ext:run-program "/bin/mktemp" '("-p" "/dev/shm") :output output-string)
      #+ccl (ccl:run-program "/bin/mktemp" '("-p" "/dev/shm") :output output-string)
      #+clisp (progn
		(let* ((stream (POSIX:MKSTEMP "/dev/shm/downloads-tmp"))
		       (string  (format nil "~a" stream))
		       (start (search "/dev/shm/downloads-tmp" string)))
		  (close stream)
		  (return (subseq string start (- (length string) 1)))))
      (string-right-trim '(#\Newline)(get-output-stream-string output-string)))))
(defun rm-file(path)
  #+sbcl (sb-ext:run-program "/bin/rm" (list path))
  #+ccl (ccl:run-program "/bin/rm" (list path))
  #+clisp (ext:execute "/bin/rm" path))
(defun drakma-gb18030-to-utf8(string)
  (let ((tmp-a (make-temp-file))
	(tmp-b (make-temp-file))
	(tmp-c (make-temp-file)))
    (rm-file tmp-a)
    (with-open-file (out tmp-a :direction :output)(format out "~a" string))
    #+(or sbcl clisp ccl)(rm-file tmp-b)
    #+sbcl (sb-ext:run-program "iconv" '("-f" "utf8" "-t" "latin1")  :input tmp-a :search t :output tmp-b)
    #+clisp (ext:execute "/usr/bin/iconv" "-f" "utf8" "-t" "latin1"  tmp-a "-o" tmp-b)
    #+ccl (ccl:run-program "iconv" '("-f" "utf8" "-t" "latin1") :input tmp-a  :output tmp-b)
    #+(or sbcl clisp ccl) (rm-file tmp-c)
    #+sbcl (sb-ext:run-program "iconv" '("-f" "gb18030" "-t" "utf8")  :input tmp-b :search t :output tmp-c)
    ;; #+ccl (ccl:run-program "iconv" '("-f" "gb18030" "-t" "utf8") :input tmp-b :output tmp-c)
    #+clisp (ext:execute "/usr/bin/iconv" "-f" "gb18030" "-t" "utf8"  tmp-b "-o" tmp-c)
    #+ccl (ccl:run-program "iconv" '("-f" "gb18030" "-t" "utf8") :input tmp-b  :output tmp-c)
    (with-open-file (in tmp-c)
      (let* ((length (file-length in))
	     (text (make-string (file-length in)))
	     (read (read-sequence text in)))
	(mapcar #'rm-file (list tmp-a tmp-b tmp-c))
	(if (< read length)
	    (subseq text 0 read)
	    text)))))
(defun get-flash-video-name(flvcd-content)
  (string-trim '(#\") (car (ppcre:all-matches-as-strings "\".*$" (car (ppcre:all-matches-as-strings "document.title = \"[^\"]+\"" (drakma-gb18030-to-utf8 flvcd-content)))))))
;; (defun download-flash(url)
;;   (let* ((video-name (get-flash-video-name (get-flvcd-content url)))
;; 	 (wget-para (flash-urls->wget-para(get-flash-urls (get-flvcd-content url))))
;; 	 (dir-pre (concatenate-strings "/home/cfy/movie/" video-name "/")))
;;     #+sbcl (sb-ext:run-program "/bin/mkdir" (list dir-pre))
;;     #+ccl (ccl:run-program "/bin/mkdir" (list dir-pre))
;;     (loop for para in wget-para do (wget (nth 0 para) :dir-pre  dir-pre :output-file (nth 1 para)))))
(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))
(defun flash-cgi-put(url)
  (let ((content (get-flvcd-content url)))
    (join-string-list
     (apply
      #'list
      (get-flash-video-name content)
      (flatten
       (flash-urls->wget-para
	(get-flash-urls content)))))))
(defun fcgi-115(req query-string)
  (cl-fastcgi:fcgx-puts req (format nil "Content-Type: text/plain~%~%~a"
				    (115-fcgi-put query-string))))
(defun fcgi-flash(req query-string)
  (cl-fastcgi:fcgx-puts req (format nil "Content-Type: text/plain~%~%~a"
				    (flash-cgi-put 
				     query-string))))
(defun fcgi(req)
  (let ((query-string (cl-fastcgi:fcgx-getparam req "QUERY_STRING")))
    (cond ((not(search "u.115.com/file" query-string))
	   (fcgi-flash req query-string))
	  (t
	   (fcgi-115 req query-string)))))
;; (defun fcgi(req)
;;   (cl-fastcgi:fcgx-puts req (format nil "Content-Type: text/plain~%~%"))
;;   (cl-fastcgi:fcgx-puts
;;    req
;;    (format nil "~a"
;; 	   (flash-cgi-put
;; 	    (cl-fastcgi:fcgx-getparam req "QUERY_STRING")))))
(defun fcgi-main()
  (cl-fastcgi:socket-server #'fcgi
			    :inet-addr "127.0.0.1"
			    :port 9000))


;; (cond ((not *load-drakma-and-cl-ppcre-p*)
;;        (swank:create-server :port 4004)))
;; (setf *load-drakma-and-cl-ppcre-p* t)
;; (defun main()
;;   (sb-thread:make-thread #'fcgi-main))

