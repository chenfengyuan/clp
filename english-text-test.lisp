#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(asdf:oos 'asdf:load-op :cl-ppcre)
(defun read-file-to-vector(filepath)
  (let ((content (make-array 0 :fill-pointer t :adjustable t)))
    (with-open-file (in filepath)
      (loop as i = (read-line in nil) while i do (vector-push-extend  i content)))
    content))
(defun remove-words(string)
  (cl-ppcre:regex-replace-all
   "(?<=\\[)[^\\]]+(?=])"
   string
   ""))

;; (defun run-emacs(filepath)
;;   (#+ sbcl
;;       sb-ext:run-program "emacsclient" (list filepath) :search t))

(defun write-vector-to-file(filepath vector)
  (with-open-file (out filepath :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop for i across vector do (format out "~a~%" i))))
(defun rm-file(path)
  (sb-ext:run-program "/bin/rm" (list path)))
(defun main()
  ;; (format t "~{~a ~}" sb-ext:*posix-argv*))
  (let* ((filepath (cadr sb-ext:*posix-argv*))
	 (content (map 'vector #'remove-words (read-file-to-vector filepath)))
	 )
    (write-vector-to-file "/dev/shm/ett-a" content)
    ;; (run-emacs "/dev/shm/ett-a")
    (sb-ext:run-program "emacsclient" (list "/dev/shm/ett-a") :search t)
    ;; (sb-ext:run-program "colordiff" (list "-ub" filepath "/dev/shm/ett-a") :search t :output t)
    (sb-ext:run-program "emacsclient" (list  "-e"
					     (format nil "(ediff \"/dev/shm/ett-a\" \"~a\")" filepath)) :search t)
    (mapcar #'rm-file '("/dev/shm/ett-a"))))
