(asdf:operate 'asdf:load-op 'cl-fastcgi)
(defun simple-app (req)
  (let ((c (format nil "Content-Type: text/plain~%~%中文测试")))(cl-fastcgi:fcgx-puts req c)))
(cl-fastcgi:socket-server #'simple-app :inet-addr "127.0.0.1" :port 9000)