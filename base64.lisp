#+ccl
(defun encode64(string)
  (cl-base64:string-to-base64-string
   (ccl:decode-string-from-octets
    (ccl:encode-string-to-octets string :external-format :utf-8))))
#+ccl
(defun decode64(string)
  (ccl:decode-string-from-octets
   (ccl:encode-string-to-octets
    (cl-base64:base64-string-to-string string))
   :external-format :utf-8))
