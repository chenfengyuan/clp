#+ccl
(defun encode64(string)
  (cl-base64:string-to-base64-string
   (decode-string-from-octets
    (encode-string-to-octets string :external-format :utf-8))))
#+ccl
(defun decode64(string)
  (decode-string-from-octets
   (encode-string-to-octets
    (cl-base64:base64-string-to-string string))
   :external-format :utf-8))
