(time (with-open-file (in "/dev/shm/po2db-test/d" :element-type '(unsigned-byte 8))
	   (let* ((text (make-array 40960 :element-type '(unsigned-byte 8))))
	     (do ((s 0 (+ s (loop for char across text counting (eql char 10)))))
		 ((eql 0 (read-sequence text in))s)))))
(time (with-open-file (in "/dev/shm/po2db-test/d" :element-type '(unsigned-byte 8))
	   (let* ((length (file-length in))
		  (text (make-array length :element-type '(unsigned-byte 8))))
	     (read-sequence text in)
	     (loop for char across text counting (eql char 10)))))