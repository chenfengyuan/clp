(defparameter *max* 0)
(defparameter *e* 0)
(defparameter *max-e* nil)
(defparameter *n* 0)
(defun maxn(n ne)
  (setf *n* (1+ *n*))
  (block nil
    (if (> ne *e*)
	(setf *max* n *e* ne))
    (if (and *max-e* (>= ne *max-e*))
    	(return))
    (if (= 0 ne)
	(loop
	   with m = (* 10 n)
	   and me = (1+ ne)
	   for i from 9 downto 1
	   if (= (mod (+ m i) me) 0)
	   do (maxn (+ m i)me))
	(loop
	   with m = (* 10 n)
	   and me = (1+ ne)
	   for i from 9 downto 0
	   if (= (mod (+ m i) me) 0)
	   do (maxn (+ m i)me)))))
(defun search-num (&optional max-e)
  (setf *max-e* max-e *max* 0 *e* 0 *n* 0)
  (maxn 0 0)
  (values *max* *e* *n*))
	
