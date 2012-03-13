(declaim (optimize (debug 3)))
(defpackage :cfy.7-segment
  (:nicknames :7s)
  (:use :cl))
(in-package :7s)

(defun seg->array (seg)
  (loop with arr = (make-array 8)
     for i from 0 to 7
     if (> (logand seg (ash 1 i)) 0)
     do (setf (elt arr i) 1)
     finally (return arr)))
(defun array->bit (arr)
  (loop with bit = (make-array 8)
     for i across #(2 3 6 5 4 1 0 7)
     for j across arr
     if (= 1 j)
     do (setf (elt bit i) 1)
     finally (return bit)))
(defun bit->qbit (bit q)
  (loop with qbit = (make-array 20)
     for i across bit
     for j across q
     if (= 1 i)
     do (setf (elt qbit j) 1)
     finally (return qbit)))
(defun qbit->seg (qbit)
  (loop
     for i across qbit
     ;;             1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20
     for j across #(-1 0  -1 -1 1  2  -1 -1 3  -1 -1 4  -1 -1 5  6  -1 -1  7 -1)
     if (= 1 i)
     sum (ash 1 j)))

(defun main ()
  (format t "unsigned char seg[]={")
  (loop for i across #(#x3f #x06 #x5b #x4f #x66 #x6d #x7d #x07 #x7f #x6f)
     do (format t "0x~x," (qbit->seg(bit->qbit (array->bit (seg->array i)) #(1 4 8 11 14 15 5 18)))))
  (format t "}"))
