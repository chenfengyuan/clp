(defun split-string (string &optional (key #\space))
	   (let* ((pos (append (loop for a = 0 then (1+ b)
		     for b = (position key string :start a)  while b collect b) (list (length string)))))
	     (loop for a = 0 then (1+ b)
		  and b in pos collect (subseq string a b))))

(defun degree->float(a b c)
  (+ a (/ b 60) (/ c 3600)))

(defun read-gcs-from-string(string)
  (apply #'degree->float
	 (mapcar #'read-from-string (split-string string #\,))))

(defun sin-d (x)
  (sin (* pi (/ x 180))))

(defun cos-d (x)
  (cos (* pi (/ x 180))))

(defun gcs->xyz(radius phi lambda)
  (let* ((p (read-gcs-from-string phi))
	 (l (read-gcs-from-string lambda))
	 (x (* radius (cos-d p) (cos-d l)))
	 (y (* radius (cos-d p) (sin-d l)))
	 (z (* radius (sin-d p))))
    (list x y z)))

(defun xyz-distances(x1 y1 z1 x2 y2 z2)
  (sqrt (+ (expt (- x1 x2) 2)
	   (expt (- y1 y2) 2)
	   (expt (- z1 z2) 2))))

(defun gcs-distances(h1 p1 l1 h2 p2 l2)
  (let* ((h-base (/ (+ 6378137.0 6356752.314245) 2)) ;http://en.wikipedia.org/wiki/World_Geodetic_System#A_new_World_Geodetic_System:_WGS_84
	 (r1 (+ h-base h1))
	 (r2 (+ h-base h2))
	 (xyz1 (gcs->xyz r1 p1 l1))
	 (xyz2 (gcs->xyz r2 p2 l2))
	 (x1 (car xyz1))
	 (x2 (car xyz2))
	 (y1 (cadr xyz1))
	 (y2 (cadr xyz2))
	 (z1 (caddr xyz1))
	 (z2 (caddr xyz2)))
    (xyz-distances x1 y1 z1 x2 y2 z2)))