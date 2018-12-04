;;; Side effects could be taken out of this code
;;; by wrapping some closures and having it return a *hash* object
;;; but personally I'm already worn out for today

;;; Answer 1 takes 62ms
;;; Answer 2 takes 49ms

;;; That's acceptable to me for running a high level language
;;; Could probably get much better in C
;;; Also why am I not using block quotes?

(defparameter *hash* (make-hash-table))

(defun range (min max &optional (step 1))
  (when (<= min max)
    (cons min (range (+ min step) max step))))

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun parse-data (data)
  (mapcar #'(lambda (x)
	      (mapcar #'parse-integer (split-sequence:split-sequence #\, x)))
	  data))

(defun data (file)
  (parse-data (get-file file)))

(defun into-rectangle (coor)
  (let ((rl (+ (car coor) (third coor)))
	(bl (+ (second coor) (fourth coor))))
    (list (list (first coor) rl)
	   (list (second coor) bl))))

(defun create-hash-row (n hashtbl)
  (setf (gethash n hashtbl) (make-hash-table)))

(defun insert-row-rect (n hashtbl)
  (if (gethash n hashtbl)
      (incf (gethash n hashtbl))
      (setf (gethash n hashtbl) 1)))

(defun insert-rect-into-hash (rect)
  (let ((iter0 (abs (- (second (first rect))
		       (first (first rect)))))
	(iter1 (abs (- (second (second rect))
		       (first (second rect))))))
    (dotimes (temp-one iter0 temp-one)
      (let ((res (gethash (+ temp-one (first (first rect))) *hash*)))
	(if res
	    (dotimes (temp-two iter1 temp-two)
	      (insert-row-rect (+ temp-two (first (second rect))) res))
	    (progn
	      (create-hash-row (+ temp-one (first (first rect))) *hash*)
	      (let ((res0 (gethash (+ temp-one (first (first rect))) *hash*)))
		(dotimes (temp-two iter1 temp-two)
		  (insert-row-rect (+ temp-two (first (second rect))) res0)))))))))

(defun range-rectangle (rect)
  (let ((base (alexandria:iota (second  (second rect))
			      :start  (first (second rect)))))
    base))

(defun rect-single-row (rect-range answer)
  (let ((len (length rect-range))
	(len0 (length (intersection rect-range answer))))
    (if (= len0 len)
	t
	'f)))

(defun find-rectangle (rect)
    (let ((iter0 (abs (- (second (first rect))
		       (first (first rect)))))
	(iter1 (abs (- (second (second rect))
		       (first (second rect))))))
      (let ((maxval (* iter0 iter1))
	    (inter '()))
	(dotimes (temp-one iter0 temp-one)
	  (let ((res (gethash (+ temp-one (first (first rect))) *hash*)))
	    (dotimes (temp-two iter1 temp-two)
	      (push (gethash (+ temp-two (first (second rect))) res) inter))))
	(when (= maxval (reduce #'+ inter))
	  rect))))

(defun answer2 (data)
  (let ((form (mapcar #'into-rectangle data)))
    (remove-if #'null (mapcar #'find-rectangle form))))

(defun answer (data)
  (let ((form (mapcar #'into-rectangle data))
	(res 0))
    (mapcar #'insert-rect-into-hash form)
    (maphash #'(lambda (x y)
		 (maphash #'(lambda (c z) (if (> z 1) (incf res) 0))
			  y)) *hash*)
    res))



