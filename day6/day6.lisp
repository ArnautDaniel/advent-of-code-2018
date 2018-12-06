
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun parse-data (data)
  (mapcar #'(lambda (y)
	      (list (parse-integer (car y)) (parse-integer (second y))))
	  (mapcar #'(lambda (x) (split-sequence:split-sequence #\, x))
		  (get-file data))))

(defun x-max (data)
  (reduce #'max data :key #'car))

(defun y-max (data)
  (reduce #'max data :key #'cadr))

(defun manhatten-dist (c z)
  ( abs (+
   (abs (- (car c) (car z)))
   (abs (- (cadr c) (cadr z))))))

(defun closet-to (x data)
  (let ((res (mapcar #'(lambda (y)
			 (list (manhatten-dist y x) y))
		     data)))
    (let ((res0 (sort res #'< :key #'(lambda (x) (abs (car x))))))
      (if (not (equal (count (caar res0) res0 :key #'(lambda (x) (abs (car x)))) 1))
	  '()
	  (cadar res0)))))

(defun total-region (x data)
  (let ((res (mapcar #'(lambda (y)
			  (manhatten-dist y x))
		     data)))
    (reduce #'+ res)))

(defun answer (data)
  (let ((max-x (x-max data))
	(max-y (y-max data))
	(res '()) (blocklist '()))
    (dotimes (temp-one (+ max-x 1) temp-one)
      (dotimes (temp-two (+ max-y 1) temp-two)
	(if (or (= temp-one 0) (= temp-one max-x)
		(= temp-two 0) (= temp-two max-y))
	    (push (closet-to (list temp-one temp-two) data)
		  blocklist)
	    (push (closet-to (list temp-one temp-two) data)
		  res))))    
    (set-difference res blocklist)))

(defun answer2 (data)
  (let ((max-x (x-max data))
	(max-y (y-max data))
	(res '()))
    (dotimes (temp-one (+ max-x 1) temp-one)
      (dotimes (temp-two (+ max-y 1) temp-two)
	(let ((res0 (total-region (list temp-one temp-two) data)))
	  (when (> 10000 res0)
	    (push res0 res)))))
     res))

					    
(defun count-graph (data)
  (let ((set0 (remove-duplicates data)))
    (sort (mapcar #'(lambda (y)
		(list (count y data) y))
	    set0) #'> :key #'car)))
    
