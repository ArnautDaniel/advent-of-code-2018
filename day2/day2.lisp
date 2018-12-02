(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun answer (data)
  (mapcar #'(lambda (x) (let ((lst x)
			   (fuck (loop for char across x
        collect char)))
			  (remove-if #'(lambda (z) (< z 2))
				     (mapcar #'(lambda (y) (count y x))
					     fuck))))
       data))
		       

(defun difference-between (z x)
  (let ((z0 (loop for char across z
	       collect char))
	(x0 (loop for char across x
	       collect char)))
     (answer-diff z0 x0 0)))

(defun answer-diff (z0 x0 acc)
  (cond
    ((null z0)
    acc)
    ((equalp (car z0) (car  x0))
     (answer-diff (cdr z0) (cdr x0) acc))
    (t
     (answer-diff (cdr z0) (cdr x0) (+ acc 1)))))

(defun answer2 (lst)
  (reverse
   (cdr
    (reverse
     (remove-duplicates
      (flatten
       (mapcar #'(lambda (x)
		   (remove-if-not #'(lambda (x) (equal 1 (cadr x)))
				  (mapcar #'(lambda (z)
					      (list z (difference-between z x)))
					  lst)))
	       lst)))))))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

