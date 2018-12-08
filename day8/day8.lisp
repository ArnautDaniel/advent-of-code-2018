(defun read-metadata (n data)
  (let ((res '()))
    (dotimes (temp-one  n  temp-one)
      (push (read data nil) res))
    res))

(defun read-nodes (fd)
  (let ((res (read fd nil)))
    (cond
      ((null res)
       0)
      ((= res 0)
	(reduce #'+ (read-metadata (read fd) fd)))
      (t
       (let ((acc '())
	     (metacnt (read fd)))
	  (dotimes (temp res temp)
	    (push (read-nodes fd) acc))
	  (let ((lst (read-metadata metacnt fd)))
	    (reduce #'+
		    (remove-if #'null
			       (mapcar #'(lambda (x)
				(if (< (- x 1) (length acc))
				    (nth (- x 1) (reverse acc))
				    0))
			    lst)))))))))
;;; (reduce #'+ (read-metadata metacnt fd) (reduce #'+ acc)) as the last expression for solution 1
