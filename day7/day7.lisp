 ;;; Part 2 is unsolved.  I'm passing on it for now since I
 ;;; need a break and problem 8 is coming out soon.
 ;;; Will come back to it later.
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defparameter *alpha* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun alpha-offset (str)
  (+ 1 (position (char str 0) *alpha* :test #'char=)))

(defun parse-data (data)
  (mapcar #'(lambda (x)
	      (list (subseq x 5 6)
		    (subseq x 36 37)))
	  (get-file data)))

(defun dependant? (x data func)
  (if (member x (mapcar func data) :test #'string=)
      t
      nil))

(defun root-options (x data)
  (remove x data :test-not #'string= :key #'first))

(defun root-remove (x data)
  (remove x data :test #'string= :key #'first))

(defun answer-loop (data contents res)
  (cond
    ((null contents)
     res)
    (t
     (let ((srtie (sort (remove-duplicates contents :test #'string=) #'string<)))
       (let ((res0 (mapcar #'cadr (root-options (car srtie) data))))
	 (cond
	   ((dependant? (car srtie) data #'cadr)
	       (answer-loop data (cdr srtie) res))
	     (t
	       (answer-loop (root-remove (car srtie) data) (append res0 (cdr srtie)) (cons (car srtie) res)))))))))
    
(defun answer (data)
  (let ((lst (remove-duplicates
	      (union (mapcar #'car data) (mapcar #'cadr data))
	      :test #'string=)))
    (let ((root (find-if-not #'(lambda (x) (dependant? x data #'cadr)) lst)))
      (reduce #'(lambda (x y) (concatenate 'string x y))
	      (cons root (reverse (answer-loop
				   (root-remove root data)
				   (remove root lst :test #'string=)
				   '())))))))

(defun create-worker (str)
  (list (+ 60 (alpha-offset str)) str))

(defun worker-tick (lst val)
  (mapcar #'(lambda (x)
	      (list (- (car x) val) (cadr x)))
	  lst))

(defun max-work (workers max-workers lst)
  (let ((len (length lst)))
    (if (>= (- max-workers workers) len)
	len
	(- max-workers workers))))

(defun workers-work (data workers contents max-workers)
  (cond
    ((null contents) 0)
    (t
     (let ((res 0)
	   (working-data (subseq contents 0 (max-work workers max-workers contents))))
       (let* ((lowball (caar (sort working-data #'<)))
	      (ticked (worker-tick working-data lowball))
	      (after-tick (remove-if-not #'(lambda (x) (<= (car x) 0)) ticked)))
	 (+ lowball (workers-work data (- (length working-data) 1)
				  (append (mapcar #'create-worker (car (root-options (cadar after-tick) data)))
					  (remove (cadar after-tick) contents :test #'string= :key #'cadr))
				  max-workers)))))))
	  
(defun answer2 (data workers)
  (let* ((lst (remove-duplicates
	       (union (mapcar #'car data) (mapcar #'cadr data))
	       :test #'string=))
	 (root (find-if-not #'(lambda (x) (dependant? x data #'cadr)) lst))
	 (threads (mapcar #'cadr (root-options root data)))
	 (acc (+ 60 (alpha-offset root)))
	 (contents (sort (mapcar #'create-worker threads) #'< :key #'car)))
    (+ acc (workers-work data 0 contents workers))))
    
    
