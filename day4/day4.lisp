
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun month (timestr)
  (subseq timestr 6 8))

(defun day (timestr)
  (subseq timestr 9 11))

(defun year (timestr)
  (subseq timestr 1 5))

(defun hour (timestr)
  (subseq timestr 12 14))

(defun minute (timestr)
  (subseq timestr 15 17))

(defun time-to-str (timestr)
  (let ((month (month timestr))
	(day (day timestr))
	(hour (hour timestr))
	(minute (minute timestr)))
    (parse-integer (concatenate 'string month day hour minute))))

(defun sort-data (data)
  (sort data #'< :key #'time-to-str))

(defparameter *ghash* (make-hash-table))

(defun guard-id (timestr)
  (subseq timestr 26 30))

(defun action-id (timestr)
  (subseq timestr 19 24))
  
(defun add-guard-sleep-time (begin end hash)
  (dotimes (temp (- end begin) temp)
    (if (gethash (+ temp begin) hash)
	(setf (gethash (+ temp begin) hash) (+ 1 (gethash (+ temp begin) hash)))
	(setf (gethash (+ temp begin) hash) 1)))
  hash)
      

     
(defun guard-total (id id-table)
  (let ((res '()))
    (maphash #'(lambda (c y)
			     (push (list c y) res))
	     id-table)
    (format t "Guard ~a is ~a and highest minute is ~a~%" id (reduce #'+ res :key #'second)
	    (first (sort res #'> :key #'second)))))
	  
(defun solve ()
  (progn
    (clrhash *ghash*)
    (guard-parse (sort-data (get-file "input")) 0 0)
    (maphash #'(lambda (c y)
		 (guard-total c y)) *ghash*)))

(defun enumerate (lst)
  (let ((len (length lst)))
    (mapcar #'list lst (alexandria:iota len))))

(defparameter *opcodes* (enumerate '(sga sfa swa ret add sum hig cpg cpe cpl low hgn phr mic pdm pcm)))

(defun guard-parse (data cur-guard offset)
  (cond
    ((null data))
    ((string= "Guard" (action-id (car data)))
     (unless (gethash (parse-integer (guard-id (car data))) *ghash*)
       (setf (gethash (parse-integer (guard-id (car data))) *ghash*) (make-hash-table)))
     (guard-parse (cdr data) (parse-integer (guard-id (car data))) 0))
    
    ((string= "falls" (action-id (car data)))
     (guard-parse (cdr data) cur-guard (parse-integer (minute (car data)))))
    
    ((string= "wakes" (action-id (car data)))
     (add-guard-sleep-time offset (parse-integer (minute (car data)))
			   (gethash cur-guard *ghash*))
     (guard-parse (cdr data) cur-guard offset))
    
    (t
     '())))

(defun assemble-parse (data)
  (cond
    ((null data)
     '())
    ((string= "Guard" (action-id (car data)))
     (cons (list 'sga (parse-integer (guard-id (car data))))
	   (assemble-parse (cdr data))))
    ((string= "falls" (action-id (car data)))
     (cons (list 'sfa (parse-integer (minute (car data))))
	   (assemble-parse (cdr data))))
    ((string= "wakes" (action-id (car data)))
     (cons (list 'swa (parse-integer (minute (car data))))
	   (cons (list 'mic 0)
		 (assemble-parse (cdr data)))))))

(defun repeat-zero (n)
  (if (<= n 1)
      "0"
  (concatenate 'string "0" (repeat-zero (- n 1)))))

(defun decimal-to-binary (n bits)
  (let ((intermediate (format nil "~b" n)))
    (if (> (length intermediate) bits)
	(error "Buffer overflow in assembly")
	(if
	 (< (length intermediate) bits)
	 (concatenate 'string (repeat-zero (- bits (length intermediate))) intermediate)
	 intermediate))))

(defparameter *test* (assemble-parse (get-file "input-sorted")))

(defun opcode-to-binary (data)
  (mapcar #'(lambda (x)
	      (concatenate 'string
			   (decimal-to-binary (cadr (assoc (car x) *opcodes*)) 8)
			   " "
			   (decimal-to-binary (cadr x) 16)))
	  data))

(defun output-binary-file (data name)
  (with-open-file (fd name :direction :output)
    (mapcar #'(lambda (x) (format fd "~a~%" x))
	    data)
    (format fd "~a~%" (car (opcode-to-binary '((hig 0)))))))
