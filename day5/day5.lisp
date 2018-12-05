;;; This code is nightmare fuel.  It works though.

;;; Staying up til 1am to do this was dumb.

;;; Will fix later


(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun make-constraint (charl)
  (if (upper-case-p charl)
      (list charl (char-downcase charl))
      (list charl (char-upcase charl))))

(defun char-case-pair? (a b)
  (and
   (or
    (and (upper-case-p a) (lower-case-p b))
    (and (lower-case-p a) (upper-case-p b)))
   (char= (char-downcase a) (char-downcase b))))

(defun answer-loop (data acc)
  (cond
    ((null data)
     acc)
    ((null (cdr data))
     (append (reverse acc) (list (car data))))
    ((char-case-pair? (car data) (cadr data))
     (answer-loop (cdr (cdr data)) acc))
    (t
     (answer-loop (cdr data) (cons (car data) acc)))))

(defun answer (data)
  (let ((len (length data)))
    (let ((res (answer-loop data '())))
      (if (= (length res) len)
	  res
	  (answer res)))))

(defun answer2 (data x)
  (let ((constraint (make-constraint x)))
    (answer
     (remove (car constraint)
	     (remove (cadr constraint)
		     data)))))

(defun answer2-map (data)
  (let ((unique (remove-duplicates (mapcar #'char-downcase data))))
    (car (sort (mapcar #'(lambda (x) (length (answer2 data x)))
		  unique)
	  #'<))))
