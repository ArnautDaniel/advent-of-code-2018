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

(defun answer2-loop (data constraint acc)
  (cond
    ((null data)
     acc)
    ((null (cdr data))
     (append acc (list (car data))))
    ((or
      (and
       (member (car data) constraint)
       (member (second data) (remove (car data) constraint)))
      (and
       (member (second data) constraint)
       (member (car data) (remove (second data) constraint))))
     (answer2-loop (cdr (cdr data)) constraint acc))
    (t
     (answer2-loop (cdr data) constraint (cons (car data) acc)))))

  (defun answer-loop (data acc)
    (cond
      ((null data)
       acc)
      ((null (cdr data))
       (append (reverse acc) (list (car data))))
      ((and (or
	     (and (upper-case-p (car data)) (lower-case-p (second data)))
	     (and (lower-case-p (car data)) (upper-case-p (second data))))
	    (char= (char-downcase (car data)) (char-downcase (second data))))
       (answer-loop (cdr (cdr data)) acc))
      (t
       (answer-loop (cdr data) (cons (car data) acc)))))

  (defun answer (data)
    (let ((len (length data)))
      (let ((res (answer-loop data '())))
	(if (= (length res) len)
	    res
	    (answer res)))))
  
  (defun answer2 (data constraint)
    (let ((len (length data)))
      (let ((res (answer2-loop data constraint '())))
	(if (= (length res) len)
	    res
	    (answer2 res constraint)))))
