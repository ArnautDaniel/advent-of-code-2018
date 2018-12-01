(defun hash-fold (cur data found)
  (let ((try-find (hash-iterate cur  data '() found)))
    (if (numberp try-find)
	try-find
	(hash-fold  (car try-find) data (append  (cdr try-find) found)))))


(defun hash-iterate (cur data finder found)
  (cond
    ((null data)
     (cons cur (reverse finder)))
    ((member (+ cur (car data)) found)
     (car (member (+ cur (car data)) found)))
    (t
     (hash-iterate (+ cur (car data)) (cdr data)
		   (cons (+ cur (car data)) finder) found))))

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun data-to-int (data)
  (mapcar #'(lambda (x) (if (string= "" x)
			    0
			    (parse-integer x))) data))

;;;Incredibly slow solution,  should be using a hash table at least.  After a few passes 'member' would have to search
;;;the length of data at the very least.
