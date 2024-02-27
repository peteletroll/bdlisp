;
;	HASH.LSP - 2005 - Bigdogs Internescional.
;
;	Hashtables.
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (defstruct (hash-table (:print-function #'hash-table-print))
(defstruct hash-table
  (test #'eql)
  (size 65)
  (count 0)
  (vector (make-vector size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sxhash (obj)
  (string-hash (string obj)))

(defun hash-table-module (hash-table)
  (vector-size (hash-table-vector hash-table)))

(defun hash-table-bucket-index (key hash-table)
  (rem (sxhash key) (hash-table-module hash-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gethash (key hash-table &optional default)
  (let* ((vector (hash-table-vector hash-table))
	 (bucket (hash-table-bucket-index key hash-table))
	 (list (vref vector bucket))
	 (test (hash-table-test hash-table))
	 (previous (assoc key list :test test)))
    (if previous
      (values (cdr previous) t)
      (values default nil))))

(defun puthash (key hash-table value)
  (let* ((vector (hash-table-vector hash-table))
	 (bucket (hash-table-bucket-index key hash-table))
	 (list (vref vector bucket))
	 (test (hash-table-test hash-table))
	 (previous (assoc key list :test test)))
    (cond
      (previous
	(rplacd previous value))
      (t
	(incf (hash-table-count hash-table))
	(setf (vref vector bucket)
	      (cons (cons key value) list))
	(hash-table-autorehash hash-table)))
    value))

(defun remhash (key hash-table)
  (let* ((vector (hash-table-vector hash-table))
	 (bucket (hash-table-bucket-index key hash-table))
	 (list (vref vector bucket))
	 (test (hash-table-test hash-table))
	 (return))
    (setf (vref vector bucket)
	  (remove-if
	    #'(lambda (entry)
		(and
		  (funcall test key (car entry))
		  (setq return t)))
	    list))
    (when return
      (decf (hash-table-count hash-table)))
    return))

(defun maphash (fun hash-table)
  (let ((vector (hash-table-vector hash-table)))
    (dotimes (i (vector-size vector))
      (mapc
	#'(lambda (entry)
	    (funcall fun (car entry) (cdr entry)))
	(vref vector i)))))

(defun clrhash (hash-table)
  (setf (hash-table-vector hash-table)
	(make-vector (hash-table-size hash-table)))
  (setf (hash-table-count hash-table) 0)
  hash-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsetf gethash (key hash-table) (new-value)
	 `(puthash ,key ,hash-table new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hash-table-rehash (hash-table new-size)
  (let ((tmp (make-hash-table :size new-size :test (hash-table-test hash-table))))
    (maphash #'(lambda (key value) (puthash key tmp value)) hash-table)
    (dolist (prop (getprops hash-table))
      (setf (get hash-table prop) (get tmp prop)))
    hash-table))

(defun hash-table-autorehash (hash-table)
  (let ((size (hash-table-size hash-table))
	(count (hash-table-count hash-table)))
    (when (> count (* .2 size size))
      (hash-table-rehash hash-table (1+ (* 2 (1- size)))))
    hash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hash-table-print (hash-table stream)
  (format stream "#<hash-table>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hash-table-dump (hash-table)
  (let ((v (hash-table-vector hash-table)))
    (dotimes (i (vector-size v))
      (let ((l (vref v i)))
	(when l
	  (format t "~5d ~5d ~s~%" i (length l) l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :hash)

