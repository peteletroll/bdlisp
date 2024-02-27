(setq hh (make-hash-table :size 2 :test #'equal))

(time
	(dotimes (i 100000)
		; (format t "adding ~s~%" i)
		(setf (gethash (list i) hh) i)
		; (hash-table-dump hh)
	)
)

(time
	(dotimes (i 100000)
		; (format t "adding ~s~%" i)
		(setf (gethash (list i) hh)
		      (intern (format nil "valore-~s" i)))
		; (hash-table-dump hh)
	)
)

(format t "result: ~s~%" hh)
(room)

