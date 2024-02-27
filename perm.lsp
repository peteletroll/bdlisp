;
;	PERM.LSP - 2006 - Bigdogs Internescional.
;
;	Combinations and permutations.
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun permutation-map (fun list &optional (length (length list)))
  (cond
    ((endp list) nil)
    (t
      (permutation-map-aux fun list nil length))))

(defun permutation-map-aux (fun list suffix level)
  (cond
    ((or (endp list) (<= level 0))
     (funcall fun (reverse suffix)))
    (t
      (dolist (i list)
	(permutation-map-aux fun
			     (remove i list)
			     (cons i suffix)
			     (1- level))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cartesian-product-map (fun &rest arglists)
  (cond
    ((endp arglists) nil)
    (t
      (cartesian-product-map-aux fun () arglists)
      nil)))

(defun cartesian-product-map-aux (fun reversed-prefix arglists)
  (if (endp arglists)
    (apply fun (reverse reversed-prefix))
    (mapc
      #'(lambda (i)
	  (cartesian-product-map-aux fun
				     (cons i reversed-prefix)
				     (cdr arglists)))
      (car arglists))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :perm)

