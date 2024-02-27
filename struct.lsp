;
;	STRUCT.LSP - 1998 - Bigdogs Internescional.
;
;	Strutture.
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; defstruct macro
;;;

(defmacro defstruct (spec &rest ifields)

  (let* ((name (struct-spec-name spec))
	 (super (struct-spec-include spec))
	 (print-function (struct-spec-print-function spec))
	 (include (get super 'struct-definition))
	 (doc (if (stringp (car ifields)) (pop ifields) nil))
	 (fields (append (adjust-struct-fields ifields) include)))

    `(progn

       (put ',name 'struct-definition ',fields)

       (put ',name 'supertype ',super)

       (put ',name 'object-print-function ,print-function)

       (defun
	 ,(symbol-join 'make- name)
	 (,@(cons '&key fields) &aux (new (make-object ',name)))
	 ,@(mapcar
	     #'(lambda (field)
		 `(put new ',(car field) ,(car field)))
	     fields)
	 new)

       (defun
	 ,(symbol-join 'copy- name)
	 (obj &aux (new (make-object ',name)))
	 ,@(mapcar
	     #'(lambda (field)
		 `(put new ',(car field) (get obj ',(car field))))
	     fields)
	 new)

       (defun ,(symbol-join name '-p) (obj)
	 (eq (type-of obj) ',name))

       ,@(mapcan
	   #'(lambda (field-spec)
	       (list
		 `(defun
		    ,(symbol-join name '- (car field-spec))
		    (obj)
		    (get obj ',(car field-spec)))
		 `(defun
		    ,(symbol-join 'set- name '- (car field-spec))
		    (obj val)
		    (put obj ',(car field-spec) val))
		 `(defsetf
		    ,(symbol-join name '- (car field-spec))
		    ,(symbol-join 'set- name '- (car field-spec)))))
	   fields)

       ',name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; auxiliary functions
;;;

(defun struct-spec-name (spec)
  (if (atom spec)
    spec
    (struct-spec-name (car spec))))

(defun struct-spec-include (spec)
  (if (consp spec)
    (cadr (assoc ':include spec))
    'atom))

(defun struct-spec-print-function (spec)
  (if (consp spec)
    (cadr (assoc ':print-function spec))
    '#'struct-instance-printer))

(defun symbol-join (&rest sym-list)
  (intern (apply #'concat sym-list)))

(defun adjust-struct-fields (flds)
  (cond
    ((atom flds)
     nil)
    ((symbolp (car flds))
     (cons (list (car flds) nil)
	   (adjust-struct-fields (cdr flds))))
    (t
     (cons (car flds) (adjust-struct-fields (cdr flds))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; input & output
;;;

(defun struct-instance-printer (object stream)
  (format stream "#S~s"
	  (cons (object-type object)
		(mapcan #'(lambda (fldspec)
			    (list
			      (intern (concat ":" (car fldspec)))
			      (get object (car fldspec))))
			(get (object-type object) 'struct-definition)))))

(set-dispatch-macro-character "#" "S"
			      '(lambda (stream char n)
				 (let ((spec (read stream)))
				   (cons
				     (symbol-join 'make- (car spec))
				     (cdr spec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :struct)

