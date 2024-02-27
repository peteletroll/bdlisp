(require :perm)

(defparameter *alphabet* '(0 1 2 3 4 5 6 7 8 9))
(defparameter *length* 4)

(randomize)

(defun play (&optional (answerer #'user-answerer) &aux attempts (count 0))
  (permutation-map
    #'(lambda (attempt)
	(when
	  (every
	    #'(lambda (previous-attempt)
		(equal
		  (reply attempt (car previous-attempt))
		  (cdr previous-attempt)))
	    attempts)
	  (push (cons attempt (funcall answerer attempt)) attempts)
	  (incf count)))
    *alphabet* *length*)
  count)

(defun reply (attempt solution)
  (list (black-reply attempt solution)
	(white-reply attempt solution)))

(defun black-reply (attempt solution)
  (cond
    ((endp attempt) 0)
    ((endp solution) 0)
    ((eql (car attempt) (car solution))
     (1+ (black-reply (cdr attempt) (cdr solution))))
    (t
      (black-reply (cdr attempt) (cdr solution)))))

(defun white-reply (attempt solution)
  (- (white-reply-aux attempt solution)
     (black-reply attempt solution)))

(defun white-reply-aux (attempt solution)
  (cond
    ((endp attempt) 0)
    ((endp solution) 0)
    ((member (car attempt) solution)
     (1+ (white-reply-aux (cdr attempt) solution)))
    (t
      (white-reply-aux (cdr attempt) solution))))

(defun user-answerer (attempt)
  (ask-to-user attempt))

(defun ask-to-user (attempt)
  (format t "~%combinazione proposta: ~a~%" attempt)
  (list
    (askint "pallini neri? ")
    (askint "pallini bianchi? ")))

(defun askint (prompt)
  (princ prompt)
  (let ((ans (read)))
    (if (and
	  (numberp ans)
	  (= ans (floor ans))
	  (<= 0 ans 9))
      ans
      (askint prompt))))

(defun make-auto-answerer (&optional (solution (new-solution)))
  (format t "~%la soluzione e' ~a~%" solution)
  #'(lambda (attempt)
      (let ((reply (reply attempt solution)))
	(format t "~%combinazione proposta: ~a~%" attempt)
	(format t
		"pallini: neri: ~a, bianchi: ~a~%"
		(car reply)
		(cadr reply))
	reply)))

(defun new-solution (&optional (length *length*) (alphabet *alphabet*))
  (cond
    ((<= length 0) '())
    (t
      (let ((new (random-element alphabet)))
	(cons new (new-solution (1- length) (remove new alphabet)))))))

(defun random-element (list)
  (nth (random (length list)) list))

