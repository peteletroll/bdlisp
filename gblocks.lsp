;;;; -*- mode:Lisp; package:user -*- ;;;;
;;;; Created: 10 December 1992
;;;; Copyright 1992 Patrick H. Winston and Berthold K. P. Horn.
;;;; All rights reserved.
;;;;
;;;; Version 1.0.1, copied from master file on 23 Apr 93       
;;;; 
;;;; This software is licensed by Patrick H. Winston and Berthold K. P. Horn
;;;; (licensors) for instructional use with the textbooks ``Lisp,'' by Patrick
;;;; H. Winston and Berthold K. P. Horn, and ``Artificial Intelligence,'' by
;;;; Patrick H. Winston.  Your are free to make copies of this software and
;;;; modify it for such instructional use as long as:
;;;; 1. You keep this notice intact.
;;;; 2. You cause any modified files to carry a prominent notice stating
;;;;    that you modified the files and the date of your modifications.
;;;; This software is licensed ``AS IS'' without warranty and the licensor
;;;; shall have no liability for any alleged defect or damages.

;;;; PROCEDURES

(defclass basic-block ()
  ((name :accessor block-name :initarg :name)
   (width :accessor block-width :initarg :width)
   (height :accessor block-height :initarg :height)
   (position :accessor block-position :initarg :position)
   (supported-by :accessor block-supported-by :initform nil)))

(defmethod draw ((block basic-block))
	(let ((position (block-position block)))
		(draw-block
			block
			*dot*
			(car position)
			(cadr position)
			(block-width block)
			(block-height block)) ) )

(defmethod erase ((block basic-block))
	(let ((position (block-position block)))
		(draw-block
			block
			" "
			(car position)
			(cadr position)
			(block-width block)
			(block-height block)) ) )

(defclass movable-block (basic-block) ())

(defclass load-bearing-block (basic-block) 
  ((support-for :accessor block-support-for :initform nil)))

(defclass brick (movable-block load-bearing-block) ())

(defclass wedge (movable-block) ())
(defclass ball (movable-block) ())

(defclass table (load-bearing-block) ())

(defclass hand ()
  ((name :accessor hand-name :initarg :name)
   (position :accessor hand-position :initarg :position)
   (grasping :accessor hand-grasping :initform nil)))

(defmethod block-support-for ((object basic-block))
  nil)

(defmethod put-on ((object basic-block) (support basic-block))
	  (format t "Sorry, can't move ~a.~%"
			  (block-name object)))

(defmethod put-on ((object movable-block) (support basic-block))
	  (format t "Sorry, ~a is not a support.~%"
			  (block-name support)))

(defmethod put-on ((object movable-block) (support load-bearing-block))
  (if (get-space object support)
	  (and (grasp object)
		   (move object support)
		   (ungrasp object))
	  (format t "Sorry, there is no room for ~a on ~a.~%"
			  (block-name object)
			  (block-name support))))

(defmethod get-space ((object movable-block) (support basic-block))
  (or (find-space object support)
	  (make-space object support)))

(defmethod grasp ((object movable-block))
  (unless (eq (hand-grasping *hand*) object)
	(when (block-support-for object) (clear-top object))
	(when (hand-grasping *hand*)
	  (get-rid-of (hand-grasping *hand*)))
	
	(setf (hand-position *hand*) (top-location object))
	
	(setf (hand-grasping *hand*) object))
  t)

(defmethod ungrasp ((object movable-block))
  (when (block-supported-by object)
	
	(setf (hand-grasping *hand*) nil)
	t))

(defmethod get-rid-of ((object movable-block))
  (put-on object table))

(defmethod make-space ((object movable-block) (support basic-block))
  (dolist (obstruction (block-support-for support))
	(get-rid-of obstruction)
	(let ((space (find-space object support)))
	  (when space (return space)))))

(defmethod clear-top ((support load-bearing-block))
  (dolist (obstacle (block-support-for support) t)
	(get-rid-of obstacle)))

(defmethod remove-support ((object movable-block))
  (let ((support (block-supported-by object)))
	(when support
	  (setf (block-support-for support)
			(remove object (block-support-for support)))
	  (setf (block-supported-by object) nil)
	  t)))

(defmethod add-support ((object movable-block)
						(support basic-block))
  t)

(defmethod add-support ((object movable-block)
						(support load-bearing-block))
  (push object (block-support-for support))
  (setf (block-supported-by object) support)
  t)

(defmethod move :before ((object movable-block) ignored-parameter)
  (let ((support (block-supported-by object)))
	(when support
	  
	  (setf (block-support-for support)
			(remove object (block-support-for support)))
	  (setf (block-supported-by object) nil)
	  t)))

(defmethod move :after ((object movable-block)
						(support load-bearing-block))
  
  (setf (block-support-for support)
		(cons object (block-support-for support)))
  (setf (block-supported-by object) support)
  t)

(defmethod move ((object movable-block) (support basic-block))
  (erase object)
  (let ((newplace (get-space object support)))
	
	(setf (block-position object) newplace))
  (draw object)
  t)

(defmethod (setf block-position)
		   :after
		   (new-position (object movable-block))
  (setf (hand-position *hand*) (top-location object)))

(defun find-space (object support)
  (dotimes (offset (+ 1 (- (block-width support)
						   (block-width object))))
	(unless (intersections-p object offset
							 (first (block-position support))
							 (block-support-for support))
	  (return (list (+ offset (first (block-position support)))
					(+ (second (block-position support))
					   (block-height support)))))))

(defun intersections-p (object offset base obstacles)
  (dolist (obstacle obstacles)
	(let* ((ls-proposed (+ offset base))
		   (rs-proposed (+ ls-proposed (block-width object)))
		   (ls-obstacle (first (block-position obstacle)))
		   (rs-obstacle (+ ls-obstacle (block-width obstacle))))
	  (unless (or (>= ls-proposed rs-obstacle)
				  (<= rs-proposed ls-obstacle))
		(return t)))))

(defun top-location (object)
  (list (+ (first (block-position object))
		   (/ (block-width object) 2))
		(+ (second (block-position object))
		   (block-height object))))

; #-gclisp
; (defmethod print-object ((x basic-block) stream)
;   (format stream "#<block ~a>" (block-name x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *dot* "#")

(defun draw-block (block dot x1 y1 width height)
	(setq x1 (* 3 x1))
	(setq y1 (* 2 y1))
	(setq width (* 3 width))
	(setq height (* 2 height))
	(setq y1 (- 23 y1 height))
	(dotimes (y (- height 1))
		(dotimes (x (- width 1))
			(cursor (+ y y1) (+ x x1))
			(princ dot) ) ) 
	(unless (eql dot " ")
		(cursor y1 x1)
		(princ (block-name block)) )
	(cursor 0 0) )

(defun refresh ()
	(cls)
	(mapc 'draw *blocks*)
	(values) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-blocks-data ()
	(cls)
	(defparameter *blocks*
	 (list 
	  (make-instance 'table :name 'table :width 20 :height 1 :position '(0 -1))
	  (make-instance 'brick :name 'b1 :width 2 :height 2 :position '(0 0))
	  (make-instance 'brick :name 'b2 :width 2 :height 2 :position '(2 0))
	  (make-instance 'brick :name 'b3 :width 4 :height 4 :position '(4 0))
	  (make-instance 'brick :name 'b4 :width 2 :height 2 :position '(8 0))
	  (make-instance 'wedge :name 'w5 :width 2 :height 4 :position '(10 0))
	  (make-instance 'brick :name 'b6 :width 4 :height 2 :position '(12 0))
	  (make-instance 'wedge :name 'w7 :width 2 :height 2 :position '(16 0))
	  (make-instance 'ball  :name 'l8 :width 2 :height 2 :position '(18 0))
	 ))

	(dolist (l *blocks*)
		(set (block-name l) l)
		(draw l) )

	(dolist (l (remove table *blocks*)) 
	  (pushnew l (block-support-for table))
	  (setf (block-supported-by l) table))

	(defparameter *hand* (make-instance 'hand :name '*hand* :position '(0 6)))

)

(init-blocks-data)

