;
;	MATH.LSP - 2003 - Bigdogs Internescional.
;
;	Calcolo.
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; gradi/radianti
;;;

(defun d2r (x)
  (* x #.(/ pi 180)))

(defun r2d (x)
  (* x #.(/ 180 pi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; math-eql - uguaglianza fra reali
;;;

(defvar *reltol* epsilon)

(defvar *abstol* *reltol*)

(defun math-eql (a b)
  (let ((d (abs (- a b))) (m (abs (/ (+ a b) 2))))
    (cond
      ((< d *abstol*)
       t)
      ((zerop m)
       nil)
      ((< (/ d m) *reltol*)
       t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; coefficienti binomiali
;;;

(defun choose (m n)
  (cond
    ((<= n 0) 1)
    ((<= m 0) 0)
    (t
     (* (choose (1- m) (1- n)) m (/ n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; scomposizione in fattori primi
;;;

(defun prime-factors (n &key trace)
  (let ((ret nil) (limit (sqrt n)) (i 2))
    (do-loop
      (when trace
        (format t "; (prime-factors): test ~a~%" i))
      (when (> i limit)
	(return (cons n ret)))
      (let ((q (/ n i)))
	(cond
	  ((zerop (rem q 1))
	   (push i ret)
	   (setq n q limit (sqrt n)))
	  (t
	   (incf i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; probabilita'
;;;

(defun prob-and (&rest args)
  (reduce #'prob-and-2 args))

(defun prob-and-2 (a b)
  (* a b))

(defun prob-or (&rest args)
  (reduce #'prob-or-2 args))

(defun prob-or-2 (a b)
  (+ a b (- (prob-and-2 a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; operazioni statistiche
;;;

(defun sum (list)
  (apply #'+ list))

(defun sum-square (list)
  (apply #'+ (mapcar #'(lambda (x) (* x x)) list)))

(defun sum-product (list1 list2)
  (sum (mapcar #'* list1 list2)))

(defun average (list)
  (if (endp list)
    0
    (/ (sum list) (length list))))

(defun stddev (list)
  (let ((avg (average list)))
    (average (mapcar #'(lambda (x) (expt (- x avg) 2)) list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; solve - soluzione equazioni con bisezione
;;;

(defun solve (fun &key (value 0) (lower 0) (upper (1+ lower)) trace)
  "solve equation with bisection"
  (let ((vfun #'(lambda (x)
		  (- (funcall fun x) value))))
    (solve-expand
      vfun
      lower (funcall vfun lower)
      upper (funcall vfun upper)
      trace 0)))

(defun solve-expand (f a fa b fb trace count)
  (when trace
    (format t "EXP(~s) [ ~s -> ~s, ~s -> ~s ]~%" count a fa b fb))
  (when (> count 1000)
    (error "expansion limit reached"))
  (cond
    ((< (* fa fb) 0)
     (solve-reduce f a fa b fb trace (1+ count)))
    ((< (abs fa) (abs fb))
     (let ((new-a (- a (- b a))))
       (solve-expand f new-a (funcall f new-a) b fb trace (1+ count))))
    (t
      (let ((new-b (+ b (- b a))))
	(solve-expand f a fa new-b (funcall f new-b) trace (1+ count))))))

(defun solve-reduce (f a fa b fb trace count)
  (let* ((c (/ (+ a b) 2)) (fc (funcall f c)))
    (when trace
      (format t "RED(~s) [ ~s -> ~s, ~s -> ~s, ~s -> ~s ]~%" count a fa c fc b fb))
    (cond
      ((eql a c)
       (values c fc))
      ((eql b c)
       (values c fc))
      ((<= (* fa fc) 0)
       (solve-reduce f a fa c fc trace (1+ count)))
      (t
	(solve-reduce f c fc b fb trace (1+ count))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; integrazione numerica con regola di Simpson adattiva
;;;

(defvar *simpson-min-steps* 4)

(defvar *simpson-max-steps* 20)

(defun simpson-drive (f a b tol &key trace)
  (simpson-step f
		a (funcall f a)
		b (funcall f b)
		(/ (+ a b) 2) (funcall f (/ (+ a b) 2))
		0 (abs tol) 0 trace))

(defun simpson-step (f a fa b fb c fc q tol level trace)
  (when trace
    (format t "SIMPSON ~va[ ~s ~s ] tol=~s~%" level "" a b tol))
  (when (> level *simpson-max-steps*)
    (error "integration limit reached"))
  (let* ((h4 (/ (- b a) 4))
	 (d (+ a h4))
	 (fd (funcall f d))
	 (e (- b h4))
	 (fe (funcall f e))
	 (ql (* h4 (/ 3) (+ fa (* 4 fd) fc)))
	 (qr (* h4 (/ 3) (+ fc (* 4 fe) fb)))
	 (err (abs (- q ql qr))))
    (when trace
      (format t "SIMPSON ~va  [ ERROR ~s ]~%" level "" err))
    (if (and (> level *simpson-min-steps*) (<= err tol))
      (+ ql qr)
      (+
	(simpson-step f a fa c fc d fd ql (/ tol 2) (1+ level) trace)
	(simpson-step f c fc b fb e fe qr (/ tol 2) (1+ level) trace)))))

(defun simpson-single-step (f a b)
  (let* ((c (/ (+ a b) 2))
	 (fa (funcall f a))
	 (fb (funcall f b))
	 (fc (funcall f c)))
    (*
      (/ 6)
      (- b a)
      (+
	fa
	(* 4 fc)
	fb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun humps (x)
  (+
    (/
      (+
	(expt (- x .3) 2)
	.01))
    (/
      (+
	(expt (- x .9) 2)
	.04))
    -6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :math)

