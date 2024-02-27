;
;	GEOM.LSP - 1998 - Bigdogs Internescional.
;
;	Geometria piana.
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; geom
;;;

; classe base oggetti geometrici

(defclass geom () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; vect
;;;

; vettore o punto

(defclass vect (geom)
  ((x :accessor vect-x
      :initarg :x
      :initform 0)
   (y :accessor vect-y
      :initarg :y
      :initform 0)))

(defmethod point ((x number) (y number))
  (make-instance 'vect :x x :y y))

(defmethod mod ((v vect))
  ; modulo
  (let ((x (vect-x v)) (y (vect-y v)))
    (sqrt (+ (* x x) (* y y)))))

(defmethod scale ((v vect) (n number))
  ; prodotto per uno scalare
  (let ((x (vect-x v)) (y (vect-y v)))
    (make-instance 'vect :x (* n x) :y (* n y))))

(defmethod sum ((v1 vect) (v2 vect))
  ; somma vettoriale
  (let ((x (+ (vect-x v1) (vect-x v2)))
	(y (+ (vect-y v1) (vect-y v2))))
    (make-instance 'vect :x x :y y)))

(defmethod diff ((v1 vect) (v2 vect))
  ; differenza vettoriale
  (let ((dx (- (vect-x v1) (vect-x v2)))
	(dy (- (vect-y v1) (vect-y v2))))
    (make-instance 'vect :x dx :y dy)))

(defmethod dist ((p1 vect) (p2 vect))
  ; distanza fra due punti
  (mod (diff p1 p2)))

(defmethod versor ((v vect))
  ; estrazione del versore
  (let ((x (vect-x v)) (y (vect-y v)) (m (mod v)))
    (if (= m 0)
      (point 0 0)
      (make-instance 'vect :x (/ x m) :y (/ y m)))))

(defmethod rot90 ((v vect))
  ; rotazione di 90 gradi in senso antiorario
  (let ((x (vect-x v)) (y (vect-y v)))
    (make-instance 'vect :x (- y) :y x)))

(defmethod rot-90 ((v vect))
  ; rotazione di 90 gradi in senso orario
  (let ((x (vect-x v)) (y (vect-y v)))
    (make-instance 'vect :x y :y (- x))))

(defmethod scalar ((v1 vect) (v2 vect))
  ; prodotto scalare
  (+
    (* (vect-x v1) (vect-x v2))
    (* (vect-y v1) (vect-y v2))))

(defmethod midpoint ((p1 vect) (p2 vect))
  (scale (sum p1 p2) 0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; straight
;;;

; retta - forma ax + by = c con a^2 + b^2 = 1

(defclass straight (geom)
  ((a :accessor straight-a
      :initarg :a
      :initform 0)
   (b :accessor straight-b
      :initarg :b
      :initform 1)
   (c :accessor straight-c
      :initarg :c
      :initform 0)))

(defmethod ortho ((r straight))
  (point (straight-a r) (straight-b r)))

(defmethod dir ((r straight))
  (rot90 (ortho r)))

(defmethod straight ((p1 vect) (p2 vect))
  ; retta per due punti
  (let* ((dir (versor (diff p2 p1)))
	 (dir90 (rot90 dir)) 
	 (c (scalar dir90 p1)))
    (make-instance 'straight :a (vect-x dir90) :b (vect-y dir90) :c c)))

(defmethod axis ((p1 vect) (p2 vect))
  ; asse di un segmento
  (let* ((dir90 (versor (diff p2 p1)))
	 (m (scale (sum p1 p2) 0.5))
	 (c (scalar dir90 m)))
    (make-instance 'straight :a (vect-x dir90) :b (vect-y dir90) :c c)))

(defmethod intersect ((r1 straight) (r2 straight))
  (let* ((a1 (straight-a r1))
	 (b1 (straight-b r1))
	 (c1 (straight-c r1))
	 (a2 (straight-a r2))
	 (b2 (straight-b r2))
	 (c2 (straight-c r2))
	 (det (det2x2 a1 b1 a2 b2)))
    (if (= det 0)
      nil
      (list (point
	      (/ (det2x2 c1 b1 c2 b2) det)
	      (/ (det2x2 a1 c1 a2 c2) det))))))

(defun det2x2 (a b c d)
  (- (* a d) (* b c)))

(defmethod dist ((p vect) (r straight))
  ; distanza fra punto e retta
  (abs (- (straight-c r)
	  (scalar p (ortho r)))))

(defmethod dist ((r straight) (p vect))
  (dist p r))

(defmethod project ((p vect) (r straight))
  ; proiezione di un punto su una retta
  (let* ((o (ortho r))
	 (d (- (scalar p o) (straight-c r)))
	 (var (scale o d)))
    (diff p var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass circle (geom)
  ((r :accessor circle-r
      :initarg :r
      :initform 1)
   (x :accessor circle-x
      :initarg :x
      :initform 0)
   (y :accessor circle-y
      :initarg :y
      :initform 0)))

(defmethod center ((c circle))
  (point (circle-x c) (circle-y c)))

(defmethod circle2 ((c vect) (r number))
  ; cerchio dati centro e raggio
  (make-instance 'circle :r r :x (vect-x c) :y (vect-y c)))

(defmethod circle2 ((r number) (c vect))
  (circle2 c r))

(defmethod circle3 ((p1 vect) (p2 vect) (r number))
  ; cerchio dati due punti ed il raggio
  (let* ((dp (/ (dist p1 p2) 2))
	 (s (sqrt (- (* r r) (* dp dp))))
	 (a (axis p1 p2)))
    (sum (midpoint p1 p2) (scale (dir a) s))))

(defmethod circle3 ((p1 vect) (p2 vect) (p3 vect))
  ; cerchio dati tre punti
  (let*
    ((c (car (intersect (axis p1 p2) (axis p2 p3))))
     (r (dist p1 c)))
    (circle2 c r)))

(defmethod intersect ((r straight) (c circle))
  ; intersezione tra retta e cerchio
  (let ((d (dist r (center c)))
	(radius (circle-r c)))
    (cond
      ((> d radius)
       nil)
      ((= d radius)
       (list (project (center c) r)))
      (t
	(let* ((p (project (center c) r))
	       (s (sqrt (- (* radius radius) (* d d))))
	       (sv (scale (dir r) s)))
	  (list (sum p sv) (diff p sv)))))))

(defmethod intersect ((c circle) (r straight))
  (intersect r c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :geom)

