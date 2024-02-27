;
;	ROCKET.LSP - 2014 - Bigdogs Internescional.
;
;	Rocket science!
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *g0* 9.81)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rocket ()
  ((mass :accessor rocket-mass
      :initarg :mass
      :initform 1)
   (fuel :accessor rocket-fuel
      :initarg :fuel
      :initform 0)
   (isp :accessor rocket-isp
      :initarg :isp
      :initform 0)))

(defmethod rocket-dry-mass ((rocket rocket))
  (- (rocket-mass rocket) (rocket-fuel rocket)))

(defmethod delta-v ((rocket rocket))
  (* *g0*
     (rocket-isp rocket)
     (log (/ (rocket-mass rocket)
             (rocket-dry-mass rocket)))))

