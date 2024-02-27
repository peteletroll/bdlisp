;
;	MUSIC.LSP - 2010 - Bigdogs Internescional.
;
;	Note, intervalli e accordi.
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; le basi
;;;

(defparameter *note-names* '(c c+ d d+ e f f+ g g+ a a+ b))

(defparameter *notes-per-octave* (length *note-names*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; nota musicale: nome e ottava
;;;

(defclass note ()
  ((name :accessor note-name
      :initarg :name
      :initform 'c)
   (octave :accessor note-octave
      :initarg :octave
      :initform 4)))

(let ((counter 0))
  (dolist (note *note-names*)
    (setf (get note 'note-number) counter)
    (incf counter)))

(defmethod note ((name symbol) (octave number))
  (unless (member name *note-names*)
    (error "undefined note name: ~s" name))
  (unless (= octave (floor octave))
    (error "not integer octave: ~s" octave))
  (make-instance 'note :name name :octave octave))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; calcoli con gli intervalli
;;;

(defun reduce-to-octave (integer)
  (let ((rem (rem (floor integer) *notes-per-octave*)))
    (if (minusp rem)
	(+ rem *notes-per-octave*)
	(abs rem))))

(defun note-to-integer (note)
  (+ (get (note-name note) 'note-number)
     (* *notes-per-octave* (note-octave note))))

(defun integer-to-note (integer)
  (let ((octave (floor (/ integer
			  *notes-per-octave*))))
         (note (nth (floor (- (floor integer)
			      (* *notes-per-octave* octave)))
		    *note-names*)
	       octave)))

(defmethod interval ((note1 note) (note2 note))
  (- (note-to-integer note2)
     (note-to-integer note1)))

(defmethod transpose ((note note) (interval number))
  (integer-to-note (+ (note-to-integer note) interval)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; circolo delle quinte
;;;

(defparameter *fifth-interval* (interval (note 'c 4) (note 'g 4)))

(defparameter *harmonic-distance-table* (make-vector *notes-per-octave* *notes-per-octave*))

(let ((interval 0))
  (dotimes (distance *notes-per-octave*)
    (let ((adjusted-distance (if (> distance (/ *notes-per-octave* 2))
                                 (- distance *notes-per-octave*)
				 distance)))
      (setf (vref *harmonic-distance-table* interval) adjusted-distance)
      (setf interval (reduce-to-octave (+ interval *fifth-interval*))))))

(defmethod harmonic-distance ((note1 note) (note2 note))
  (vref *harmonic-distance-table*
	(reduce-to-octave (interval note1 note2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; frequenze
;;;

(defun interval-to-ratio (interval)
  (expt 2 (/ interval *notes-per-octave*)))

(defparameter *note-zero-frequency* 1)

(defun tune (note frequency)
  (setf *note-zero-frequency* (/ frequency
				 (interval-to-ratio (note-to-integer note)))))

(tune (note 'a 4) 440)

(defmethod frequency ((note note))
  (* *note-zero-frequency* (interval-to-ratio (note-to-integer note))))

