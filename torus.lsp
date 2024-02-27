; calcolo coperture auditorium

(defun sqr (x)
  (* x x))

(defun ssqrt (x)
  (if (> x 0)
      (sqrt x)
      0))

; r_gen(x, r1, r2) = r1 + ssqrt(r2 ** 2 - x ** 2)

(defun r-gen (x r1 r2)
  (+ r1
     (ssqrt
       (- (sqr r2) (sqr x)))))

; torus(x, y, r1, r2) = abs(x) < r2 ? ssqrt(r_gen(x, r1, r2) ** 2 - y ** 2) : 0

(defun torus (x y r1 r2)
  (if (< (abs x) r2)
      (ssqrt (- (sqr (r-gen x r1 r2)) (sqr y)))
      0))


