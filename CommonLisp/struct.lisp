(defstruct vec2d x y)

(defun distance2d (a b)
  (sqrt (+ (expt (- (vec2d-x a) (vec2d-x b)) 2)
           (expt (- (vec2d-y a) (vec2d-y b)) 2))))

(defun colinearp (a b c)
  (= 0 (+ (* (vec2d-x a) (- (vec2d-y b) (vec2d-y c)))
          (* (vec2d-x b) (- (vec2d-y c) (vec2d-y a)))
          (* (vec2d-x c) (- (vec2d-y a) (vec2d-y b))))))

(defun triangularp (a b c)
  (not (colinearp a b c)))

(setq origin (make-vec2d :x 1.61 :y 0))
(setf (vec2d-x origin) 0)
