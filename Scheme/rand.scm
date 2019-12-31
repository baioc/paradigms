(define (rand)
  (random 19470208))

(define (cesaro)
  (= (gcd (rand) (rand)) 1))

(define (estimate-pi n)
  (sqrt (/ 6 (monte-carlo n cesaro))))

(define (monte-carlo trials experiment)
  (let iter ((trials-remaining trials) (trials-passed 0))
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed)))))


(define (random-in-range low high)
  (+ low (random (- high low))))

(define (estimate-integral pred x1 y1 x2 y2 n)
  (* (- x2 x1)
     (- y2 y1)
     (monte-carlo n
       (lambda () (pred (random-in-range x1 x2)
                        (random-in-range y1 y2))))))

; (estimate-integral (lambda (x y) (<= (+ (* x x) (* y y)) 1.0)) -1.0 -1.0 1.0 1.0 1000000)
