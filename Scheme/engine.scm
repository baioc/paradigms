(define *infinity* (/ 1 0.0))

(define clock
  (let ((stopped? #t)
        (clock-interrupt-handler (lambda () (error "Clock interrupt!"))))
    (let ((generate-clock-interrupt
           (lambda ()
             (set! stopped? #t)
             (clock-interrupt-handler))))
      (sigaction SIGALRM (lambda (sig) (generate-clock-interrupt)))
      (lambda (msg val)
        (cond ((eq? msg 'set-handler)
               (set! clock-interrupt-handler val))
              ((eq? msg 'set)
               (cond
                 ;; stops the clock and returns its remaining time
                 ((= val *infinity*)
                  (let ((time-remaining (alarm 0)))
                    (if stopped?
                        *infinity*
                        (begin
                          (set! stopped? #t)
                          time-remaining))))
                 ;; sets the alarm to go off immediately
                 ;; also returns the previously remaining time
                 ((= val 0)
                  (let ((time-remaining (alarm 0)))
                    (if stopped?
                        (begin
                          (generate-clock-interrupt)
                          *infinity*)
                        (begin
                          (generate-clock-interrupt)
                          time-remaining))))
                 ;; sets up the clock to generate an interrupt after val seconds
                 ;; also returns the previously remaining time
                 (else
                  (let ((time-remaining (alarm val)))
                    (if stopped?
                        (begin
                          (set! stopped? #f)
                          *infinity*)
                        time-remaining)))))
              (else (error "undefined operation -- CLOCK" msg)))))))


(define *engine-escape* #f)
(define *engine-entrance* #f)
(clock 'set-handler (lambda () (call/cc *engine-escape*)))

(define (make-engine thunk)
  (lambda (ticks success failure)
    (let* ((ticks-left 0)
           (engine-succeeded? #f)
           (result
             ;; captures the continuation of an abortive engine's thunk
             (call/cc
               (lambda (k)
                 (set! *engine-escape* k)
                 (let ((result
                         ;; runs the engine, saving entrance continuation
                         (call/cc
                           (lambda (k)
                             (set! *engine-entrance* k)
                             (clock 'set ticks)
                             (*engine-entrance* (thunk))))))
                    ;; if thunk finishes, *engine-entrance* goes here
                    (set! ticks-left (clock 'set *infinity*))
                    (set! engine-succeeded? #t)
                    result)))))
      (if engine-succeeded?
          (success result ticks-left)
          (failure (make-engine (lambda () (result 'resume))))))))


(define fibonacci-engine
  (make-engine
    (lambda ()
      (let loop ((n 0) (prev 1) (curr 0))
        (display "fibonacci ")
        (display n)
        (display " -> ")
        (display curr)
        (newline)
        (loop (+ n 1) curr (+ prev curr))))))

(define *more* #f)
(fibonacci-engine 1 list (lambda (new-engine) (set! *more* new-engine)))
