#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(module+ test (require rackunit))
(require transparent-procedure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (let ([map (make-procedure map)])
    (map add1 '(1 2 3))
    (map (λ (x) (* x 2)) '(2 3 4))

    (check-equal? (dict-ref (dict-ref map (λ (x) (+ x 1))) '(1 2 3))
                  '(2 3 4))
    (check-false (dict-ref (dict-ref map (λ (x) (+ x 1))) '(1 2) #f))
    (check-equal? (dict-ref (dict-ref map (curry * 2)) '(2 3 4))
                  '(4 6 8)))

  (let ([inc (make-procedure (λ (x) (if (zero? x) 0 (+ x 1))))]
        [add1 (make-procedure add1)])
    (inc 1)
    (inc 2)
    (add1 1)
    (add1 3)
    (check-true (equal? inc add1))
    (add1 0)
    (check-false (equal? inc add1)))

  (let* ([adder (make-procedure (curry +))]
         [plus10 (adder 10)]
         [plus20 (adder 20)])
    (plus10 3)
    (plus10 4)
    (plus20 5)
    (check-equal? ((dict-ref adder 10) 3) 13)
    (check-equal? ((dict-ref adder 10) 8) 18)
    (check-equal? ((dict-ref adder 20) 5) 25)
    (check-false (dict-ref adder 30 #f))
    )

  (let ()
    (define counter
      (make-procedure
       (let ([state 0])
         (λ (_)
           (set! state (add1 state))
           state))))
    (check-equal?
     (dict-ref (begin (counter (void)) counter) (void))
     1)
    (check-equal?
     (dict-ref (begin (counter (void)) counter) (void))
     2)
    ))
