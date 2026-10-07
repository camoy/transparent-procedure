#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(module+ test (require rackunit))
(require transparent-procedure
         transparent-procedure/synchronize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (let ([map (make-procedure map)])
    (map add1 '(1 2 3))
    (map (λ (x) (* x 2)) '(2 3 4))

    (check-equal? (dict-ref map (list (λ (x) (+ x 1)) '(1 2 3)))
                  '(2 3 4))
    (check-false (dict-ref map (list (λ (x) (+ x 1)) '(1 2)) #f))
    (check-equal? (dict-ref map (list (curry * 2) '(2 3 4)))
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
    (check-equal? ((dict-ref adder (list 10)) 3) 13)
    (check-equal? ((dict-ref adder (list 10)) 8) 18)
    (check-equal? ((dict-ref adder (list 20)) 5) 25)
    (check-false (dict-ref adder (list 30) #f)))

  (let ()
    (define counter
      (make-procedure
       (let ([state 0])
         (λ (_)
           (set! state (add1 state))
           state))))
    (check-equal?
     (dict-ref (begin (counter (void)) counter) (list (void)))
     1)
    (check-equal?
     (dict-ref (begin (counter (void)) counter) (list (void)))
     2))

  (let ()
    (define-values (run proxy handle)
      (synchronize add1))

    (define sum 0)
    (run (λ () (set! sum (+ (handle 0) sum))))
    (run (λ () (set! sum (+ (handle 2) (handle 1) sum))))

    (check-equal? sum 0)
    (check-equal? (proxy 0) 1)
    (check-equal? sum 1)
    (check-equal? (proxy 1) 2)
    (check-equal? sum 1)
    (check-equal? (proxy 2) 3)
    (check-equal? sum 6))

  (let ()
    (define-values (run-map proxy-map handle-map) (synchronize map))
    (define-values (run-add1 proxy-add1 handle-add1) (synchronize add1))
    (define result #f)
    (run-map
     (λ ()
       (run-add1
        (λ ()
          (proxy-map add1 '(7))
          (proxy-map add1 '(1 2))
          (proxy-add1 1)
          (proxy-add1 2)
          (set! result (handle-map handle-add1 '(1 2)))))))
    (check-equal? result '(2 3)))

  (let ()
    (define incr!
      (let ([num 0])
        (λ ()
          (set! num (add1 num))
          num)))

    (define-values (run proxy handle)
      (synchronize incr!))

    (check-equal? (proxy) 1)
    (check-exn exn:fail? (λ () (proxy))))
  )
