#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [synchronize
   (-> procedure? (values procedure? procedure? procedure?))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (submod "main.rkt" private)
         "main.rkt"
         racket/control
         racket/dict
         racket/list
         racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define ABSENT (gensym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implementation

(struct mapping (args kont tag))

(define (synchronize f)
  (define tag (make-continuation-prompt-tag))
  (define g (make-procedure f))
  (define suspended '())
  (define (run thk)
    (define (handler konts)
      (define-values (here there)
        (partition (λ (m) (eq? (mapping-tag m) tag)) (flatten konts)))
      (set! suspended (append here suspended))
      (unless (empty? there)
        (abort/cc par-or-prompt-tag there)))
    (call/prompt (λ () (call/prompt thk par-or-prompt-tag handler)) tag))
  (define (proxy . args)
    (define old (dict-ref g args ABSENT))
    (define result (apply g args))
    (unless (or (eq? old ABSENT) (equal? old result))
      (error 'synchronize "not extensionally pure"))
    (define-values (succ fail)
      (partition (λ (m) (equal? (mapping-args m) args)) suspended))
    (set! suspended fail)
    (for ([m (in-list succ)])
      ((mapping-kont m) result))
    result)
  (define (handle . args)
    (define result (dict-ref g args ABSENT))
    (cond
      [(eq? result ABSENT)
       (call/comp*
        (λ (kont)
          (abort/cc par-or-prompt-tag (list (mapping args kont tag))))
        tag)]
      [else result]))
  (values run proxy handle))

(define (call/comp* proc tag)
  (call/comp
   (λ (kont)
     (define (kont* x)
       (call/prompt (λ () (kont x)) tag void))
     (proc kont*))
   tag))
