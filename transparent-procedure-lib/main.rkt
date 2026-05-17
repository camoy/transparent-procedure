#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [make-procedure
   (-> procedure? transparent-procedure?)]
  [transparent-procedure?
   predicate/c]
  [procedure-includes?
   (-> transparent-procedure? procedure? boolean?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require data/gvector
         racket/dict
         racket/function
         racket/list
         racket/match
         racket/struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define ((KEY-NOT-FOUND key))
  (error 'dict-ref "no value found for key ~v" key))

(define unq unquoted-printing-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implementation

(define (make-procedure proc)
  (transparent-procedure proc (make-gvector)))

(struct leaf (value) #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match-define (leaf value) self)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (lambda (p port) (print p port mode))]))
     (recur value port))])

(struct transparent-procedure (proc tree)
  #:property prop:procedure
  (λ (self . xs)
    (match-define (transparent-procedure proc tree) self)
    (let go ([xs xs] [xs′ null] [tree tree])
      (match xs
        [(list)
         (define y′ (inject (apply proc (reverse xs′))))
         (gvector-set! tree 0 (leaf y′))
         y′]
        [(cons x xt)
         (define x′ (inject x))
         (define-values (subtree idx) (tree-ref tree x))
         (cond
           [idx
            (go xt (cons x′ xs′) subtree)]
           [else
            (define node (make-gvector))
            (gvector-add! tree (cons x′ node))
            (go xt (cons x′ xs′) node)])])))

  #:methods gen:equal+hash
  [(define (equal-proc self other _recur)
     (if (transparent-procedure? other)
         (and (procedure-includes? self other _recur)
              (procedure-includes? other self _recur))
         (eq? self other)))

   (define (hash-proc self recur)
     (recur (transparent-procedure-tree self)))

   (define (hash2-proc self recur)
     (recur (transparent-procedure-tree self)))]

  #:methods gen:dict
  [(define (dict-ref self key [default (KEY-NOT-FOUND key)])
     (match-define (transparent-procedure proc tree) self)
     (define-values (y idx) (tree-ref tree key))
     (if idx
         (match y
           [(? contains-leaf? (app extract-at-leaf v)) v]
           [_ (transparent-procedure (curry proc key) y)])
         (if (procedure? default) (default) default)))

   (define (dict-iterate-first self)
     (define tree (transparent-procedure-tree self))
     (and (cons? tree) 0))

   (define (dict-iterate-next self pos)
     (define tree (transparent-procedure-tree self))
     (define pos′ (add1 pos))
     (and (< pos′ (length tree)) pos′))

   (define (dict-iterate-key self pos)
     (define tree (transparent-procedure-tree self))
     (car (list-ref tree pos)))

   (define (dict-iterate-value self pos)
     (define tree (transparent-procedure-tree self))
     (cdr (list-ref tree pos)))]

  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (const 'procedure)
      (λ (self)
        (match-define (transparent-procedure _proc tree) self)
        (let go ([tree tree] [pre '()])
          (match tree
            [(? contains-leaf? (app extract-at-leaf v))
             (append (cons (unq "[") (reverse pre))
                     (list (unq ".") v (unq "]")))]
            [_
             (define result
               (for/list ([k+v (in-gvector tree)])
                 (match-define (cons k v) k+v)
                 (go v (cons k pre))))
             (apply append result)])))))])

(define (tree-ref tree key)
  (let go ([idx 0])
    (cond
      [(< idx (gvector-count tree))
       (match-define (cons x y) (gvector-ref tree idx))
       (if (procedure-includes? x key)
           (values y idx)
           (go (add1 idx)))]
      [else
       (values #f #f)])))

(define (inject v)
  (match v
    [(? procedure?) (make-procedure v)]
    [(? cons?) (cons (inject (car v)) (inject (cdr v)))]
    [(app prefab-struct-key (? prefab-key? key))
     (define flds (cdr (vector->list (struct->vector v))))
     (apply make-prefab-struct key (map inject flds))]
    [(? flat?) v]
    [_ (error 'make-procedure "cannot inspect ~v" v)]))

(define flat?
  (or/c null?
        boolean?
        number?
        string?
        bytes?
        char?
        symbol?
        void?
        regexp?
        byte-regexp?
        keyword?))

(define (procedure-includes? v f [equal? equal?])
  (match v
    [(transparent-procedure _proc tree)
     (let go ([args (list)] [tree tree])
       (for/and ([x+subtree (in-gvector tree)])
         (match-define (cons x subtree) x+subtree)
         (define args′ (cons x args))
         (match subtree
           [(? contains-leaf? (app extract-at-leaf y))
            (procedure-includes? y (apply f (reverse args′)))]
           [_ (go args′ subtree)])))]
    [_ (equal? v f)]))

(define (contains-leaf? x)
  (and (positive? (gvector-count x))
       (let ([x (gvector-ref x 0)])
         (and x (leaf? x)))))

(define (extract-at-leaf x)
  (leaf-value (gvector-ref x 0)))
