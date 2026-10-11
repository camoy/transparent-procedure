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
         racket/format
         racket/function
         racket/list
         racket/match
         racket/string
         racket/struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define (KEYS-NOT-FOUND keys)
  (error 'dict-ref "no value found for keys ~a"
         (string-join (map ~v keys))))

(define unq unquoted-printing-string)
(define no-default (gensym))
(struct some (val))

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
    (define full-xs′ (map inject xs))
    (define y′ (inject (apply proc full-xs′)))
    ;; It's important we compute y′ first because that will expand the xs′.
    (let go ([xs′ full-xs′] [tree tree])
      (match xs′
        [(list)
         (gvector-set! tree 0 (leaf y′))
         y′]
        [(cons x′ xt′)
         ;; The existing trie entry has to match exactly for us to reuse it.
         ;; Additionally, transparent-procedure-equal? ensures that we don't
         ;; mutate keys while looking at it.
         (or (tree-ref tree x′ transparent-procedure-equal? (curry go xt′))
             (let ([node (make-gvector)])
               (gvector-add! tree (cons x′ node))
               (go xt′ node)))])))

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
  [(define (dict-ref self keys [default KEYS-NOT-FOUND])
     (unless (list? keys)
       (raise-argument-error 'dict-ref "list?" keys))
     (match-define (transparent-procedure proc tree) self)
     (define (failed)
       (cond
         [(eq? default KEYS-NOT-FOUND) (KEYS-NOT-FOUND keys)]
         [(procedure? default) (default)]
         [else default]))
     (let/cc return
       (let go ([keys keys] [tree tree])
         (match keys
           [(list)
            (when (contains-leaf? tree)
              (return (extract-at-leaf tree)))]
           [(cons key keys-rst)
            ;; This checks `(procedure-includes? trie-entry key)` which is guaranteed
            ;; not to expand `trie-entry` but it may end up expanding `key` if that
            ;; happens to be itself a transparent procedure.
            (tree-ref tree key procedure-includes? (curry go keys-rst))]))
       (failed)))

   (define (dict-iterate-first self)
     (define tree (transparent-procedure-tree self))
     (and (gvector? tree) 0))

   (define (dict-iterate-next self pos)
     (define tree (transparent-procedure-tree self))
     (define pos′ (add1 pos))
     (and (< pos′ (gvector-count tree)) pos′))

   (define (dict-iterate-key self pos)
     (define tree (transparent-procedure-tree self))
     (car (gvector-ref tree pos)))

   (define (dict-iterate-value self pos)
     (define proc (transparent-procedure-proc self))
     (define tree (transparent-procedure-tree self))
     (define key (car (gvector-ref tree pos)))
     (match (cdr (gvector-ref tree pos))
       [(? contains-leaf? (app extract-at-leaf v)) v]
       [y (transparent-procedure (curry proc key) y)]))]

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

(define (tree-ref tree key matches? kont)
  (let go ([idx 0])
    (cond
      [(< idx (gvector-count tree))
       (match-define (cons x y) (gvector-ref tree idx))
       (par-or (and (matches? x key) (kont y))
               (go (add1 idx)))]
      [else #f])))

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
       ;; For our purposes, we can actually keep this as `for/and`
       ;; since it occurs only within a `par-or`.
       (for/and ([x+subtree (in-gvector tree)])
         (match-define (cons x subtree) x+subtree)
         (define args′ (cons x args))
         (match subtree
           [(? contains-leaf? (app extract-at-leaf y))
            (procedure-includes? y (apply f (reverse args′)))]
           [_ (go args′ subtree)])))]
    [_ (equal? v f)]))

(define (transparent-procedure-equal? x y)
  (match* (x y)
    [((transparent-procedure _ x-tree) (transparent-procedure _ y-tree))
     (equal? (flatten-tree x-tree) (flatten-tree y-tree))]
    [(_ _) (equal? x y)]))

(define (flatten-tree v)
  (let go ([v v])
    (for/hash ([x+subtree (in-gvector v)])
      (match-define (cons x subtree) x+subtree)
      (match subtree
        [(? contains-leaf? (app extract-at-leaf y)) (values x y)]
        [_ (values x (go subtree))]))))

(define (contains-leaf? x)
  (and (positive? (gvector-count x))
       (let ([x (gvector-ref x 0)])
         (and x (leaf? x)))))

(define (extract-at-leaf x)
  (leaf-value (gvector-ref x 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private

(module private racket/base
  (provide #;for/and/par
           par-or-prompt-tag
           par-or)

  (require racket/control
           racket/match
           racket/generator)

  (define par-or-prompt-tag (make-continuation-prompt-tag))
  (struct suspended (kont))

  (define-syntax-rule (par-or l r)
    (par-or-fn (λ () l) (λ () r)))

  #;(define-syntax-rule (par-and l r)
    (par-and-fn (λ () l) (λ () r)))

  #;(define-syntax-rule (for/and/par ([x y]) body ...)
    (let ([next! (sequence->generator y)])
      (let go ()
        (define x (next!))
        (if (void? x) #t (par-and (let () body ...) (go))))))

  (define (par-or-fn l r)
    (match (call/prompt (λ () (l)) par-or-prompt-tag (λ (val) (suspended val)))
      [#f (r)]
      [(suspended l-val)
       (match (call/prompt (λ () (r)) par-or-prompt-tag (λ (val) (suspended val)))
         [#f #f]
         [(suspended r-val)
          (abort/cc par-or-prompt-tag (list l-val r-val))]
         [y y])]
      [y y]))

  #;(define (par-and-fn l r)
    (match (call/prompt (λ () (l)) par-or-prompt-tag (λ (val) (suspended val)))
      [#f #f]
      [(suspended l-val)
       (match (call/prompt (λ () (r)) par-or-prompt-tag (λ (val) (suspended val)))
         [#f #f]
         [(suspended r-val)
          (abort/cc par-or-prompt-tag (list l-val r-val))]
         [y
          (abort/cc par-or-prompt-tag (list l-val))])]
      [y (r)])))

(require 'private)
