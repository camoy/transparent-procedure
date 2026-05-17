#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@require[@for-label[transparent-procedure
                    racket/base
		    racket/contract
		    racket/dict
		    racket/match
		    rackunit]
	 racket/sandbox
	 scribble/example]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; constants

@(define transparent-procedure-eval
   (make-base-eval '(require racket/dict
			     racket/match
			     transparent-procedure
			     rackunit)))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Transparent Procedures}
@author{Cameron Moy}

@defmodule[transparent-procedure]

Procedures are opaque values that,
at least in principle,
have no properties other than their behavior upon application.
In practice,
most programming languages aren't so disciplined.
@examples[#:eval transparent-procedure-eval #:label #f
  (equal? add1 add1)]
Equality of functions in Racket,
as in many other languages,
is defined as pointer equality.
This capability is not strictly kosher.
@examples[#:eval transparent-procedure-eval #:label #f
  (equal? add1 (lambda (x) (add1 x)))]
An η-expanded version of @racket[add1] yields a different result,
meaning that η-expansion does not preserve all observations of a function.
This lapse is a deliberate choice
to ensure that @racket[equal?] remains a total function.

Usually one does not want to compare functions,
but there are situations where it's desirable.
For instance,
most memoization implementations use a hash table
to cache previous results.
As a consequence,
memoization of higher-order functions is thwarted
by extensionally equivalent program transformations
such as η expansion.
The potential of cache misses for memoized higher-order functions
does not affect correctness,
so it may not seem like much of a problem.
However,
it's actually possible to @emph{perfectly} memoize higher-order functions.
The idea is to track all interactions with a higher-order input
and store a first-order representation of its extensional behavior.
Determining if a future application is in cache
requires testing if inputs behave (extensionally) the same
as anything recorded in the memo table.

This library provides @emph{transparent procedures}
that automatically construct these first-order representations.
Given a procedure,
@racket[make-procedure] returns a transparent version
that records all interactions with the underlying value.
@examples[#:eval transparent-procedure-eval #:label #f
  (define inc (make-procedure (λ (x) (+ x 1))))
  inc]
Initially,
the procedure has no recorded behavior so it's printed empty.
@examples[#:eval transparent-procedure-eval #:label #f
  (void (inc 1) (inc 2) (inc 3))
  inc]
Printing after three applications
shows a @emph{partial graph} of the function.
Transparent procedures whose partial graphs can be made compatible
are considered @racket[equal?].
@examples[#:eval transparent-procedure-eval #:label #f
  (define add1′ (make-procedure add1))
  (void (add1′ 3) (add1′ 4) (add1′ 5))
  (equal? inc add1′)]
After checking for equality,
the partial graphs of each function are equal (as sets).
@examples[#:eval transparent-procedure-eval #:label #f
  inc
  add1′]

Intuitively,
think of @racket[make-procedure] as similar to constructors
of other mutable data structures such as @racket[make-hash].
However,
instead of being able to mutate the data structure using an operation
such as @racket[hash-set!],
application of a transparent procedure mutates the underlying data structure
based on the input and output.
As with other mutable data structures,
@racket[equal?] is interpreted as "equal now,"
and @racket[eq?] is interpreted as "equal always."
The result of @racket[equal?] may change based on future interactions
with the transparent procedure that reveal more of its graph.

Because higher-order functions nest arbitrarily,
the graph has to be deep,
in the sense that higher-order arguments and results
must themselves be transparent too.
@examples[#:eval transparent-procedure-eval #:label #f
  (define map′ (make-procedure map))
  (void (map′ add1 '(1 2 3)))
  (void (map′ sub1 '(4 5 6)))
  (eval:alts
   map′
   (eval:result
    @racketresultfont[
@literal["(procedure\n"
"  [ (procedure [ 6 . 5 ] [ 5 . 4 ] [ 4 . 3 ]) '(4 5 6) . '(3 4 5) ]\n"
"  [ (procedure [ 3 . 4 ] [ 2 . 3 ] [ 1 . 2 ]) '(1 2 3) . '(2 3 4) ]"]]))]
Because @racket[map′] is transparent,
it "infects" the higher-order inputs @racket[add1] and @racket[sub1].
A consequence of this behavior is that @racket[make-procedure]
works correctly only for data that is inspectable.
@examples[#:eval transparent-procedure-eval #:label #f
  (struct unknown ())
  (define values′ (make-procedure values))
  (eval:error (values′ (unknown)))]

Transparent procedures are dictionaries
that allow access to the partial graph.
@examples[#:eval transparent-procedure-eval #:label #f
  (define (f x) (if (zero? x) 0 (add1 x)))
  (define map′-curried (dict-ref map′ f))
  map′-curried
  (dict-ref map′-curried '(1 2 3))]
@margin-note{
  The nested structure of this dictionary makes it a trie
  where every argument is a "character."
}
Even though @racket[f] is not the same function as @racket[add1],
the dictionary lookup is successful
because it behaves extensionally the same
as far as @racket[map′] is concerned.
The @racket[dict-ref] function takes one argument at a time,
leading to currying behavior.
@examples[#:eval transparent-procedure-eval #:label #f
  (eval:error (dict-ref map′-curried '(10 11 12)))
  (map′ f '(10 11 12))
  (dict-ref map′-curried '(10 11 12))]

The partial graph of a pure function grows monotonically over time.
Impure functions,
however,
can modify existing entries like any other mutable data structure.
@examples[#:eval transparent-procedure-eval #:label #f
  (define counter
    (make-procedure
     (let ([state 0])
       (λ ()
	 (set! state (add1 state))
	 state))))
  (begin (counter) counter)
  (begin (counter) counter)]

Coming back to the original example,
higher-order memoization works by making the underlying procedure transparent
and then retrieving cached entries through the dictionary interface.
@examples[#:eval transparent-procedure-eval #:label #f
  (define (memo f)
    (define g (make-procedure f))
    (define absent (gensym))
    (λ args
      (let go ([g g] [args args])
	(match args
	  [(list) g]
	  [(cons arg args-rest)
	   (let ([g′ (dict-ref g arg absent)])
	     (if (eq? g′ absent)
		 (apply g args)
		 (go g′ args-rest)))]))))]
Here,
@racket[g] is equivalent to the hash table of an ordinary memoization function.
Upon application,
the memoized function checks for each argument according to @racket[g].
If the argument is not present,
then the result is computed.
Otherwise,
the cached value is returned.
@examples[#:eval transparent-procedure-eval #:label #f
  (define filter′
    (memo
     (λ (f xs)
       (displayln "cache miss")
       (filter f xs))))
  (filter′ integer? '(1 0.5 3))
  (filter′ (λ (x) (integer? x)) '(1 0.5 3))]

@section{Reference}

@defproc[(make-procedure [v procedure?]) transparent-procedure?]{
  Returns a transparent version of @racket[v]
  that incrementally builds a partial graph of its behavior.
  Transparent procedures are comparable with
  @racket[equal?] and @racket[procedure-includes?].
  A transparent procedure is also an immutable dictionary.
}

@defproc[(procedure-includes? [x transparent-procedure?] [y procedure?]) boolean?]{
  @margin-note{
    The order imposed by @racket[procedure-includes?]
    is precisely the same as that for function domains:
    f ⊑ g if and only if for all x in the domain of f,
    f(x) ⊑ g(x).
  }
  Returns whether the partial graph of @racket[x] is contained within the behavior of @racket[y].
  This function interacts with @racket[y] according to every entry in the partial graph of @racket[x]
  in order to compute the result.
  @examples[#:eval transparent-procedure-eval #:label #f
    (define abs′ (make-procedure abs))
    (void (abs′ 1) (abs′ -2))
    (procedure-includes? abs′ (λ (x) (if (> x 0) x (- x))))
    (procedure-includes? abs′ values)]
}

@defproc[(transparent-procedure? [v any/c]) boolean?]{
  Returns whether @racket[v] is a transparent procedure.
}

@section{Prior Art}

Higher-order memoization was first described by Ralf Hinze @cite{Hinze 2000}
as a passing remark.
It was explored further by Conal Elliott
in a @hyperlink["http://conal.net/blog/posts/memoizing-higher-order-functions"]{series of blog posts},
which became the foundation for
a @hyperlink["https://hackage.haskell.org/package/MemoTrie"]{memoization library}
for Haskell.
This library is an adaptation of those ideas
in ways that are ergonomic for Racket.

@(bibliography
  (bib-entry
   #:key "Hinze 2000"
   #:title "Memo functions, polytypically!"
   #:is-book? #f
   #:author "Ralf Hinze"
   #:location "Workshop on Generic Programming (WGP)"
   #:date "2000"
   #:url "https://www.cs.ox.ac.uk/people/ralf.hinze/publications/WGP00b.ps.gz"))
