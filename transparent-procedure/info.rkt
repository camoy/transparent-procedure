#lang info

;; general

(define name "transparent-procedure")
(define collection "transparent-procedure")
(define pkg-desc "Full implementation of transparent-procedure")
(define version "0.0")
(define pkg-authors '(camoy))
(define license 'Apache-2.0)
(define scribblings
  '(["scribblings/transparent-procedure.scrbl" ()]))

;; dependencies

(define deps
  '("base"
    "transparent-procedure-lib"))

(define implies
  '("transparent-procedure-lib"))

(define build-deps
  '("rackunit-lib"
    "racket-doc"
    "sandbox-lib"
    "scribble-lib"))
