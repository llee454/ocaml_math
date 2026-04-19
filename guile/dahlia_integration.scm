; This library is the entry point into the Dahlia Integration Scheme library.
; It loads the necessary extensions and exposes functions defined in the
; Guile Dahlia extension.
;
; Usage
;
; (load "./dahlia_integration.scm")
; (use-modules (dahlia integration))

(define-module (dahlia integration)
  #:version (1 0 0)
  #:export (
    make-t t-out t-err t-neval
    make-qag-params-t
    qag-params-t-epsabs 
    qag-params-t-epsrel
    qag-params-t-limit 
    qag
  ))

(import (rnrs (6)))

(load-extension
  (string-append
    (dirname (current-filename))
    "/guile_dahlia.so")
  "init")

(define-record-type t
  (fields out err neval))

(define (list->t x)
  (make-t
    (list-ref x 0)
    (list-ref x 1)
    (list-ref x 2)))

(define-record-type qag-params-t
  (fields epsabs epsrel limit))

(define (qag-params-t->list x)
  (list
    (qag-params-t-epsabs x)
    (qag-params-t-epsrel x)
    (qag-params-t-limit x)))

; Accepts four arguments:
; * params, a qag-params-t record that specifies the error bounds and iteration limits
; * f, a function that accepts a real number and returns a real number
; * lower, a real number
; * upper, a real number
; integrates f over the interval lower to upper and returns a t record
; type.
;
; This function uses the Quadrature Adaptive Gauss (QAG) algorithm
; provided by the GNU Scientific Library (GSL) (and ultimately from the famed
; QUADPACK library).
(define (qag params f lower upper)
  (list->t
    (dahlia-qag
        (qag-params-t->list params)
        f lower upper)))
