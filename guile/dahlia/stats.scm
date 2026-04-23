; Usage
;
; $ GUILE_EXTENSIONS_PATH='guile' GUILE_LOAD_PATH='./guile' rlwrap guile
; (use-modules (dahlia stats))

(define-module (dahlia stats)
  #:version (1 0 0)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module ((rnrs) #:version (6))
  #:export (
    erf
  ))

(define erf
  (foreign-library-function "libgsl.so" "gsl_sf_erf"
    #:return-type double
    #:arg-types (list double)))
