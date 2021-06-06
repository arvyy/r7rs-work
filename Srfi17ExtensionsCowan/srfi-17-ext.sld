(define-library
    (srfi-17-ext)
    (cond-expand
        (guile
            (import
                (rename (scheme base) (set! default-set!))
                (srfi srfi-17)))
        (else
            (import
                (rename (scheme base) (set! default-set!))
                (srfi 17))))
    (export
        push!
        pop!
        inc!
        dec!
        update!)
    (include "srfi-17-ext-impl.scm"))
