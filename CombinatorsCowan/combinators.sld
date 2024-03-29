(define-library
    (combinators)
    (import (scheme base)
            (scheme case-lambda))
    (cond-expand
        (guile (import (srfi srfi-1)))
        (else (import (srfi 1))))
    (export
        constantly
        complement
        swap
        flip
        on-left
        on-right
        conjoin
        disjoin
        each-of
        all-of
        some-of
        on
        left-section
        right-section
        apply-chain
        arguments-all
        arguments-any
        arguments-drop
        arguments-drop-right
        arguments-take
        arguments-take-right
        group-by

        begin-procedure
        if-procedure
        when-procedure
        unless-procedure
        value-procedure
        case-procedure
        lazy-and-procedure
        eager-and-procedure
        lazy-or-procedure
        eager-or-procedure
        loop-procedure
        while-procedure
        until-procedure

        always
        never
        boolean
        identity)
    (include "combinators-impl.scm"))
