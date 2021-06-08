(define-library
    (combinators)
    (import (scheme base)
            (scheme case-lambda))
    (export
        constantly
        complement
        swap
        ;flip
        on-left
        on-right
        conjoin
        disjoin
        each-of
        all-of
        any-of
        on
        left-section
        right-section


        begin-procedure
        if-procedure
        if-not-procedure
        value-procedure
        case-procedure
        and-procedure
        or-procedure
        loop-procedure
        while-procedure
        until-procedure

        always
        never
        boolean
        identity)
    (include "combinators-impl.scm"))
