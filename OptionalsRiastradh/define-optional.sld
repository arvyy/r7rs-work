(define-library
    (define-optional)
    (import (scheme base)
            (scheme case-lambda))
    (export define-optional)
    (include "define-optional-impl.scm"))
