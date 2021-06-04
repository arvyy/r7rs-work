(define-library
    (define-optional)
    (import (scheme base)
            (scheme case-lambda)
            (srfi 148))
    (export define-optional)
    (include "define-optional-impl.scm"))
