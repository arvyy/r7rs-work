(define-library
    (lambda-star)
    (import (scheme base)
            (scheme case-lambda))
    (export lambda*)
    (include "lambda-star-impl.scm"))
