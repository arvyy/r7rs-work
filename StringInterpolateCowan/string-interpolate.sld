(define-library
    (string-interpolate)
    (import (scheme base)
            (scheme mapping)
            (scheme write))
    (export string-interpolate)
    (include "string-interpolate-impl.scm"))
