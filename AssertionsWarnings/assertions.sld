(define-library
    (assertions)
    (import (scheme base)
            (scheme write))
    (export
        assert
        assertion-object?
        assertion-object-message
        assertion-object-irritants

        warn
        warning-object?
        warning-object-message
        warning-object-irritants)
    (include "assertions-impl.scm"))
