(define-library
    (iteration-cl)
    (import (scheme base))
    (export
        while
        until
        do-times
        do-list
        tagged-begin)
    (include "iteration-cl-impl.scm"))
