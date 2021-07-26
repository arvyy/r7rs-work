(define-library
    (toposort)
    (import
        (scheme base)
        (scheme case-lambda)
        (srfi 1))
    (export topological-sort)
    (include "toposort-impl.scm"))
