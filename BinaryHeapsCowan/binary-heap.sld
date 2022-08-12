(define-library
  (binary-heap)
  (import (scheme base)
          (srfi 128))
  (cond-expand
    ((library (srfi 145)) (import (srfi 145)))
    (else (begin
            (define-syntax assume
              (syntax-rules ()
                ((assume expression message ...)
                 (or expression
                     (error "invalid assumption" (quote expression) (list message ...))))
                ((assume . _)
                 (syntax-error "invalid assume syntax")))))))
  (export
    make-heap
    heap?
    heap-comparator
    heap-count
    heap-key

    heap-insert!
    heap-pop!
    heap-top
    heap-delete!
    heap-map
    heap-empty?
    heap-size)
  (include "binary-heap-impl.scm"))
