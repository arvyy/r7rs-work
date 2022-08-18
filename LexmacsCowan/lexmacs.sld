(define-library
    (lexmacs)
    (import (scheme base)
            (scheme case-lambda)
            (scheme read)
            (scheme write)
            (scheme eval)
            (srfi 1))
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
        make-lexenv
        add-to-lexenv!
        lexmacs-internalize
        lexmacs-externalize
        lexmacs-read
        lexmacs-write
        lexmacs-eval)
    (include "lexmacs-impl.scm"))
