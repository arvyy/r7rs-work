(define-library
    (conditions)
    (import (except (scheme base)
                    error-object?
                    file-error?
                    read-error?
                    error-object-message
                    error-object-irritants)
            (prefix (only (scheme base)
                          error-object?
                          file-error?
                          read-error?
                          error-object-message
                          error-object-irritants)
                    base-)
            (srfi 1)
            (srfi 222))
    (cond-expand
      ((library (rnrs conditions))
       (import (prefix (rnrs conditions (6)) r6rs-)))
      (else
        (begin
          (define (r6rs-condition? obj) #f)
          (define (r6rs-record-rtd obj) #f)
          (define (r6rs-record-type-parent obj) #f)
          (define (r6rs-simple-conditions obj) #f)
          (define (r6rs-message-condition? obj) #f)
          (define (r6rs-condition-message obj) #f)
          (define (r6rs-irritants-condition? obj) #f)
          (define (r6rs-condition-irritants obj) #f))))
    (export
      condition?
      condition-of-type?
      condition-types
      error-object?
      file-error?
      read-error?
      error-object-message
      error-object-irritants)
    (include "conditions-impl.scm"))
