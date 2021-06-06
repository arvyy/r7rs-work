(define-syntax if*
  (syntax-rules ()
    ((_) (syntax-error "Expected at least 2 arguments"))
    ((_ a) (syntax-error "Expected at least 2 arguments"))
    ((_ arg ...) (h () arg ...))))

(define-syntax h
  (syntax-rules ()
    ((_ (c ...) arg1 arg2 arg ...)
     (h (c ... (arg1 arg2)) arg ...))
    ((_ (c ...) arg)
     (cond c ... (else arg)))
    ((_ (c ...))
     (cond c ...))))
