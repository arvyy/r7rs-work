(define-record-type <assertion>
  (assertion message irritants)
  assertion-object?
  (message assertion-object-message)
  (irritants assertion-object-irritants))

(define-record-type <warning>
  (warning message irritants)
  warning-object?
  (message warning-object-message)
  (irritants warning-object-irritants))

(define-syntax assert
  (syntax-rules ()
    ((_ obj message irritant ...)
     (begin
       (unless (string? message)
         (error "assert message must be a string"))
       (unless obj
         (raise (assertion message (list irritant ...))))))))

(define-syntax warn
  (syntax-rules ()
    ((_ obj message irritant ...)
     (begin
       (unless (string? message)
         (error "warn message must be a string"))
       (unless obj
         (let ((returned (raise-continuable (warning message (list irritant ...)))))
           (display returned (current-error-port))))))))
