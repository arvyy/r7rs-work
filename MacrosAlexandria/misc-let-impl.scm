(define-syntax let-list
  (syntax-rules ()
    ((_ (var ... . rest)
        lst . body)
     (let-list-h () (var ...) rest lst . body))))

(define-syntax let-list-h
  (syntax-rules ()
    ((_ (let-clauses ...)
        (var1 var ...)
        rest
        lst . body)
     (let-list-h (let-clauses ... (var1 (car lst)))
                 (var ...)
                 rest
                 (cdr lst) . body))
    ((_ (let-clauses ...)
        ()
        ()
        lst . body)
     (let (let-clauses ...) . body))
    ((_ (let-clauses ...)
        ()
        rest
        lst . body)
     (let-list-h (let-clauses ... (rest lst))
                 ()
                 ()
                 lst . body))))

(define-syntax if-let
  (syntax-rules ()
    ((_ ((var expr) ...)
        then
        else)
     (let ((var expr) ...)
       (if (and var ...)
           then
           else)))))

(define-syntax if-let*
  (syntax-rules ()
    ((_ ((var expr) ...)
        then
        else)
     (let* ((var expr) ...)
       (if (and var ...)
           then
           else)))))

(define-syntax when-let
  (syntax-rules ()
    ((_ ((var expr) ...) . body)
     (let ((var expr) ...)
       (when (and var ...)
         (let () . body))))))

(define-syntax when-let*
  (syntax-rules ()
    ((_ ((var expr) ...) . body)
     (let* ((var expr) ...)
       (when (and var ...)
         (let () . body))))))


(define-syntax case-using
  (syntax-rules (else =>)
    ((case-using pred (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case-using pred atom-key clauses ...)))
    ((case-using pred key
       (else => result))
     (result key))
    ((case-using pred key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case-using pred key
                 ((atoms ...) => result))
     (if (member key '(atoms ...) pred)
         (result key)))
    ((case-using pred key
                 ((atoms ...) => result)
                 clause clauses ...)
     (if (member key '(atoms ...) pred)
         (result key)
         (case-using pred key clause clauses ...)))
    ((case-using pred key
       ((atoms ...) result1 result2 ...))
     (if (member key '(atoms ...) pred)
         (begin result1 result2 ...)))
    ((case-using pred key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (member key '(atoms ...) pred)
         (begin result1 result2 ...)
         (case-using pred key clause clauses ...)))))

(define-syntax andmap
  (syntax-rules ()
    ((_ proc expr ...)
     (and (proc expr) ...))))

(define-syntax ormap
  (syntax-rules ()
    ((_ proc expr ...)
     (or (proc expr) ...))))
