(define-syntax push!
  (syntax-rules ()
    ((_ (proc arg ...) item)
     ((setter proc) arg ... (cons item (proc arg ...))))
    ((_ variable item)
     (default-set! variable (cons item variable)))))

(define-syntax pop!
  (syntax-rules ()
    ((_ (proc arg ...))
     (let* ((val (proc arg ...)))
       ((setter proc) arg ... (cdr val))
       (car val)))
    ((_ variable)
     (let* ((val variable))
       (default-set! variable (cdr val))
       (car val)))))

(define-syntax inc!
  (syntax-rules ()
    ((_ arg)
     (inc! arg 1))
    ((_ (proc arg ...) delta)
     ((setter proc) arg ... (+ (proc arg ...) delta)))
    ((_ variable delta)
     (default-set! variable (+ variable delta)))))

(define-syntax dec!
  (syntax-rules ()
    ((_ arg)
     (inc! arg -1))
    ((_ arg delta)
     (inc! arg (- delta)))))

(define-syntax update!
  (syntax-rules ()
    ((_ (proc arg ...) updater)
     ((setter proc) arg ... (updater (proc arg ...))))
    ((_ variable updater)
     (default-set! variable (updater variable)))))
