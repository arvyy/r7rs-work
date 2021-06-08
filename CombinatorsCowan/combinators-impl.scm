(define (constantly . args)
  (lambda ignored-args
    (apply values args)))

(define (complement proc)
  (lambda (obj)
    (not (proc obj))))

;;TODO flip identical?
(define (swap proc)
  (lambda (obj1 obj2)
    (proc obj2 obj1)))

(define (on-left proc)
  (lambda (obj1 obj2)
    (proc obj1)))

(define (on-right proc)
  (lambda (obj1 obj2)
    (proc obj2)))

(define (conjoin . predicates)
  (case-lambda
    (() #t)
    (args (let loop-args ((args args))
            (if (null? args)
                #t
                (let ((arg (car args)))
                  (let loop-preds ((predicates predicates))
                    (cond
                     ((null? predicates) (loop-args (cdr args)))
                     ((not ((car predicates) arg)) #f)
                     (else (loop-preds (cdr predicates)))))))))))

(define (disjoin . predicates)
  (case-lambda
    (() #t)
    (args (let loop-args ((args args))
            (if (null? args)
                #t
                (let ((arg (car args)))
                  (let loop-preds ((predicates predicates))
                    (cond
                     ((null? predicates) #f)
                     (((car predicates) arg) (loop-args (cdr args)))
                     (else (loop-preds (cdr predicates)))))))))))

(define (each-of . procs)
  (lambda args
    (for-each
     (lambda (proc) (apply proc args))
     procs)))

;;TODO spec syntax
(define (all-of predicate)
  (lambda (lst)
    (let loop ((lst lst))
      (cond
       ((null? lst) #t)
       ((predicate (car lst)) (loop (cdr lst)))
       (else #f)))))

;;TODO empty case good?
(define (any-of predicate)
  (lambda (lst)
    (if (null? lst)
        #t
        (let loop ((lst lst))
          (cond
           ((null? lst) #f)
           ((predicate (car lst)) #t)
           (else (loop (cdr lst))))))))

(define (on reducer mapper)
  (lambda objs
    (apply reducer (map mapper objs))))

(define (left-section proc . args)
  (lambda objs
    (apply proc (append args objs))))

(define (right-section proc . args)
  (let ((args-reverse (reverse args)))
    (lambda objs
      (apply proc (append objs args-reverse)))))

(define (begin-procedure . thunks)
  (let loop ((value (if #f #f))
             (thunks thunks))
    (if (null? thunks)
        value
        (loop ((car thunks))
              (cdr thunks)))))

(define if-procedure
  (case-lambda
    ((value then-thunk)
     (if value
         (then-thunk)))
    ((value then-thunk else-thunk)
     (if value
         (then-thunk)
         (else-thunk)))))

(define (if-not-procedure value else-thunk)
  (if value
      (if #f #f)
      (else-thunk)))

(define (value-procedure value then-proc else-thunk)
  (if value
      (then-proc value)
      (else-thunk)))

(define case-procedure
  (case-lambda
    ((value thunk-alist) (case-procedure value thunk-alist (lambda args (if #f #f))))
    ((value thunk-alist else-thunk)
     (cond
      ((assv value thunk-alist) => (lambda (entry)
                                     ((cdr entry))))
      (else (else-thunk))))))

(define and-procedure
  (case-lambda
    (() #t)
    (thunks (let loop ((thunks thunks))
              (cond
               ((null? (cdr thunks)) ((car thunks)))
               ((not ((car thunks))) #f)
               (else (loop (cdr thunks))))))))

(define or-procedure
  (case-lambda
    (() #f)
    (thunks (let loop ((thunks thunks))
              (cond
               ((null? thunks) #f)
               (((car thunks)) => identity)
               (else (loop (cdr thunks))))))))

(define (loop-procedure thunk)
  (thunk)
  (loop-procedure thunk))

(define (while-procedure thunk)
  (if (thunk)
      (while-procedure thunk)
      #f))

(define (until-procedure thunk)
  (define v (thunk))
  (if v
      v
      (until-procedure thunk)))

(define (always . args) #t)

(define (never . args) #f)

(define (boolean obj)
  (if obj #t #f))

(define (identity obj) obj)
