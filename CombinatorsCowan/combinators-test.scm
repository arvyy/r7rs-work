(cond-expand
  (guile
   (import (scheme base)
           (combinators)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (combinators)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (combinators)
           (srfi 64))))


(test-begin "Combinators")

(test-group
 "constantly"

 (test-equal '(1 2)
   (call-with-values
       (lambda () ((constantly 1 2) 'a 'b))
     list))

 (test-equal '(1)
   (call-with-values
       (lambda () ((constantly 1) 'a 'b))
     list))

 (test-equal '()
   (call-with-values
       (lambda () ((constantly) 'a 'b))
     list)))



(test-group
 "complement"

 (test-equal #f
   ((complement symbol?) 'a))

 (test-equal #t
   ((complement symbol?) 1)))



(test-group
 "swap"

 (test-equal '(2 1 3 4)
   ((swap list) 1 2 3 4)))



(test-group
 "flip"

 (test-equal '(4 3 2 1)
   ((flip list) 1 2 3 4)))



(test-group
 "on-left"

 (test-equal '(1)
   ((on-left list) 1 2)))



(test-group
 "on-right"

 (test-equal '(2)
   ((on-right list) 1 2)))



(test-group
 "conjoin"

 (test-assert
     ((conjoin number? exact?)))

 (test-assert
     ((conjoin number? exact?) 1 2))

 (test-assert
     (not ((conjoin number? exact?) 1 2.)))

 (test-assert
     ((conjoin) 1 2)))



(test-group
 "disjoin"

 (test-assert
     ((disjoin number? string?)))

 (test-assert
     ((disjoin number? string?) 1 "a"))

 (test-assert
     (not ((disjoin number? string?) 'a 'b)))

 (test-assert
     (not ((disjoin) 1 2))))



(test-group
 "each-of"

 (let ((r1 #f)
       (r2 #f))
   ((each-of
     (lambda args (set! r1 args))
     (lambda args (set! r2 args)))
    1 2)
   (test-equal r1 '(1 2))
   (test-equal r2 '(1 2))))



(test-group
 "all-of"

 (test-assert
     ((all-of string?) '()))

 (test-assert
     ((all-of string?) '("a" "b")))

 (test-assert
     (not ((all-of string?) '("a" b))))

 (test-assert
     (not ((all-of (lambda (x)
                     (when (equal? x 'c)
                       ;; should short circuit before this point
                       (test-assert #f))
                     (string? x)))
           '("a" b c)))))



(test-group
 "any-of"

 (test-assert
     ((any-of string?) '()))

 (test-assert
     ((any-of string?) '("a" b)))

 (test-assert
     (not ((any-of string?) '(a b))))

 (test-assert
     ((any-of (lambda (x)
                (when (equal? x 'b)
                  ;; should short circuit before this point
                  (test-assert #f))
                (string? x)))
      '("a" b))))



(test-group
 "on"

 (test-equal '(2 3 4)
   ((on list (lambda (x) (+ 1 x))) 1 2 3)))



(test-group
 "left-section"

 (test-equal '(1 2 3 4)
   ((left-section list 1 2) 3 4)))



(test-group
 "right-section"

 (test-equal '(3 4 2 1)
   ((right-section list 1 2) 3 4)))



(test-group
 "begin-procedure"

 (test-equal 2
   (begin-procedure
    (lambda () 1)
    (lambda () 2))))



(test-group
 "if-procedure"

 (test-equal 1
   (if-procedure #t
                 (lambda () 1)))

 (test-equal 1
   (if-procedure #t
                 (lambda () 1)
                 (lambda () (test-assert #f))))

 (test-equal 2
   (if-procedure #f
                 (lambda () (test-assert #f))
                 (lambda () 2))))



(test-group
 "if-not-procedure"

 (test-equal 2
   (if-not-procedure #f
                     (lambda () 2))))



(test-group
 "value-procedure"

 (test-equal "1"
   (value-procedure 1
                    number->string
                    (lambda () (test-assert #f))))

 (test-equal 2
   (value-procedure #f
                    (lambda args (test-assert #f))
                    (lambda () 2))))



(test-group
 "case-procedure"

 (test-equal 2
   (case-procedure 'b
                   `((a . ,(lambda () 1))
                     (b . ,(lambda () 2)))))

 (test-equal 3
   (case-procedure 'c
                   `((a . ,(lambda () 1))
                     (b . ,(lambda () 2)))
                   (lambda () 3))))



(test-group
 "and-procedure"

 (test-assert
     (and-procedure))

 (test-equal 2
   (and-procedure (lambda () 1)
                  (lambda () 2)))

 (test-assert
     (not (and-procedure (lambda () #f)
                         (lambda () (test-assert #f))))))



(test-group
 "or-procedure"

 (test-assert
     (not (or-procedure)))

 (test-equal 2
   (or-procedure (lambda () #f)
                 (lambda () 2)))

 (test-assert
     (or-procedure (lambda () 1)
                   (lambda () (test-assert #f)))))



(test-group
 "loop-procedure"

 (call/cc (lambda (k)
            (define v 0)
            (define (thunk)
              (when (> v 5)
                (k #t))
              (set! v (+ 1 v)))
            (loop-procedure thunk)
            (test-assert #t))))



(test-group
 "while-procedure"

 (define v 0)
 (define (thunk)
   (set! v (+ 1 v))
   (< v 5))
 (while-procedure thunk)
 (test-equal 5 v))



(test-group
 "until-procedure"

 (define v 0)
 (define (thunk)
   (set! v (+ 1 v))
   (>= v 5))
 (until-procedure thunk)
 (test-equal 5 v))


(test-group
 "always"

 (test-assert (always))
 (test-assert (always 'a)))



(test-group
 "never"

 (test-assert (not (never)))
 (test-assert (not (never 'a))))



(test-group
 "boolean"

 (test-equal #t (boolean 1))
 (test-equal #f (boolean #f)))



(test-group
 "identity"

 (test-equal 1 (identity 1))
 (test-equal 'a (identity 'a)))


(test-end)
