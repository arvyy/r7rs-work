(cond-expand
  (guile
   (import (scheme base)
           (lambda-star)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (lambda-star)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (lambda-star)
           (srfi 64))))


(test-begin "LambdaStar")

(test-group
 "Test unary"
 (define f (lambda* (a) a))
 (test-equal 1 (f 1)))

(test-group
 "Test polyadic"
 (define f (lambda* (a b c) (+ a b c)))
 (test-equal 6 (f 1 2 3))
 (test-equal 6 (((f 1) 2) 3))
 (test-equal 6 (apply f '(1 2 3))))

(test-group
 "Test polyvariadic"
 (define f (lambda* (a b . c) (apply + `(,a ,b ,@c))))
 (test-equal 3 (f 1 2))
 (test-equal 6 (f 1 2 3))
 (test-equal 10 (f 1 2 3 4))
 (test-equal 3 ((f 1) 2))
 (test-equal 10 ((f 1) 2 3 4))
 (test-equal 6 (apply f '(1 2 3))))

(test-group
 "Test nesting lambda star"
 (define f (lambda* (a) (lambda* (b c) (+ a b c))))
 (test-equal 6 (f 1 2 3))
 (test-equal 6 ((f 1 2) 3))
 (test-equal 6 (((f 1) 2) 3))
 (test-equal 6 (apply f '(1 2 3))))

(test-group
 "Test multi-expression body"
 (define f (lambda* (a b c)
             (define rez (+ a b c))
             (set! rez (+ 1 rez))
             rez))
 (test-equal 7 (f 1 2 3))
 (test-equal 7 (((f 1) 2) 3))
 (test-equal 7 (apply f '(1 2 3))))

(test-end)
