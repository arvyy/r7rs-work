(cond-expand
  (guile
   (import (scheme base)
           (if-star)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (if-star)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (if-star)
           (srfi 64))))


(test-begin "IfStar")

(test-equal (if #f #f) (if* #f #f))
(test-equal 1 (if* #t 1 0))
(test-equal 0 (if* #f 1 0))

(test-equal 2
  (if* #f 0
       #f 1
       #t 2))

(test-equal 3
  (if* #f 0
       #f 1
       #f 2
       3))

(test-end)
