(cond-expand
  (guile
   (import (scheme base)
           (scheme case-lambda)
           (srfi srfi-64)
           (define-optional)))
  (chibi
   (import (scheme base)
           (scheme case-lambda)
           (rename (except (chibi test) test-equal)
                   (test test-equal))
           (define-optional)))
  (else
   (import (scheme base)
           (scheme case-lambda)
           (srfi 64)
           (define-optional))))



(test-begin "OptionalsRiastradh")

(test-group
 "Test define-optional"
 (define-optional (a req1 req2 (opt1 'foo) (opt2 'bar opt2?))
   (list req1 req2 opt1 opt2 opt2?))
 (define-optional (b req1 req2 (opt1 'foo) . rest)
   (list req1 req2 opt1 rest))

 (test-equal (a 1 2) '(1 2 foo bar #f))
 (test-equal (a 1 2 3) '(1 2 3 bar #f))
 (test-equal (a 1 2 3 4) '(1 2 3 4 #t))

 (test-equal (b 1 2) '(1 2 foo ()))
 (test-equal (b 1 2 3) '(1 2 3 ()))
 (test-equal (b 1 2 3 4) '(1 2 3 (4))))

(test-end)
