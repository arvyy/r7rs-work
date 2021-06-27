(cond-expand
  (guile
   (import (scheme base)
           (uuid)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (scheme write)
           (uuid)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (scheme write)
           (uuid)
           (srfi 64))))


(test-begin "uuid")

(test-group
 "random uuid"

 (define uuid (make-random-uuid))
 (test-equal 4 (uuid-version uuid))

 (newline)
 (newline)
 (display "Some random uuids bellow") (newline)
 (display (uuid->string (make-random-uuid))) (newline)
 (display (uuid->string (make-random-uuid))) (newline)
 (display (uuid->string (make-random-uuid))) (newline)
 (display (uuid->string (make-random-uuid))) (newline)
 (newline))

(test-group
 "relative uuid"

 (test-equal "4be0643f-1d98-573b-97cd-ca98a65347dd" (uuid->string (make-relative-uuid dns-namespace-uuid "test")))
 )

(test-end)
