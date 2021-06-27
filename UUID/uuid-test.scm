(cond-expand
  (chibi
   (import (scheme base)
           (scheme write)
           (srfi 128)
           (uuid)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (scheme write)
           (srfi 128)
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

 (define uuid (make-relative-uuid dns-namespace-uuid "test"))
 (test-equal 5 (uuid-version uuid))
 (test-equal "4be0643f-1d98-573b-97cd-ca98a65347dd" (uuid->string uuid)))

(test-group
 "uuid <-> string"

 (define uuid (make-relative-uuid dns-namespace-uuid "test"))
 (define uuid-str (uuid->string uuid))
 (test-assert (string? uuid-str))
 (test-equal 36 (string-length uuid-str))
 (test-assert (=? uuid-comparator uuid (string->uuid uuid-str))))

(test-group
 "uuid <-> int"

 (define uuid (make-relative-uuid dns-namespace-uuid "test"))
 (define uuid-int (uuid->integer uuid))
 (test-assert (exact-integer? uuid-int))
 (test-assert (=? uuid-comparator uuid (integer->uuid uuid-int))))

(test-group
 "uuid <-> bytevector"

 (define uuid (make-relative-uuid dns-namespace-uuid "test"))
 (define uuid-bytevec (uuid->bytevector uuid))
 (test-assert (bytevector? uuid-bytevec))
 (test-equal 16 (bytevector-length uuid-bytevec))
 (test-assert (=? uuid-comparator uuid (bytevector->uuid uuid-bytevec))))

(test-group
 "constants"

 (test-assert (uuid? dns-namespace-uuid))
 (test-assert (uuid? url-namespace-uuid))
 (test-assert (uuid? oid-namespace-uuid))
 (test-assert (uuid? x500-namespace-uuid))
 (test-assert (uuid? nil-uuid)))

(test-group
 "comparator"

 (test-assert (=? uuid-comparator dns-namespace-uuid dns-namespace-uuid))
 (test-assert (not (=? uuid-comparator dns-namespace-uuid url-namespace-uuid)))
 (test-assert (<? uuid-comparator dns-namespace-uuid url-namespace-uuid))
 (test-assert (number? (comparator-hash uuid-comparator dns-namespace-uuid)))
 (test-assert (not (equal? (comparator-hash uuid-comparator dns-namespace-uuid)
                           (comparator-hash uuid-comparator url-namespace-uuid)))))

(test-end)
