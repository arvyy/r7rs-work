(import (scheme base)
        (srfi 64)
        (uris))

(define (do-test-uri-part-computation)
  (define (assert-uris-match uri-producer expected-uri)
    (let ((parts (list uri-whole uri-scheme uri-specific uri-authority uri-userinfo uri-username uri-password uri-host uri-port uri-path uri-query uri-fragment)))
      (for-each
       (lambda (part)
         (test-equal (part (uri-producer)) (part expected-uri)))
       parts)))

  (assert-uris-match
   (lambda ()
     (make-uri-object 'scheme "http"
                      'fragment "WARNING"
                      'path "/pub/ietf/uri/historical.html"
                      'authority "www.ics.uci.edu"))
   (string->uri-object "http://www.ics.uci.edu/pub/ietf/uri/historical.html#WARNING")))


(test-begin "URIs")

(test-group "uri part computation"
  (do-test-uri-part-computation))

(test-end)
