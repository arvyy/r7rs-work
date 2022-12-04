(import (scheme base)
        (scheme write)
        (srfi 64)
        (uris))

(define (do-test-uri-part-computation-from-parent)

  ;; from whole
  (test-group "whole"
    (let ((whole (make-uri-object 'whole "http://foo:bar@baz/path#fragment")))
      (test-equal "http" (uri-scheme whole))
      (test-equal "//foo:bar@baz/path" (uri-specific whole))
      (test-equal "fragment" (uri-fragment whole)))
    (let ((whole (make-uri-object 'whole "http://foo@baz/path#fragment")))
      (test-equal "http" (uri-scheme whole))
      (test-equal "//foo@baz/path" (uri-specific whole))
      (test-equal "fragment" (uri-fragment whole)))
    (let ((whole (make-uri-object 'whole "http://baz/path#fragment")))
      (test-equal "http" (uri-scheme whole))
      (test-equal "//baz/path" (uri-specific whole))
      (test-equal "fragment" (uri-fragment whole)))
    (let ((whole (make-uri-object 'whole "//baz/path#fragment")))
      (test-equal #f (uri-scheme whole))
      (test-equal "//baz/path" (uri-specific whole))
      (test-equal "fragment" (uri-fragment whole)))
    (let ((whole (make-uri-object 'whole "http://baz:80/path?query=foo#fragment")))
      (test-equal "http" (uri-scheme whole))
      (test-equal "//baz:80/path?query=foo" (uri-specific whole))
      (test-equal "fragment" (uri-fragment whole)))
    (let ((whole (make-uri-object 'whole "http://baz:80?query=foo")))
      (test-equal "http" (uri-scheme whole))
      (test-equal "//baz:80?query=foo" (uri-specific whole))
      (test-equal #f (uri-fragment whole)))
    (let ((whole (make-uri-object 'whole "http:/path")))
      (test-equal "http" (uri-scheme whole))
      (test-equal "/path" (uri-specific whole))
      (test-equal #f (uri-fragment whole)))
    (let ((whole (make-uri-object 'whole "http:path")))
      (test-equal "http" (uri-scheme whole))
      (test-equal "path" (uri-specific whole))
      (test-equal #f (uri-fragment whole)))
    (let ((whole (make-uri-object 'whole "path")))
      (test-equal #f (uri-scheme whole))
      (test-equal "path" (uri-specific whole))
      (test-equal #f (uri-fragment whole))))

  ;; from specific
  (test-group "specific"
    (let ((specific (make-uri-object 'specific "//foo:bar@baz/path?query=a")))
      (test-equal "foo:bar@baz" (uri-authority specific))
      (test-equal "/path" (uri-path specific))
      (test-equal "query=a" (uri-query specific)))
    (let ((specific (make-uri-object 'specific "//foo@baz/path")))
      (test-equal "foo@baz" (uri-authority specific))
      (test-equal "/path" (uri-path specific))
      (test-equal #f (uri-query specific)))
    (let ((specific (make-uri-object 'specific "//baz:80/path")))
      (test-equal "baz:80" (uri-authority specific))
      (test-equal "/path" (uri-path specific))
      (test-equal #f (uri-query specific)))
    (let ((specific (make-uri-object 'specific "/path")))
      (test-equal #f (uri-authority specific))
      (test-equal "/path" (uri-path specific))
      (test-equal #f (uri-query specific)))
    (let ((specific (make-uri-object 'specific "//baz:80")))
      (test-equal "baz:80" (uri-authority specific))
      (test-equal "" (uri-path specific))
      (test-equal #f (uri-query specific))))

  ;; from authority
  (test-group "authority"
    (let ((authority (make-uri-object 'authority "foo:bar@baz:80")))
      (test-equal "foo:bar" (uri-userinfo authority))
      (test-equal "baz" (uri-host authority))
      (test-equal 80 (uri-port authority)))
    (let ((authority (make-uri-object 'authority "foo@baz:80")))
      (test-equal "foo" (uri-userinfo authority))
      (test-equal "baz" (uri-host authority))
      (test-equal 80 (uri-port authority)))
    (let ((authority (make-uri-object 'authority "baz:80")))
      (test-equal #f (uri-userinfo authority))
      (test-equal "baz" (uri-host authority))
      (test-equal 80 (uri-port authority)))
    (let ((authority (make-uri-object 'authority "foo:bar@baz")))
      (test-equal "foo:bar" (uri-userinfo authority))
      (test-equal "baz" (uri-host authority))
      (test-equal #f (uri-port authority)))
    (let ((authority (make-uri-object 'authority "baz")))
      (test-equal #f (uri-userinfo authority))
      (test-equal "baz" (uri-host authority))
      (test-equal #f (uri-port authority))))

  ;; from userinfo
  (test-group "userinfo"
    (let ((userinfo (make-uri-object 'host "host" 'userinfo "foo:bar")))
      (test-equal "foo" (uri-username userinfo))
      (test-equal "bar" (uri-password userinfo)))
    (let ((userinfo (make-uri-object 'host "host" 'userinfo "foo:bar:bar")))
      (test-equal "foo" (uri-username userinfo))
      (test-equal "bar:bar" (uri-password userinfo)))
    (let ((userinfo (make-uri-object 'host "host" 'userinfo "foo")))
      (test-equal "foo" (uri-username userinfo))
      (test-equal #f (uri-password userinfo))))

  )

(define (do-test-uri-part-computation-from-children)

  (test-group "whole"
    (let ((uri (make-uri-object 'scheme "http" 'specific "//foo:bar@baz:80/path?query=a" 'fragment "fragment")))
      (test-equal "http://foo:bar@baz:80/path?query=a#fragment" (uri-whole uri)))
    (let ((uri (make-uri-object 'specific "//foo:bar@baz:80/path?query=a" 'fragment "fragment")))
      (test-equal "//foo:bar@baz:80/path?query=a#fragment" (uri-whole uri)))
    (let ((uri (make-uri-object 'specific "//foo:bar@baz:80/path?query=a")))
      (test-equal "//foo:bar@baz:80/path?query=a" (uri-whole uri))))

  (test-group "specific"
    (let ((uri (make-uri-object 'authority "foo:bar@baz:80" 'path "/path" 'query "query=a")))
      (test-equal "//foo:bar@baz:80/path?query=a" (uri-specific uri)))
    (let ((uri (make-uri-object 'path "path" 'query "query=a")))
      (test-equal "path?query=a" (uri-specific uri)))
    (let ((uri (make-uri-object 'authority "foo:bar@baz:80" 'path "/path")))
      (test-equal "//foo:bar@baz:80/path" (uri-specific uri)))
    (let ((uri (make-uri-object 'authority "foo:bar@baz:80" 'query "query=a")))
      (test-equal "//foo:bar@baz:80?query=a" (uri-specific uri)))
    (let ((uri (make-uri-object 'authority "foo:bar@baz:80")))
      (test-equal "//foo:bar@baz:80" (uri-specific uri))))

  (test-group "authority"
    (let ((uri (make-uri-object 'host "baz" 'port 80 'userinfo "foo:bar")))
      (test-equal "foo:bar@baz:80" (uri-authority uri)))
    (let ((uri (make-uri-object 'host "baz" 'port 80)))
      (test-equal "baz:80" (uri-authority uri)))
    (let ((uri (make-uri-object 'host "baz" 'userinfo "foo:bar")))
      (test-equal "foo:bar@baz" (uri-authority uri)))
    (let ((uri (make-uri-object 'host "baz")))
      (test-equal "baz" (uri-authority uri))))

  (test-group "userinfo"
    (let ((uri (make-uri-object 'host "baz" 'username "foo" 'password "bar")))
      (test-equal "foo:bar" (uri-userinfo uri)))
    (let ((uri (make-uri-object 'host "baz" 'username "foo")))
      (test-equal "foo" (uri-userinfo uri))))

  )



(define (do-test-uri-computation)
  (define (assert-uris-match uri-producer expected-uri)
    (test-equal (uri-whole expected-uri) (uri-whole (uri-producer)))
    (test-equal (uri-scheme expected-uri) (uri-scheme (uri-producer)))
    (test-equal (uri-specific expected-uri) (uri-specific (uri-producer)))
    (test-equal (uri-authority expected-uri) (uri-authority (uri-producer)))
    (test-equal (uri-userinfo expected-uri) (uri-userinfo (uri-producer)))
    (test-equal (uri-username expected-uri) (uri-username (uri-producer)))
    (test-equal (uri-password expected-uri) (uri-password (uri-producer)))
    (test-equal (uri-host expected-uri) (uri-host (uri-producer)))
    (test-equal (uri-port expected-uri) (uri-port (uri-producer)))
    (test-equal (uri-path expected-uri) (uri-path (uri-producer)))
    (test-equal (uri-query expected-uri) (uri-query (uri-producer)))
    (test-equal (uri-fragment expected-uri) (uri-fragment (uri-producer))))

  (test-group "scheme+fragment+path+authority"
    (assert-uris-match
     (lambda ()
       (make-uri-object 'scheme "http"
                        'fragment "WARNING"
                        'path "/pub/ietf/uri/historical.html"
                        'authority "www.ics.uci.edu"))
     (string->uri-object "http://www.ics.uci.edu/pub/ietf/uri/historical.html#WARNING")))

  (test-group "scheme+fragment+specific"
    (assert-uris-match
     (lambda ()
       (make-uri-object 'scheme "http"
                        'fragment "WARNING"
                        'specific "//www.ics.uci.edu/pub/ietf/uri/historical.html"))
     (string->uri-object "http://www.ics.uci.edu/pub/ietf/uri/historical.html#WARNING")))

  (test-group "scheme+host+port+path"
    (assert-uris-match
     (lambda ()
       (make-uri-object 'scheme "http"
                        'host "www.ics.uci.edu"
                        'port 80
                        'path "/pub/ietf/uri/historical.html"))
     (string->uri-object "http://www.ics.uci.edu:80/pub/ietf/uri/historical.html")))

  (test-group "username+password+host+path"
    (assert-uris-match
     (lambda ()
       (make-uri-object 'username "foo"
                        'password "bar"
                        'host "www.ics.uci.edu"
                        'path ""))
     (string->uri-object "//foo:bar@www.ics.uci.edu")))

  (test-group "userinfo+password+host+path"
    (assert-uris-match
     (lambda ()
       (make-uri-object 'userinfo "foo:bar"
                        'password "bar"
                        'host "www.ics.uci.edu"
                        'path ""))
     (string->uri-object "//foo:bar@www.ics.uci.edu")))

  (test-group "username+host+path"
    (assert-uris-match
     (lambda ()
       (make-uri-object 'username "foo"
                        'host "www.ics.uci.edu"
                        'path ""))
     (string->uri-object "//foo@www.ics.uci.edu")))

  )

(define (do-test-parsing-error)
  (define-syntax assert-throws
    (syntax-rules ()
      ((_ name thunk)
       (call/cc (lambda (k)
                  (with-exception-handler
                      (lambda (err)
                        (if (uri-error? err)
                            (k #t)
                            (test-assert (string-append name " throws error satisfying `uri-error?`") #f)))
                    (lambda ()
                      (thunk)
                      (test-assert (string-append name " throws error") #f))))))))

  ;; password without username
  (assert-throws "password without username"
                 (lambda ()
                   (uri-whole (make-uri-object 'password "bar" 'host "baz"))))

  ;; port without host
  (assert-throws "port without host"
                 (lambda ()
                   (uri-whole (make-uri-object 'port 80))))

  ;; query without path nor authority
  (assert-throws "query without path nor authority"
                 (lambda ()
                   (uri-whole (make-uri-object 'query "query=a")))))

(define (do-test-uri-merge)
  ;; TODO
  #f
  )

(define (do-test-parse-path)
  (test-equal
      '("" "foo" "bar")
    (uri-parse-path (string->uri-object "http:foo/bar")))
  (test-equal
      '("foo" "bar")
    (uri-parse-path (string->uri-object "http://hostname/foo/bar")))
  (test-equal
      '("")
    (uri-parse-path (string->uri-object "http://hostname"))))

(define (do-test-parse-query)
  (define (test-alist-equal expected actual)
    (for-each
     (lambda (e)
       (define e2 (assoc (car e) actual))
       (test-assert e2)
       (test-equal (cdr e) (cdr e2)))
     expected))
  (test-alist-equal '((foo . "bar") (foo2 . "bar2"))
                    (uri-parse-query (string->uri-object "hostname/?foo=bar&foo2=bar2")))
  (test-alist-equal '((foo . "bar") (foo2 . "bar2"))
                    (uri-parse-query (string->uri-object "hostname/?foo=bar;foo2=bar2")))
  (test-alist-equal '()
                    (uri-parse-query (string->uri-object "hostname/"))))

(define (do-test-data-parsing)
  (test-group "non-data"
    (let*-values (((uri) (string->uri-object "http://foo?bar=baz"))
                  ((mediatype data) (uri-parse-data uri)))
      (test-assert (not mediatype))
      (test-assert (not data))))

  (test-group "base64 binary"
    (let*-values (((uri) (string->uri-object "data:asd/asd;base64,"))
                  ((mediatype data) (uri-parse-data uri)))
      (test-equal mediatype "asd/asd")
      (test-assert (bytevector? data))
      (test-equal "" (utf8->string data)))
    (let*-values (((uri) (string->uri-object "data:asd/asd;base64,Zg=="))
                  ((mediatype data) (uri-parse-data uri)))
      (test-equal mediatype "asd/asd")
      (test-assert (bytevector? data))
      (test-equal "f" (utf8->string data)))
    (let*-values (((uri) (string->uri-object "data:asd/asd;base64,Zm8="))
                  ((mediatype data) (uri-parse-data uri)))
      (test-equal mediatype "asd/asd")
      (test-assert (bytevector? data))
      (test-equal "fo" (utf8->string data)))
    (let*-values (((uri) (string->uri-object "data:asd/asd;base64,Zm9v"))
                  ((mediatype data) (uri-parse-data uri)))
      (test-equal mediatype "asd/asd")
      (test-assert (bytevector? data))
      (test-equal "foo" (utf8->string data)))
    (let*-values (((uri) (string->uri-object "data:asd/asd;base64,Zm9vbw=="))
                  ((mediatype data) (uri-parse-data uri)))
      (test-equal mediatype "asd/asd")
      (test-assert (bytevector? data))
      (test-equal "fooo" (utf8->string data))))

  (test-group "% encoded binary"
    (let*-values (((uri) (string->uri-object "data:asd/asd,"))
                  ((mediatype data) (uri-parse-data uri)))
      (test-equal mediatype "asd/asd")
      (test-assert (bytevector? data))
      (test-equal "" (utf8->string data)))
    (let*-values (((uri) (string->uri-object "data:asd/asd,%66%6f%6f%6f"))
                  ((mediatype data) (uri-parse-data uri)))
      (test-equal mediatype "asd/asd")
      (test-assert (bytevector? data))
      (test-equal "fooo" (utf8->string data)))
    (let*-values (((uri) (string->uri-object "data:asd/asd,fooo"))
                  ((mediatype data) (uri-parse-data uri)))
      (test-equal mediatype "asd/asd")
      (test-assert (bytevector? data))
      (test-equal "fooo" (utf8->string data))))

  (test-group "text"
    (let*-values (((uri) (string->uri-object "data:text/plain,fooo"))
                  ((mediatype data) (uri-parse-data uri)))
      (test-equal mediatype "text/plain")
      (test-assert (string? data))
      (test-equal "fooo" data))
    (let*-values (((uri) (string->uri-object "data:text/plain;charset=UTF-8,%ce%bb"))
                  ((mediatype data) (uri-parse-data uri)))
      (test-equal mediatype "text/plain;charset=UTF-8")
      (test-assert (string? data))
      (test-equal "λ" data)))

  (test-group "default mediatype"
    (let*-values (((uri) (string->uri-object "data:,fooo"))
                  ((mediatype data) (uri-parse-data uri)))
      (test-equal mediatype "text/plain;charset=US-ASCII")
      (test-assert (string? data))
      (test-equal "fooo" data))))

(test-begin "URIs")

(test-group "predicates"
  (test-assert (uri-object? (string->uri-object "http://foo")))
  (test-assert (not (uri-object? 1)))
  (test-assert (uri-reference? (string->uri-object "http://foo")))
  (test-assert (not (uri-reference? (make-uri-object 'username "test"))))
  (test-assert (uri-absolute? (string->uri-object "http://foo")))
  (test-assert (not (uri-absolute? (string->uri-object "//foo"))))
  (test-assert (uri-relative-reference? (string->uri-object "//foo")))
  (test-assert (not (uri-relative-reference? (string->uri-object "http://foo")))))

(test-group "uri part computation"
  (do-test-uri-computation)
  (do-test-uri-part-computation-from-parent)
  (do-test-uri-part-computation-from-children)
  (do-test-parsing-error))

(test-group "uri parse path"
  (do-test-parse-path))

(test-group "uri parse query"
  (do-test-parse-query))

(test-group "uri merge"
  (do-test-uri-merge))

(test-group "uri parse data"
  (do-test-data-parsing))

(test-end)
