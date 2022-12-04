(define-library
    (uris)
    (import (scheme base)
            (scheme bitwise)
            (scheme char)
            (scheme case-lambda)
            (scheme write)
            (srfi 1))
    (export uri-object?
            uri-reference?
            uri-absolute?
            uri-relative-reference?
            make-uri-object
            string->uri-object
            uri-whole
            uri-scheme
            uri-specific
            uri-authority
            uri-userinfo
            uri-username
            uri-password
            uri-host
            uri-port
            uri-path
            uri-query
            uri-fragment
            uri-parse-path
            uri-parse-query
            uri-merge
            uri-parse-data
            uri-error?)
    (include "impl/util.scm")
    (include "impl/uri.scm")
    (include "impl/patterns.scm")
    (include "impl/uriparsepatterns.scm")
    (include "impl/uriparse.scm")
    (include "impl/dataparse.scm"))
