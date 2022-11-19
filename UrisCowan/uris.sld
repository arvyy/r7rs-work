(define-library
    (uris)
    (import (scheme base)
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
            uri-parse-error?)
    (include "uris-impl.scm"))
