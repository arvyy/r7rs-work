(define-library
    (uuid)
    (import (scheme base)
            (scheme char)
            (scheme list)
            (scheme vector)
            (scheme bitwise)
            (srfi 27)
            (srfi 128))
    (export
        uuid?
        uuid-version

        make-random-uuid
        make-relative-uuid
        dns-namespace-uuid
        url-namespace-uuid
        oid-namespace-uuid
        x500-namespace-uuid
        nil-uuid

        uuid->string
        string->uuid
        uuid->bytevector
        bytevector->uuid
        uuid->integer
        integer->uuid

        uuid-comparator)
    (include "sha-1.scm")
    (include "uuid-impl.scm"))
