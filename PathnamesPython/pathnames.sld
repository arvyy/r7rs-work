(define-library
    (pathnames)
    (import (scheme base)
            (scheme case-lambda)
            (scheme char)
            (scheme list)
            (scheme write))
    (export
        parse-posix-pathname
        parse-windows-pathname

        posix-pathname
        windows-pathname
        pathname
        path->file-uri

        path-reserved?
        path-absolute-posix?
        path-absolute-windows?
        path-portable?

        path-parent
        path-filename
        ;path-match
        path-relative-to
        path-suffix
        path-with-suffix
        path-without-suffix

        path-join
        path-with-filename
        path-normalize

        path-error?)
    (include "pathnames-impl.scm"))
