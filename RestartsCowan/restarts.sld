(define-library
    (restarts)
    (import (scheme base)
            (scheme list)
            (scheme write)
            (srfi 222))
    (export
        make-restarter
        restarter?
        restarter-tag
        restarter-description

        restart
        ambient-restarters
        with-restarter
        find-restarter
        collect-restarters
        interactor)
    (include "restarts-impl.scm"))
