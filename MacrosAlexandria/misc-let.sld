(define-library
    (misc-let)
    (import (scheme base))
    (export
        let-list
        if-let
        if-let*
        when-let
        when-let*
        case-using
        ;;TODO
        ;;case-match
        andmap
        ormap)
    (include "misc-let-impl.scm"))
