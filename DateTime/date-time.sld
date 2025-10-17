(define-library
    (date-time)
    (import (scheme base)
            (gauche base)  ;; for debugging, TODO remove
            (scheme write) ;; for debugging, TODO remove
            ;;
            )
    (export
        ;; error raised on invalid method calls
        ;; not part of spec
        date-time-error?
        date-time-error-message
        date-time-error-args

        make-date
        date?
        date-ymd
        date-year
        date-month
        date-day

        date-weekday
        date-iso-week
        date-iso-week-year
        date-iso-weekday

        date->iso8601
        date->mjd
        mjd->date
        date->rata-die
        rata-die->date

        date=?
        date<?
        date<=?
        date>?
        date>=?

        ;;
        )
    (include "date-time-impl.scm"))
