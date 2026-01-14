(define-library
    (date-time)
    (import (scheme base)
            (scheme file)
            (scheme division)
            (date-time timezone)
            (date-time tzfile)
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

        ;; date
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

        ;; clock-time
        make-clock-time
        clock-time?
        clock-time-hour
        clock-time-minute
        clock-time-second
        clock-time-hms

        ;; moment
        make-moment
        moment?
        moment-date
        moment-second-of-day

        moment=?
        moment<?
        moment<=?
        moment>?
        moment>=?

        ;; timestamp
        make-timestamp
        date+clock-time->timestamp
        timestamp?
        ;; TODO
        ;; moment->timestamp
        ;; timestamp->moment
        ;; timestamp-in-timezone
        ;; posix-time->utc-timestamp
        ;; posix-time->utc-timestamp
        ;; timestamp->utc-posix-time
        timestamp-date
        timestamp-ymd
        timestamp-year
        timestamp-month
        timestamp-day
        timestamp-clock-time
        timestamp-hms
        timestamp-hour
        timestamp-minute
        timestamp-second
        timestamp-timezone
        timestamp-fold
        timestamp-timezone-offset
        ;; TODO
        ;; timestamp->iso-8601

        ;; timezone
        timezone?
        utc-timezone
        utc-offset-timezone
        system-timezone
        tz-timezone
        tz-timezones

        ;; dt
        years-dt
        months-dt
        weeks-dt
        days-dt
        hours-dt
        minutes-dt
        seconds-dt
        dt-years
        dt-months
        dt-weeks
        dt-days
        dt-hours
        dt-minutes
        dt-seconds
        dt+
        dt-negate
        date+
        ;; TODO
        ;; timestamp+
        ;; current-moment
        ;; current-utc-timestamp
        ;; current-system-timestamp

        )
    (include "date-time-impl.scm"))
