;; Module for parsing tzif file
(define-library
  (date-time tzfile)

  (import 
    (scheme base))

  (export
    make-tzfile-error
    tzfile-error?
    tzfile-error-message

    make-tzfile
    tzfile?
    tzfile-version
    tzfile-time-transitions
    tzfile-leap-corrections

    make-time-transition
    time-transition?
    time-transition-timepoint
    time-transition-offset
    time-transition-dst-flag
    time-transition-timezone-abbreviation

    make-leap-correction
    leap-correction?
    leap-correction-timepoint
    leap-correction-amount

    read-tz-file)

  (include "tzfile.scm"))
