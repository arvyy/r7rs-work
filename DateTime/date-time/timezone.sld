;; helper that uses timezone info from tzfile and augments with ability to find offset
;; using local time
(define-library (date-time timezone)
  (import (scheme base)
          (date-time tzfile)
          (gauche base)  ;; for debugging, TODO remove
          )
  (export timezone?
          create-static-timezone
          create-timezone-from-tz-transitions
          find-offset)
  (include "timezone.scm"))
