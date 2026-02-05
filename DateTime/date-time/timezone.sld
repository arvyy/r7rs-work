;; helper that uses timezone info from tzfile and augments with ability to find offset
;; using local time
(define-library (date-time timezone)
  (import (scheme base)
          (date-time tzfile))
  (export timezone?
          create-static-timezone
          create-timezone-from-tz-transitions
          find-offset/wall
          find-offset/utc)
  (include "timezone.scm"))
