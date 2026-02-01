;; Module for parsing a text file containing leap second data
(define-library
  (date-time leapsecondsfile)

  (import
    (scheme base)
    (scheme read))

  (export
    read-leapseconds-file
    leapsecond-info/tai
    leapsecond-info/utc)

  (include "leapsecondsfile.scm"))
