;; Module for parsing a text file containing leap second data
(define-library
  (date-time leapsecondsfile)

  (import 
    (scheme base)
    (scheme read))

  (export
    read-leapseconds-file)

  (include "leapsecondsfile.scm"))
