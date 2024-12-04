;; read leap seconds file
;; takes textual input port and returns a vector of pairs, where car is NTP second, cdr is amount of leap seconds to add to TAI to catch up UTC
;; doesn't close the input port
(define (read-leapseconds-file in)
  (do ((line (read-line in) (read-line in))
       (content/rev '() (let ((parsed-entry (read-entry line)))
                          (if parsed-entry
                              (cons parsed-entry content/rev)
                              content/rev))))
      ((eof-object? line) (list->vector (reverse content/rev)))))

(define (read-entry line)
  (define (do-read-line)
    (let* ((in (open-input-string line))
           (ntp (read in))
           (delta (read in)))
      (cons ntp delta)))
  (if (char=? (string-ref line 0) #\#)
      #f
      (do-read-line)))
