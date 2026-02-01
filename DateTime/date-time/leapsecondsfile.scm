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

;; amount to add to epoch (which starts at 1970-01-01) to convert to ntp seconds (starts at 1900-01-01)
(define epoch-ntp-diff (* (+ 17 (* 70 365)) 86400))

;; leapseconds is vector returned by read-leapseconds-file
;; tai-timestamp is unix timestamp in tai scale
;; return (values leap? offset)
;; if leap? is #t, it means in utc time the second part is 60
;; offset returns (positive) diff between tai and utc. If leap? is #t, the offset is the 'next' offset, so after calculation the seconds part will 59 and will need to be added +1 afterwards
(define (leapsecond-info/tai leapseconds tai-timestamp)
    (let* ((ntp-timestamp (+ tai-timestamp epoch-ntp-diff))
           (datum-fn (lambda (e) (+ (car e) (cdr e))))
           (index (find-index leapseconds 0 (vector-length leapseconds) ntp-timestamp datum-fn))
           (e (vector-ref leapseconds index))
           (found-value (datum-fn e))
           (leap? (= found-value ntp-timestamp)))
      (values leap? (cdr e))))

;; leapseconds is vector returned by read-leapseconds-file
;; timestamp is unix timestamp in utc scale
;; return (values leap? offset)
;; if leap? is #t, it means in utc time the second part must be 60
;; offset returns (positive) diff between tai and utc. If leap? is #t, the offset is the 'prev' offset, the extra second is already accounted by being of value 60
(define (leapsecond-info/utc leapseconds timestamp)
    (let* ((ntp-timestamp (+ timestamp epoch-ntp-diff))
           (datum-fn (lambda (e) (car e)))
           (index (find-index leapseconds 0 (vector-length leapseconds) ntp-timestamp datum-fn))
           (index (max 0 (- index 1)))
           (e (vector-ref leapseconds index))
           (found-value (datum-fn e))
           (leap? (= found-value ntp-timestamp)))
      (values leap? (cdr e))))

(define (find-index leapseconds start end v datum-fn)
    (if (= start end)
        start
        (let* ((mid (floor (/ (+ start end) 2)))
               (value (datum-fn (vector-ref leapseconds mid))))
          (cond
              ((= mid v) mid)
              ((< mid v) (find-index leapseconds (+ 1 mid) end v datum-fn))
              ((> mid v) (find-index leapseconds start (- mid 1) v datum-fn))))))
