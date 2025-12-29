(define-record-type <tzfile-error>
  (make-tzfile-error message)
  tzfile-error?
  (message tzfile-error))

(define-record-type <tzfile>
  (make-tzfile version time-transitions leap-corrections)
  tzfile?
  (version tzfile-version)
  (time-transitions tzfile-time-transitions)
  (leap-corrections tzfile-leap-corrections))

(define-record-type <time-transition>
  (make-time-transition timepoint offset dst-flag tz-abbr)
  time-transition?
  (timepoint time-transition-timepoint)
  (offset time-transition-offset)
  (dst-flag time-transition-dst-flag)
  (tz-abbr time-transition-timezone-abbreviation))

(define-record-type <leap-correction>
  (make-leap-correction timepoint amount)
  leap-correction?
  (timepoint leap-correction-timepoint)
  (amount leap-correction-amount))

;; raise error specifically regarding issues with tzfile content
(define (err message)
  (raise (make-tzfile-error message)))

;; parse tzfile. `in` must be a binary input stream. Doesn't close the stream once done.
;; returns instance of <tzfile> on successful parse.
;; raises <tzfile-error> if something went wrong.
(define (read-tz-file in)
  (define v1 (read-tz-file-block in 4))
  (cond
    ((= 1 (tzfile-version v1)) v1)
    (else (let ((v2 (read-tz-file-block in 8)))
            ;; TODO read ending POSIX string
            v2))))

(define (read-tz-file-block in timepoint-byte-width)
  (verify-magic-string in)
  (let ((version (read-version in)))
    (skip-n-bytes in 15)
    (let* ((indicators-count (read-n-byte-int in 4))
           (standard/wall-count (read-n-byte-int in 4))
           (leapseconds-count (read-n-byte-int in 4))
           (transitions-count (read-n-byte-int in 4))
           (timetypes-count (read-n-byte-int in 4))
           (timezone-abbr-bytes-count (read-n-byte-int in 4))

           (transitions (read-transitions in timepoint-byte-width transitions-count timetypes-count timezone-abbr-bytes-count))
           (leap-seconds (read-leap-seconds in timepoint-byte-width leapseconds-count)))
        (skip-n-bytes in indicators-count)
        (skip-n-bytes in standard/wall-count)
        (make-tzfile version transitions leap-seconds))))

;; check file starts with TZif magic string
(define (verify-magic-string in)
  (let* ((magic "TZif")
         (bv (read-bytevector (string-length magic) in))
         (found-string (utf8->string bv)))
    (unless (string=? magic found-string)
      (err "tzfile doesn't contain expected magic string 'TZif'"))))

;; read version, should be 1-4
(define (read-version in)
  (let ((bv (read-bytevector 1 in)))
    (cond
      ((= 0 (bytevector-u8-ref bv 0)) 1)
      (else (let ((version-string (utf8->string bv)))
              (cond
                ((string->number version-string) => values)
                (else (err "Failed to read tzfile version"))))))))

;; read transitions as vector of <time-transition>
(define (read-transitions in timepoint-byte-width transition-count type-count abbr-bytes)
  (let* ((timepoints (make-vector* transition-count (lambda () (read-n-byte-signed-int in timepoint-byte-width))))
         (type-indeces (make-vector* transition-count (lambda () (read-u8 in))))
         (types (read-timetypes in type-count abbr-bytes)))
    (vector-map
      (lambda (timepoint index)
        (define transition-args (cons timepoint (vector-ref types index)))
        (apply make-time-transition transition-args))
      timepoints
      type-indeces)))

;; read type definitions as a vector, where each element is a list (offset dst-flag timezone-string)
(define (read-timetypes in count abbr-byte-count)
  (define (read-type)
    (vector
      (read-n-byte-signed-int in 4)
      (read-u8 in)
      (read-u8 in)))
  (let* ((types (make-vector* count read-type))
         (abbr-bytes (read-bytevector abbr-byte-count in)))
    (vector-map
      (lambda (type)
        (list (vector-ref type 0)
              (not (= 0 (vector-ref type 1)))
              (extract-string abbr-bytes (vector-ref type 2))))
      types)))

(define (read-leap-seconds in timepoint-byte-width count)
  (make-vector* count (lambda ()
                        (let ((time (read-n-byte-int in timepoint-byte-width))
                              (amount (read-n-byte-int in 4)))
                          (make-leap-correction time amount)))))

;; ------------ utils

(define (skip-n-bytes in n)
  (read-bytevector n in))

(define (read-n-byte-int in n)
  (let ((bv (read-bytevector n in)))
    (do ((value 0 (+ (* 256 value) (bytevector-u8-ref bv i)))
         (i 0 (+ i 1)))
        ((>= i n) value))))

(define (read-n-byte-signed-int in n)
  (let ((max-value (- (expt 2 (* 8 (- n 1))) 1))
        (unsigned-value (read-n-byte-int in n)))
    (if (> unsigned-value max-value)
        (- unsigned-value (expt 2 (* 8 n)))
        unsigned-value)))

(define (make-vector* n fill-proc)
  (define vec (make-vector n))
  (do ((i 0 (+ i 1)))
      ((>= i n) vec)
    (vector-set! vec i (fill-proc))))

(define (extract-string bytes start)
  (let ((end (bytevector-find bytes start 0)))
    (utf8->string bytes start end)))

(define (bytevector-find bv start-index value)
  (do ((i start-index (+ i 1)))
      ((or (>= i (bytevector-length bv))
           (= value (bytevector-u8-ref bv i))) i)))
