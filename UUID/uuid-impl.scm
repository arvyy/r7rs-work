(define-record-type <uuid>
  (make-uuid version data)
  uuid?
  (version uuid-version)
  (data uuid-data))

(define rand-max (exact (expt 2 122)))
(define rand-src (let ((src (make-random-source)))
                   (random-source-randomize! src)
                   src))
(define rand-int (random-source-make-integers rand-src))
(define (make-random-uuid)
  (let* ((rand (rand-int rand-max))
         (bits (bits->vector rand))
         (bits (if (= 122 (vector-length bits))
                   bits
                   (vector-append bits (make-vector (- 122 (vector-length bits)) #f))))
         (data-vec (make-vector 128 #f)))
  ;; Set the four most significant bits (bits 12 through 15) of the
  ;; time_hi_and_version field to the 4-bit version number
  (vector-set! data-vec (+ 48 0) #f)
  (vector-set! data-vec (+ 48 1) #t)
  (vector-set! data-vec (+ 48 2) #f)
  (vector-set! data-vec (+ 48 3) #f)

  ;; Set the two most significant bits (bits 6 and 7) of the
  ;; clock_seq_hi_and_reserved to zero and one, respectively.
  (vector-set! data-vec (+ 64 0) #t)
  (vector-set! data-vec (+ 64 1) #f)

    ;; Set all the other bits to randomly (or pseudo-randomly) chosen
    ;; values.
    (vector-copy! data-vec 0 bits 0 (+ 48 12))
    (vector-copy! data-vec (+ 48 16) bits (+ 48 12) (+ 64 2))
    (vector-copy! data-vec (+ 64 8) bits (+ 64 2) 122)

    (let ((data (apply bytevector
                       (map
                        (lambda (index)
                          (define start (* index 8))
                          (define end (* (+ 1 index) 8))
                          (vector->bits (vector-reverse-copy data-vec start end)))
                        (iota 16)))))
      (make-uuid 4 data))))

(define (string->uuid/valid? chars)
  (and (= 36 (length chars))
       (fold (lambda (char index valid)
               (and valid
                    (cond
                     ((or (= index 8)
                          (= index 13)
                          (= index 18)
                          (= index 23))
                      (equal? char #\-))
                     (else
                      (or (and (char-ci<=? #\0 char #\9))
                          (and (char-ci<=? #\a char #\f)))))))
             #t
             chars
             (iota 36))))

(define (string->uuid/bytevector chars)
  (let loop ((bytes '())
             (chars chars))
    (cond
     ((null? chars) (apply bytevector (reverse bytes)))
     ((equal? #\- (car chars)) (loop bytes (cdr chars)))
     (else (loop (cons (string->number (string (car chars) (cadr chars)) 16)
                       bytes)
                 (cddr chars))))))

(define (parse-version bytevec)
  (let* ((vec (bits->vector (bytevector-u8-ref bytevec 8)))
         (l (vector-length vec))
         (vec (if (>= l 4)
                  vec
                  (vector-append vec (make-vector (- 4 l) #f)))))
    (vector->bits (vector-copy vec 0 4))))

(define (string->uuid str)
  (define chars (string->list str))
  (if (not (string->uuid/valid? chars))
      #f
      (let* ((data (string->uuid/bytevector chars))
             (version (parse-version data)))
        (make-uuid version data))))

(define (uuid->string uuid)
  (define data (uuid-data uuid))
  (define parts
    (map
     (lambda (index)
       (define hex (let ((hex (number->string (bytevector-u8-ref data index) 16)))
                     (if (= 2 (string-length hex))
                         hex
                         (string-append (make-string (- 2 (string-length hex)) #\0)
                                        hex))))
       (cond
        ((or (= index 3)
             (= index 5)
             (= index 7)
             (= index 9))
         (string-append hex "-"))
        (else hex)))
     (iota 16)))
  (apply string-append  parts))

(define (uuid->bytevector uuid)
  (bytevector-copy (uuid-data uuid)))

(define (bytevector->uuid bytevec)
  (unless (= 16 (bytevector-length bytevec))
    (error "parameter to bytevector->uuid should be 16 bytes long"))
  (make-uuid
   (parse-version bytevec)
   (bytevector-copy bytevec)))

(define (uuid->integer uuid)
  (define data (uuid-data uuid))
  (define bits-vec
    (apply vector-append
           (map
            (lambda (index)
              (define vec (bits->vector (bytevector-u8-ref data index)))
              (define l (vector-length vec))
              (if (= 8 l)
                  vec
                  (vector-append vec (make-vector (- 8 l) #f))))
            (iota 16))))
  (vector->bits bits-vec))

(define (integer->uuid int)
  (let* ((bits-vec (bits->vector int))
         (l (vector-length bits-vec))
         (bits-vec (if (= 128 l)
                       bits-vec
                       (vector-append bits-vec (make-vector (- 128 l) #f))))
         (bytes (map
                 (lambda (index)
                   (define start (* index 8))
                   (define end (* (+ 1 index) 8))
                   (define vec (vector-copy bits-vec start end))
                   (vector->bits vec))
                 (iota 16)))
         (bytevec (apply bytevector bytes))
         (version (parse-version bytevec)))
    (make-uuid
     version
     bytevec)))

(define (make-relative-uuid namespace name)
  (define name-bytevec
    (cond
     ((bytevector? name) name)
     ((string? name) (string->utf8 name))
     (else (error "name should be string or bytevector" name))))
  (define namespace-bytevec (uuid-data namespace))
  (define sha-1-hash (sha-1 (bytevector-append namespace-bytevec name-bytevec)))
  (define sha-1-vec (let* ((vec (bits->vector (string->number sha-1-hash 16)))
                           (l (vector-length vec))
                           (vec (if (= l 160)
                                    vec
                                    (vector-append vec (make-vector (- 160 l) #f)))))
                      (vector-reverse-copy vec)))
  (define data-vec (vector-copy sha-1-vec 0 128))

  ;; Set the four most significant bits (bits 12 through 15) of the
  ;; time_hi_and_version field to the 4-bit version number
  (vector-set! data-vec (+ 48 0) #f)
  (vector-set! data-vec (+ 48 1) #t)
  (vector-set! data-vec (+ 48 2) #f)
  (vector-set! data-vec (+ 48 3) #t)

  ;; Set the two most significant bits (bits 6 and 7) of the
  ;; clock_seq_hi_and_reserved to zero and one, respectively.
  (vector-set! data-vec (+ 64 0) #t)
  (vector-set! data-vec (+ 64 1) #f)

  (let ((data (apply bytevector
                     (map
                      (lambda (index)
                        (define start (* index 8))
                        (define end (* (+ 1 index) 8))
                        (vector->bits (vector-reverse-copy data-vec start end)))
                      (iota 16)))))
    (make-uuid 5 data)))

(define dns-namespace-uuid (string->uuid "6ba7b810-9dad-11d1-80b4-00c04fd430c8"))
(define url-namespace-uuid (string->uuid "6ba7b811-9dad-11d1-80b4-00c04fd430c8"))
(define oid-namespace-uuid (string->uuid "6ba7b812-9dad-11d1-80b4-00c04fd430c8"))
(define x500-namespace-uuid (string->uuid "6ba7b814-9dad-11d1-80b4-00c04fd430c8"))
(define nil-uuid (string->uuid "00000000-0000-0000-0000-000000000000"))

(define uuid-comparator
  (let ((bytevector-comparator (make-vector-comparator
                                (make-comparator exact-integer? = < number-hash)
                                bytevector?
                                bytevector-length
                                bytevector-u8-ref)))
    (make-comparator uuid?
                     (lambda (a b)
                       ((comparator-equality-predicate bytevector-comparator)
                        (uuid-data a)
                        (uuid-data b)))
                     (lambda (a b)
                       ((comparator-ordering-predicate bytevector-comparator)
                        (uuid-data a)
                        (uuid-data b)))
                     (lambda (uuid)
                       ((comparator-hash-function bytevector-comparator) (uuid-data uuid))))))
