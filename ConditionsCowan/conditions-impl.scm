(define (condition? obj)
  (or (r6rs-condition? obj)
      (compound? obj)))

(define (condition-of-type? obj type)
  (memq type (condition-types obj)))

(define (condition-types obj)
  (cond
    ((r6rs-condition? obj)
     (let* ((simple (r6rs-simple-conditions obj))
            (rtds (map record-rtd simple))
            (rtds (apply append (map rtd->rtd-hierarchy rtds)))
            (names (map rtd->symbol rtds)))
       (apply lset-adjoin symbol=? '() names)))
    (else
      (apply lset-adjoin symbol=? '() (filter symbol? (compound-subobjects obj))))))

(define (rtd->rtd-hierarchy rtd)
  (let loop ((rtd rtd)
             (result '()))
    (if rtd
      (loop (r6rs-record-type-parent rtd)
            (cons rtd result))
      result)))

(define (rtd->symbol rtd)
  (define s (record-type-name rtd))
  (if (char=? #\& (string-ref s 0))
    (string->symbol (substring s 1 (string-length s)))
    (string->symbol s)))

(define (error-object? obj)
  (or (base-error-object? obj)
      (condition-of-type? obj 'simple)))

(define (file-error? obj)
  (or (base-file-error? obj)
      (condition-of-type? obj 'file)))

(define (read-error? obj)
  (or (base-read-error? obj)
      (condition-of-type? obj 'read)))

(define (error-object-message err)
  (if (r6rs-condition? err)
    (if (r6rs-message-condition? err)
      (r6rs-condition-message err)
      #f)
    (map cdr
         (filter
           (lambda (obj)
             (and (pair? obj)
                  (equal? 'message (car obj))
                  (string? (cdr obj))))
           (compound-subobjects err)))))

(define (error-object-irritants err)
  (if (r6rs-condition? err)
    (if (r6rs-irritants-condition? err)
      (r6rs-condition-irritants err)
      #f)
    (apply append
           (map cdr
                (filter
                  (lambda (obj)
                    (and (pair? obj)
                         (equal? 'irritants (car obj))
                         (list? (cdr obj))))
                  (compound-subobjects err))))))
