;; This file implements data: scheme handling and relating concerns

;; convert a base64 char to a [0 .. 63] number
(define base64char->int
  (let ((charA (char->integer #\A))
        (charZ (char->integer #\Z))
        (chara (char->integer #\a))
        (charz (char->integer #\z))
        (char0 (char->integer #\0))
        (char9 (char->integer #\9)))
    (lambda (char)
      (define codepoint (char->integer char))
      (cond
       ((char=? #\+ char) 62)
       ((char=? #\/ char) 63)
       ((<= codepoint char9) (+ 52 (- codepoint char0)))
       ((<= codepoint charZ) (- codepoint charA))
       ((<= codepoint charz) (+ 26 (- codepoint chara)))
       (else (validation-error))))))

;; take 4 chars starting at index and return a base64 decoded bytevector of 1-3 length
(define (base64-quadruple->bytevector str index)
  (define i1 (base64char->int (string-ref str index)))
  (define i2 (base64char->int (string-ref str (+ 1 index))))
  (define i3 (let ((char (string-ref str (+ 2 index))))
               (if (char=? char #\=)
                   #f
                   (base64char->int char))))
  (define i4 (let ((char (string-ref str (+ 3 index))))
               (if (char=? char #\=)
                   #f
                   (base64char->int char))))
  (define input
    (+ (arithmetic-shift i1 18)
       (arithmetic-shift i2 12)
       (if i3 (arithmetic-shift i3 6) 0)
       (if i4 i4 0)))
  (define o1 (bitwise-and (arithmetic-shift input -16) #xFF))
  (define o2 (bitwise-and (arithmetic-shift input -8) #xFF))
  (define o3 (bitwise-and input #xFF))
  (cond
   ((and i3 i4) (bytevector o1 o2 o3))
   (i3 (bytevector o1 o2))
   (else (bytevector o1))))

;; read base64 data into bytevector starting from idnex
(define (read-data-bytevector/base64 str index)
  (define len (string-length str))
  ;; how many `=' the string ends with
  (define padding-count
    (cond
     ((not (char=? #\= (string-ref str (- len 1))))
      0)
     ((char=? #\= (string-ref str (- len 2)))
      2)
     (else 1)))
  ;; each 4 chars in base64 data string encodes 3 bytes of binary data
  ;; subtract padding at the end
  (define bytecount
    (- (* 3 (/ (- len index) 4))
       padding-count))
  (define bv (make-bytevector bytecount))
  (let loop ((i index)
             (j 0))
    (cond
     ((>= i len)
      bv)
     (else (let* ((quadruple-bv (base64-quadruple->bytevector str i)))
             (bytevector-copy! bv j quadruple-bv)
             (loop (+ i 4)
                   (+ j (bytevector-length quadruple-bv))))))))

;; read url encoded data into bytevector starting from index
(define (read-data-bytevector/url-encoded str index)
  (define strlen (string-length str))
  (define bytecount
    (let loop ((i index)
               (count 0))
      (cond
       ((>= i strlen) count)
       ((char=? #\% (string-ref str i)) (loop (+ 3 i)
                                              (+ count 1)))
       (else (loop (+ i 1)
                   (+ count 1))))))
  (define bv (make-bytevector bytecount))
  (let loop ((i index)
             (j 0))
    (cond
     ((>= i strlen) bv)
     ((char=? #\% (string-ref str i))
      (begin
        (bytevector-u8-set! bv j (string->number (substring str (+ 1 i) (+ 3 i)) 16))
        (loop (+ 3 i)
              (+ 1 j))))
     (else
      (begin
        (bytevector-u8-set! bv j (char->integer (string-ref str i)))
        (loop (+ 1 i) (+ 1 j)))))))

(define do-parse-uri-data
  (letrec* ((restricted-name-first (or-pattern
                                    (char-predicate-pattern char-alphabetic?)
                                    (char-predicate-pattern char-numeric?)))
            (restricted-name-chars (or-pattern
                                    restricted-name-first
                                    (char-predicate-pattern (let ((allowed '(#\! #\# #\$ #\& #\- #\^ #\_)))
                                                              (lambda (c)
                                                                (member c allowed))))))
            (restricted-name-pattern (seq-pattern
                                      restricted-name-first
                                      (repeat-pattern restricted-name-chars)))
            (mediatype-noparams-pattern (seq-pattern
                                         restricted-name-pattern
                                         (char-pattern #\/)
                                         restricted-name-pattern))
            (mediatype-param-pattern (seq-pattern
                                      (char-pattern #\;)
                                      restricted-name-pattern
                                      (char-pattern #\=)
                                      restricted-name-pattern))
            (parse-param (lambda (str index)
                           (define match (match-part str index mediatype-param-pattern))
                           (if match
                               (let* ((param (substring str (+ 1 index) match))
                                      (parts (string-split param '(#\=)))
                                      (key (list-ref parts 0))
                                      (value (list-ref parts 1)))
                                 (values (cons key value) match))
                               (values #f index))))
            (parse-params (lambda (str index)
                            (let loop ((index index)
                                       (params '()))
                              (define-values (param new-index) (parse-param str index))
                              (if param
                                  (loop new-index (cons param params))
                                  (values (reverse params) new-index)))))
            (parse-media-type (lambda (str index)
                                (define match-beforeparams (match-part str index mediatype-noparams-pattern))
                                (if match-beforeparams
                                    (let*-values (((params index) (parse-params str match-beforeparams)))
                                      (values (substring str 0 index) params index))
                                    (values "text/plain;charset=US-ASCII" '(("charset" . "US-ASCII")) index))))
            (base64-marker-pattern (string-pattern ";base64"))
            (parse-base64? (lambda (str index)
                             (define match (match-part str index base64-marker-pattern))
                             (if match
                                 (values #t match)
                                 (values #f index)))))
    (lambda (content)
      (let*-values (((mediatype params index) (parse-media-type content 0))
                    ((base64? index) (parse-base64? content index))
                    ((textual?) (string-starts-with mediatype "text/")))
        (unless (and (< index (string-length content))
                     (char=? #\, (string-ref content index)))
          (validation-error))
        (let* ((charset (cond
                         ((assoc "charset" params) => cdr)
                         (else "US-ASCII")))
               (content (if base64?
                            (read-data-bytevector/base64 content (+ 1 index))
                            (read-data-bytevector/url-encoded content (+ 1 index))))
               (content (if textual?
                            (bytevector->text charset content)
                            content)))
          (values mediatype params content))))))

(define (uri-parse-data uri)
  (cond
   ((not (equal? "data" (uri-scheme uri)))
    (values #f #f #f))
   (else (do-parse-uri-data (uri-path uri)))))
