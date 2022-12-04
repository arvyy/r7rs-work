;; misc procedures

(define (string-split str chars)
  (define len (string-length str))
  (let loop ((i 0)
             (chunks '())
             (chunk-start 0))
    (cond
     ((>= i len) (reverse (cons (substring str chunk-start i) chunks)))
     (else (let ((char (string-ref str i)))
             (if (member char chars)
                 (loop (+ 1 i) (cons (substring str chunk-start i) chunks) (+ 1 i))
                 (loop (+ 1 i) chunks chunk-start)))))))

(define (string-starts-with str1 str2)
  (define len1 (string-length str1))
  (define len2 (string-length str2))
  (let loop ((i 0))
    (cond
     ((>= i len2) #t)
     ((>= i len1) #f)
     ((char=? (string-ref str1 i)
              (string-ref str2 i))
      (loop (+ 1 i)))
     (else #f))))

(define (index-of str char)
  (define len (string-length str))
  (let loop ((i 0))
    (cond
     ((>= i len) #f)
     ((char=? char (string-ref str i)) i)
     (else (loop (+ 1 i))))))

(define (decode-% str exception)
  (define len (string-length str))
  (let loop ((chunks '())
             (chunk-start 0)
             (i 0))
    (cond
     ((>= i len)
      (apply string-append (reverse (cons (substring str chunk-start i) chunks))))
     ((char=? #\% (string-ref str i))
      (begin
        (unless (%-encoding-pattern str i)
          (validation-error))
        (let* ((codepoint-str (substring str (+ i 1) (+ i 3)))
               (codepoint (string->number codepoint-str 16))
               (char (integer->char codepoint)))
          (if (member char exception)
              (loop (append (list (string-downcase (substring str i (+ i 3))) (substring str chunk-start i)) chunks) (+ 3 i) (+ 3 i))
              (loop (append (list (string char) (substring str chunk-start i)) chunks) (+ 3 i) (+ 3 i))))))
     (else (loop chunks chunk-start (+ 1 i))))))

(define (bytevector->text charset content)
  (cond
   ((or (string=? charset "US-ASCII")
        (string=? charset "UTF-8"))
    (utf8->string content))
   (else
    (raise (string-append "Unrecognized charset: " charset)))))
