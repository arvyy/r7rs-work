;; patterns (FSA) to be used for parsing in uriparse.scm

(define %-encoding-pattern
  (let* ((ranges (vector #\A #\F #\a #\f #\0 #\9))
         (ranges (vector-map char->integer ranges)))
    (define (hex? char)
      (define codepoint (char->integer char))
      (or (<= (vector-ref ranges 0) codepoint (vector-ref ranges 1))
          (<= (vector-ref ranges 2) codepoint (vector-ref ranges 3))
          (<= (vector-ref ranges 4) codepoint (vector-ref ranges 5))))
    (lambda (str index)
      (cond
       ((>= (+ index 2) (string-length str))
        #f)
       ((and (char=? #\% (string-ref str index))
             (hex? (string-ref str (+ 1 index)))
             (hex? (string-ref str (+ 1 index))))
        (+ 3 index))
       (else #f)))))

(define gendelim-pattern
  (char-pattern #\: #\/ #\? #\# #\[ #\] #\@))

(define subdelim-pattern
  (char-pattern #\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=))

(define unreserved-pattern
  (char-pattern #\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=))

(define unreserved-pattern
  (or-pattern
     (char-predicate-pattern char-alphabetic?)
     (char-predicate-pattern char-numeric?)
     (char-pattern #\- #\. #\_ #\~)))

(define ipv4-pattern
  (let* ((digit (char-predicate-pattern char-numeric?))
         (period (char-pattern #\.))
         (dec-octet (or-pattern
                     (seq-pattern (char-pattern #\2)
                                  (char-pattern #\5)
                                  (char-range-pattern #\0 #\5))
                     (seq-pattern (char-pattern #\2)
                                  (char-range-pattern #\0 #\4)
                                  digit)
                     (seq-pattern (char-pattern #\1)
                                  digit
                                  digit)
                     (seq-pattern (char-range-pattern #\1 #\9)
                                  digit)
                     digit)))
    (seq-pattern
     dec-octet period
     dec-octet period
     dec-octet period
     dec-octet)))


(define ipv6-pattern
  (seq-pattern
   (char-pattern #\[)
   (repeat-pattern
    (or-pattern
     unreserved-pattern
     (char-pattern #\:)))
   (char-pattern #\])))

(define pchar-pattern
  (or-pattern
   unreserved-pattern
   %-encoding-pattern
   subdelim-pattern
   (char-pattern #\:)
   (char-pattern #\@)))

(define segment-pattern
  (repeat-pattern pchar-pattern))

(define segment-nz-pattern
  (seq-pattern
   pchar-pattern
   (repeat-pattern pchar-pattern)))

(define segment-nz-nc-pattern
  (let ((pchar-except-colon (or-pattern
                             unreserved-pattern
                             %-encoding-pattern
                             subdelim-pattern
                             (char-pattern #\@))))
    (seq-pattern
     pchar-except-colon
     (repeat-pattern pchar-except-colon))))

(define path-absolute-pattern
  (or-pattern (seq-pattern (char-pattern #\/)
                           segment-nz-pattern
                           (repeat-pattern
                            (seq-pattern
                             (char-pattern #\/)
                             segment-pattern)))
              (char-pattern #\/)))

(define query-pattern
  (seq-pattern
   (char-pattern #\?)
   (repeat-pattern
    (or-pattern
     pchar-pattern
     (char-pattern #\/)
     (char-pattern #\?)))))

(define fragment-pattern
  (seq-pattern
   (char-pattern #\#)
   (repeat-pattern
    (or-pattern
     pchar-pattern
     (char-pattern #\/)
     (char-pattern #\?)))))

(define userinfo-pattern
  (seq-pattern
   (repeat-pattern
    (or-pattern unreserved-pattern
                %-encoding-pattern
                subdelim-pattern
                (char-pattern #\:)))
   (char-pattern #\@)))

(define host-pattern
  (or-pattern
   ipv4-pattern
   ipv6-pattern
   (repeat-pattern
    (or-pattern
     unreserved-pattern
     %-encoding-pattern
     subdelim-pattern))))

(define port-pattern
  (seq-pattern
   (char-pattern #\:)
   (repeat-pattern
    (char-predicate-pattern char-numeric?))))

(define scheme-pattern
  (let ((allowed-char? (lambda (c)
                         (or (char-alphabetic? c) (char-numeric? c) (char=? c #\+) (char=? c #\-) (char=? c #\.)))))
    (seq-pattern
     (char-predicate-pattern char-alphabetic?)
     (repeat-pattern
      (char-predicate-pattern allowed-char?))
     (char-pattern #\:))))
