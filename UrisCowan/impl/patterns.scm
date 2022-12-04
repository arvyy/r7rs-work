;; This file defines pattern (FSA) mechanism
;; used to match URI syntax against input.
;;
;; Each pattern is a procedure, that takes string and index parameters,
;; and returns new string index if it matched (with new index being the next char outside pattern)
;; or #f if it didn't match

(define (match-part input-string start-index pattern)
  (cond
   ((>= start-index (string-length input-string))
    #f)
   ((pattern input-string start-index) => values)
   (else #f)))

;; tries patterns in order, returns first match
(define (or-pattern . patterns)
  (lambda (str index)
    (let loop ((patterns patterns))
      (cond
       ((null? patterns) #f)
       (else (let ((p (car patterns)))
               (cond
                ((p str index) => values)
                (else (loop (cdr patterns))))))))))

;; matches sequence of patterns
(define (seq-pattern . patterns)
  (lambda (str start-index)
    (let loop ((index start-index)
               (patterns patterns))
      (cond
       ((null? patterns) index)
       (else (let ((p (car patterns)))
               (cond
                ((p str index) => (lambda (new-index)
                                    (loop new-index (cdr patterns))))
                (else #f))))))))

;; repeat matches a pattern as much as possible
(define (repeat-pattern pattern)
  (lambda (str index)
    (let loop ((index index))
      (cond
       ((>= index (string-length str)) index)
       ((pattern str index) => (lambda (new-index)
                                 (loop new-index)))
       (else index)))))

;; construct a pattern that matches a char satisfying a predicate function
(define (char-predicate-pattern predicate)
  (lambda (str index)
    (if (and
         (< index (string-length str))
         (predicate (string-ref str index)))
        (+ 1 index)
        #f)))

;; construct a pattern that matches a char if its a member of chars list
(define (char-pattern . chars)
  (lambda (str index)
    (if (and
         (< index (string-length str))
         (member (string-ref str index) chars))
        (+ 1 index)
        #f)))

;; construct a pattern that matches all chars whose codepoint is between char1 and char2
(define (char-range-pattern char1 char2)
  (let ((n1 (char->integer char1))
        (n2 (char->integer char2)))
    (lambda (str index)
      (if (and
           (< index (string-length str))
           (<= n1 (char->integer (string-ref str index)) n2))
          (+ 1 index)
          #f))))

;; construct a pattern that matches a given string
(define (string-pattern pat-str)
  (lambda (str index)
    (define l1 (string-length str))
    (define l2 (string-length pat-str))
    (let loop ((i index)
               (j 0))
      (cond
       ((>= i l1) #f)
       ((>= j l2) i)
       ((char=? (string-ref str i) (string-ref pat-str j))
        (loop (+ 1 i) (+ 1 j)))
       (else #f)))))
