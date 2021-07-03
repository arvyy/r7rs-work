(cond-expand
  (guile
   (import (scheme base)
           (iteration-cl)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (iteration-cl)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (iteration-cl)
           (srfi 64))))


(test-begin "iteration-cl")

(test-group
 "while"
 (define i 0)
 (define lst '())

 (while (< i 3)
   (set! lst (cons i lst))
   (set! i (+ 1 i)))

 (test-equal '(2 1 0) lst))

(test-group
 "until"
 (define i 0)
 (define lst '())

 (until (>= i 3)
        (set! lst (cons i lst))
        (set! i (+ 1 i)))

 (test-equal '(2 1 0) lst))

(test-group
 "do-times"

 (define lst '())
 (define rez
   (do-times i (start 0) (end 10) (step 2) (result lst)
             (set! lst (cons i lst))))

 (test-equal '(8 6 4 2 0) rez))

(test-group
 "do-list"

 (define input-lst '(1 2 3))
 (define output-lst '())
 (do-list i input-lst
          (set! output-lst (cons i output-lst)))
 (test-equal '(3 2 1) output-lst))

(test-group
 "tagged-begin"

 ;; example adapted from http://clhs.lisp.se/Body/s_tagbod.htm
 (define result
   (let ((val #f))
    (tagged-begin
      (set! val 1)
      (point-a)
      (set! val (+ val 16))
     point-c
      (set! val (+ val 04))
      (point-b)
      (set! val (+ val 32))
     point-a
      (set! val (+ val 02))
      (point-c)
      (set! val (+ val 64))
     point-b
      (set! val (+ val 08)))
    val))
 (test-equal result 15))

(test-end)
