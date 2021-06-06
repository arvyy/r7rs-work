(cond-expand
  (guile
   (import (scheme base)
           (srfi-17-ext)
           (srfi srfi-17)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (srfi-17-ext)
           (srfi 17)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (srfi-17-ext)
           (srfi 17)
           (srfi 64))))


(test-begin "SRFI-17 extension")

(test-group
 "Test push!"

 (let ((queue '(2 3)))
   (push! queue 1)
   (test-equal '(1 2 3) queue))

 (let* ((queue '(2 3))
        (v (vector queue)))
   (push! (vector-ref v 0) 1)
   (test-equal '(1 2 3) (vector-ref v 0))))

(test-group
 "Test pop!"

 (let ((queue '(2 3)))
   (pop! queue)
   (test-equal '(3) queue))

 (let* ((queue '(2 3))
        (v (vector queue)))
   (pop! (vector-ref v 0))
   (test-equal '(3) (vector-ref v 0))))

(test-group
 "Test inc!"

 (let ((value 1))
   (inc! value)
   (test-equal 2 value)
   (inc! value 2)
   (test-equal 4 value))

 (let ((v (vector 1)))
   (inc! (vector-ref v 0))
   (test-equal 2 (vector-ref v 0))
   (inc! (vector-ref v 0) 2)
   (test-equal 4 (vector-ref v 0))))

(test-group
 "Test dec!"

 (let ((value 4))
   (dec! value)
   (test-equal 3 value)
   (dec! value 2)
   (test-equal 1 value))

 (let ((v (vector 4)))
   (dec! (vector-ref v 0))
   (test-equal 3 (vector-ref v 0))
   (dec! (vector-ref v 0) 2)
   (test-equal 1 (vector-ref v 0))))

(test-group
 "Test update!"
 (define (updater value) (+ 1 value))

 (let ((value 1))
   (update! value updater)
   (test-equal 2 value))

 (let ((v (vector 1)))
   (update! (vector-ref v 0) updater)
   (test-equal 2 (vector-ref v 0))))

(test-end)
