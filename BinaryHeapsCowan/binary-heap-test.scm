(cond-expand
  (guile
   (import (scheme base)
           (binary-heap)
           (srfi srfi-64)
           (srfi srfi-128)))
  (chibi
   (import (scheme base)
           (srfi 128)
           (binary-heap)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (binary-heap)
           (srfi 128)
           (srfi 64))))

(define (heap->list heap)
  (let loop ((lst '()))
    (if (heap-empty? heap)
      (reverse lst)
      (loop (cons (heap-pop! heap) lst)))))

(test-begin "binary-heap")

(test-group 
  "heap-insert!, heap-pop!, heap-top, heap-count, heap-size"

  ;; no key
  (let ((heap (make-heap (make-default-comparator) 10 #f)))
    (test-equal 10 (heap-size heap))
    (test-equal 0 (heap-count heap))
    (heap-insert! heap 2)
    (test-equal 1 (heap-count heap))
    (test-equal 2 (heap-top heap))
    (heap-insert! heap 3)
    (test-equal 2 (heap-top heap))
    (test-equal 2 (heap-count heap))
    (heap-insert! heap 1)
    (test-equal 1 (heap-top heap))
    (test-equal 3 (heap-count heap))
    (test-equal '(1 2 3) (heap->list heap)))

  ;; with key
  (let ((heap (make-heap (make-default-comparator) #f cdr)))
    (heap-insert! heap (cons 'a 2))
    (heap-insert! heap (cons 'b 3))
    (heap-insert! heap (cons 'c 1))
    (test-equal '((c . 1) (a . 2) (b . 3)) (heap->list heap))
    (test-equal cdr (heap-key heap)))

  ;; test grow
  (let ((heap (make-heap (make-default-comparator) 2 #f)))
    (test-equal 2 (heap-size heap))
    (heap-insert! heap 1)
    (heap-insert! heap 2)
    (heap-insert! heap 3)
    (test-assert (> (heap-size heap) 2))))

(test-group
  "heap-delete!"

  ;; delete all
  (let ((heap (make-heap (make-default-comparator) #f #f)))
    (heap-insert! heap 1)
    (heap-insert! heap 2)
    (heap-insert! heap 3)
    (heap-insert! heap 2)
    (test-equal 2 (heap-delete! heap 2 #f))
    (test-equal '(1 3) (heap->list heap)))

  ;; delete some
  (let ((heap (make-heap (make-default-comparator) #f #f)))
    (heap-insert! heap 1)
    (heap-insert! heap 2)
    (heap-insert! heap 3)
    (heap-insert! heap 2)
    (test-equal 1 (heap-delete! heap 2 1))
    (test-equal '(1 2 3) (heap->list heap)))

  ;; nothing to delete
  (let ((heap (make-heap (make-default-comparator) #f #f)))
    (heap-insert! heap 1)
    (heap-insert! heap 3)
    (test-equal 0 (heap-delete! heap 2 #f))
    (test-equal '(1 3) (heap->list heap))))

(test-group
  "heap-map"

  (let ((heap (make-heap (make-default-comparator) #f #f)))
    (heap-insert! heap 1)
    (heap-insert! heap 2)
    (heap-insert! heap 3)
    (test-equal '(-3 -2 -1) (heap->list (heap-map - heap)))
    (test-equal '(1 2 3) (heap->list heap))))

(test-end)
