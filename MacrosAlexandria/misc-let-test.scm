(cond-expand
  (guile
   (import (scheme base)
           (scheme char)
           (misc-let)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (scheme char)
           (misc-let)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (scheme char)
           (misc-let)
           (srfi 64))))


(test-begin "MacrosAlexandria misc-let")

(test-group
 "Test let-list"

 (let-list
  (a b c)
  '(1 2 3)
  (test-equal a 1)
  (test-equal b 2)
  (test-equal c 3))

 (let-list
  (a b)
  '(1 2 3)
  (test-equal a 1)
  (test-equal b 2))

 (let-list
  (a . b)
  '(1 2 3)
  (test-equal a 1)
  (test-equal b '(2 3))))

(test-group
 "if-let"

 (if-let ((a 1)
          (b 2))
         (begin
           (test-equal a 1)
           (test-equal b 2))
         (test-assert #f))

 (if-let ((a 1)
          (b #f))
         (test-assert #f)
         (begin
           (test-equal a 1)
           (test-equal b #f))))

(test-group
 "if-let*"

 (if-let* ((a 1)
           (b (+ a 1)))
          (begin
            (test-equal a 1)
            (test-equal b 2))
          (test-assert #f))

 (if-let* ((a 1)
           (b (not a)))
          (test-assert #f)
          (begin
            (test-equal a 1)
            (test-equal b #f))))

(test-group
 "when-let"

 (call/cc
  (lambda (k)
    (when-let ((a 1)
               (b 2))
              (begin
                (test-equal a 1)
                (test-equal b 2)
                (k #t)))
    ;;shouldn't get here
    (test-assert #f)))

 (when-let ((a 1)
            (b #f))
           ;; shouldn't get here
           (test-assert #f)))

(test-group
 "when-let*"

 (call/cc
  (lambda (k)
    (when-let* ((a 1)
                (b (+ a 1)))
              (begin
                (test-equal a 1)
                (test-equal b 2)
                (k #t)))
    ;;shouldn't get here
    (test-assert #f)))

 (when-let ((a 1)
            (b #f))
           ;; shouldn't get here
           (test-assert #f)))

(test-group
 "case-using"

 (test-equal 'b
   (case-using string-ci=? "d"
               (("A" "B") 'a)
               (("C" "D") 'b)))

 (test-equal 'b
   (case-using string-ci=? "d"
               (("A" "B") 'a)
               (else 'b)))

 (test-equal 'b
   (case-using string-ci=? "d"
               (("A" "B") 'a)
               (("C" "D") => (lambda (arg)
                               (test-equal "d" arg)
                               'b))))

 (test-equal 'b
   (case-using string-ci=? "d"
               (("A" "B") 'a)
               (else => (lambda (arg)
                          (test-equal "d" arg)
                          'b)))))

(test-group
 "andmap"

 (define (fn x)
   (if x
       (+ 1 x)
       #f))

 (test-equal 4 (andmap fn 1 2 3))
 (test-equal #f (andmap fn 1 2 #f (test-assert #f)))
 (test-equal #t (andmap fn)))


(test-group
 "ormap"

 (define (fn x)
   (if x
       (+ 1 x)
       #f))

 (test-equal 2 (ormap fn 1 2 3))
 (test-equal 2 (ormap fn #f #f 1 (test-assert #f)))
 (test-equal #f (ormap fn)))

(test-end)
