(cond-expand
  (chibi
   (import (except (scheme base)
                   error-object?
                   file-error?
                   read-error?
                   error-object-message
                   error-object-irritants)
           (conditions)
           (srfi 1)
           (srfi 222)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (except (scheme base)
                   error-object?
                   file-error?
                   read-error?
                   error-object-message
                   error-object-irritants)
           (conditions)
           (srfi 1)
           (srfi 64)
           (srfi 222))))


(test-begin "conditions")

(test-assert (condition? (make-compound 'foo 'bar)))
(test-assert (not (condition? 'foo)))
(test-assert (condition-of-type? (make-compound 'foo (cons 1 1)) 'foo))
(test-assert (not (condition-of-type? (make-compound 'foo (cons 1 1)) 'bar)))
(test-assert (not (condition-of-type? 'foo 'bar)))
(test-assert (lset= symbol=? '(foo bar) (condition-types (make-compound 'foo 'foo 'bar (cons 1 1)))))
(test-equal '() (condition-types (list 'foo 'bar)))
(test-assert (error-object? (make-compound 'simple (cons 'message "test1") (cons 'message "test2") (cons 'message 'test3))))
(test-equal '("test1" "test2") 
            (error-object-message (make-compound 'simple 
                                                 (cons 'message "test1") 
                                                 (cons 'message "test2") 
                                                 (cons 'message 'test3))))
(test-equal '("test1" "test2" "test3") 
            (error-object-irritants (make-compound 'simple 
                                                   (cons 'irritants (list "test1" "test2")) 
                                                   (cons 'irritants (list "test3")) 
                                                   (cons 'irritants "test4"))))

(test-assert (file-error? (make-compound 'file 'foo)))
(test-assert (read-error? (make-compound 'read 'foo)))

(test-end)
