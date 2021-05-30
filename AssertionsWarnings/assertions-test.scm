(cond-expand
  (guile
   (import (scheme base)
           (assertions)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (assertions)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (assertions)
           (srfi 64))))


(test-begin "Assertions")

(test-group
 "Test assert"
 (call/cc
  (lambda (k)
    (with-exception-handler
        (lambda (err)
          (test-assert (assertion-object? err))
          (test-equal "assertion-message" (assertion-object-message err))
          (test-equal '(1 2 3) (assertion-object-irritants err))
          (k #t))
      (lambda ()
        (assert #t "assertion-message-unused" 4 5 6)
        (assert #f "assertion-message" 1 2 3)
        ;;shouldn't get here
        (test-assert #f))))))

(test-group
 "Test warn"
 (define port (open-output-string))
 (parameterize ((current-error-port port))
   (with-exception-handler
       (lambda (err)
         (test-assert (warning-object? err))
         (test-equal "warning-message" (warning-object-message err))
         (test-equal '(1 2 3) (warning-object-irritants err))
         "message-to-display")
     (lambda ()
       (warn #t "warning-message-unused" 4 5 6)
       (warn #f "warning-message" 1 2 3)
       (test-equal "message-to-display" (get-output-string (current-error-port)))))))

(test-end)
