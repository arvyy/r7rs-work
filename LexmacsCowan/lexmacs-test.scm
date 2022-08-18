(cond-expand
  (chibi
   (import (scheme base)
           (lexmacs)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (lexmacs)
           (srfi 64))))

(define-record-type <foo>
  (make-foo a b)
  foo?
  (a foo-a)
  (b foo-b))

(test-begin "lexmacs")

(define lex-env (make-lexenv))
(add-to-lexenv! 
  lex-env
  '/foo (lambda (lst)
          (apply make-foo (cdr lst)))
  foo? (lambda (foo)
         (list '/foo 
               (lexmacs-externalize (foo-a foo) lex-env) 
               (lexmacs-externalize (foo-b foo) lex-env))))

(test-equal
  '(1 "2" (/foo #(3) (/foo (4) #t)))
  (lexmacs-externalize (list 1 "2" (make-foo #(3) (make-foo '(4) #t))) lex-env))

(let ((data (lexmacs-internalize '(1 "2" (/foo #(3) (/foo (4) #t))) lex-env)))
  (test-assert (list? data))
  (test-equal 3 (length data))
  (test-equal 1 (list-ref data 0))
  (test-equal "2" (list-ref data 1))
  (test-assert (foo? (list-ref data 2)))
  (test-equal #(3) (foo-a (list-ref data 2)))
  (test-assert (foo? (foo-b (list-ref data 2))))
  (test-equal '(4) (foo-a (foo-b (list-ref data 2))))
  (test-equal #t (foo-b (foo-b (list-ref data 2)))))

(let ((port (open-output-string)))
  (lexmacs-write (make-foo 1 2) lex-env port)
  (test-equal "(/foo 1 2)" (get-output-string port)))

(let ((port (open-output-string)))
  (parameterize
    ((current-output-port port))
    (lexmacs-write (make-foo 1 2) lex-env))
  (test-equal "(/foo 1 2)" (get-output-string port)))

(let ((port (open-input-string "(/foo 1 2)")))
  (define data (lexmacs-read lex-env port))
  (test-assert (foo? data))
  (test-equal 1 (foo-a data))
  (test-equal 2 (foo-b data)))

(let ((port (open-input-string "(/foo 1 2)")))
  (define data
    (parameterize
      ((current-input-port port))
      (lexmacs-read lex-env)))
  (test-assert (foo? data))
  (test-equal 1 (foo-a data))
  (test-equal 2 (foo-b data)))

(test-end)
