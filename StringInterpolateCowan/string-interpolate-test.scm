(cond-expand
  (guile
   (import (scheme base)
           (scheme mapping)
           (scheme comparator)
           (string-interpolate)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (scheme mapping)
           (scheme comparator)
           (string-interpolate)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (scheme mapping)
           (scheme comparator)
           (string-interpolate)
           (srfi 64))))


(test-begin "string-interpolate")

(test-group
 "Test $$"

 (define base-string "foo $$ bar")
 (define mapping* (mapping (make-default-comparator)))

 (test-equal
     "foo $ bar"
   (string-interpolate base-string mapping*)))

(test-group
 "Test $;"

 (define mapping* (mapping (make-default-comparator)))

 (test-equal
     "foo\nbaz"
   (string-interpolate "foo$; bar\nbaz" mapping*))

 (test-equal
     "foo"
   (string-interpolate "foo$; bar" mapping*)))

(test-group
 "Test $|"

 (define mapping* (mapping (make-default-comparator)))

 (test-equal
     "foo\nbar"
   (string-interpolate "foo\n     $|bar" mapping*))

 (test-equal
     "foo"
   (string-interpolate "    $|foo" mapping*))

 (call/cc
  (lambda (k)
    (with-exception-handler
        (lambda (err)
          (k #t))
      (lambda ()
        ;; should throw due to non-space chars in between newline and $|'
        (string-interpolate "  foo $|" mapping*)))
    ;; shouldn't get here
    (test-assert #f))))

(test-group
 "Test $#|"

 (define mapping* (mapping (make-default-comparator)))

 (test-equal
     "foo  bar"
   (string-interpolate "foo $#| comment #| \nnested $;|#|# bar" mapping*))

 (call/cc
  (lambda (k)
    (with-exception-handler
        (lambda (err)
          (k #t))
      (lambda ()
        ;; should throw due to missing '|#'
        (string-interpolate "foo $#|" mapping*)))
    ;; shouldn't get here
    (test-assert #f))))

(test-group
 "Test $[...]"

 (define mapping* (mapping (make-default-comparator) "k1" 1 "k2" 'foo "k3" '(a b)))

 (test-equal
     "1 foo (a b)"
   (string-interpolate "$[k1] $[k2] $[k3]" mapping*))

 (call/cc
  (lambda (k)
    (with-exception-handler
        (lambda (err)
          (k #t))
      (lambda ()
        ;; should throw due to missing ']'
        (string-interpolate "foo $[k1" mapping*)))
    ;; shouldn't get here
    (test-assert #f))))

(test-end)
