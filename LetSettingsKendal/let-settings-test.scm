(cond-expand
  (guile
   (import (scheme base)
           (let-settings)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (let-settings)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (let-settings)
           (srfi 64))))


(test-begin "let-settings")

(define plist '(a 1 b 2 c 3))

(define plist-getter
  (let ((called #f))
    (lambda ()
      (if called
        (raise "Repeat evaluation")
        (begin
          (set! called #t)
          plist)))))

(test-equal
  '(3 1 #f)
  (let-settings
    ((c a d) (plist-getter))
    (list c a d)))

(test-end)
