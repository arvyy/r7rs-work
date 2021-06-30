(cond-expand
  (guile
   (import (scheme base)
           (restarts)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (restarts)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (restarts)
           (srfi 64))))


(test-begin "restarts")


(test-end)
