(cond-expand
  (guile
   (import (scheme base)
           ({{cookiecutter.name}})
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           ({{cookiecutter.name}})
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           ({{cookiecutter.name}})
           (srfi 64))))


(test-begin "{{cookiecutter.name}}")


(test-end)
