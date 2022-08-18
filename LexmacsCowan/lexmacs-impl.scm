(define-record-type <lexenv>
  (make-lexenv-private entries)
  lexenv?
  (entries entries set-entries!))

(define (make-lexenv)
  (make-lexenv-private (list)))

(define-record-type <lexenv-entry>
  (lexenv-entry symbol internalizer predicate externalizer)
  lexenv-entry?
  (symbol entry-symbol)
  (internalizer entry-internalizer)
  (predicate entry-predicate)
  (externalizer entry-externalizer))

(define (add-to-lexenv! lexenv symbol internalizer predicate externalizer)
  (assume (lexenv? lexenv))
  (assume (or (and (equal? #f symbol)
                   (equal? #f internalizer))
              (and (symbol? symbol)
                   (procedure? internalizer))))
  (assume (or (and (equal? #f predicate)
                   (equal? #f externalizer))
              (and (procedure? predicate)
                   (procedure? externalizer))))
  (set-entries! lexenv (cons (lexenv-entry symbol internalizer predicate externalizer)
                             (entries lexenv))))

(define (find-by-symbol lexenv symbol)
  (find 
    (lambda (e)
      (define s (entry-symbol e))
      (and s (symbol=? s symbol)))
    (entries lexenv)))

(define (find-by-predicate lexenv obj)
  (find
    (lambda (e)
      (define p (entry-predicate e))
      (and p (p obj)))
    (entries lexenv)))

(define (lexmacs-internalize obj lexenv)
  (define (do-internalize obj)
    ;; get internalizer procedure if this is an externalized object with recognized symbol
    (define internalizer
      (cond
        ((and (list? obj)
              (not (null? obj))
              (symbol? (car obj)))
         (let* ((symbol (car obj))
                (entry (find-by-symbol lexenv symbol)))
           (if entry
             (entry-internalizer entry) 
             #f)))
        (else #f)))
    (cond
      (internalizer (internalizer (cons (car obj) (map do-internalize (cdr obj)))))
      ((list? obj) (map do-internalize obj))
      ((vector? obj) (vector-map do-internalize obj))
      (else obj)))
  (assume (lexenv? lexenv))
  (do-internalize obj))

(define (lexmacs-externalize obj lexenv)
  (define (do-externalize obj)
    (define e (find-by-predicate lexenv obj))
    (define (externalize-shallow obj)
      (if e
        ((entry-externalizer e) obj)
        obj))
    (cond
      ((list? obj) (externalize-shallow (map do-externalize obj)))
      ((vector? obj) (externalize-shallow (vector-map do-externalize obj)))
      (else (externalize-shallow obj))))
  (assume (lexenv? lexenv))
  (do-externalize obj))

(define lexmacs-read
  (case-lambda 
    ((lexenv) (lexmacs-read lexenv (current-input-port)))
    ((lexenv port)
     (assume (lexenv? lexenv))
     (lexmacs-internalize (read port) lexenv))))

(define lexmacs-write
  (case-lambda 
    ((obj lexenv) (lexmacs-write obj lexenv (current-output-port)))
    ((obj lexenv port)
     (assume (lexenv? lexenv))
     (write (lexmacs-externalize obj lexenv) port))))

(define (lexmacs-eval obj lexenv env)
  (eval (lexmacs-internalize obj lexenv) env))
