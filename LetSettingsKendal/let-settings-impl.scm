(define (plist-find plist key)
  (cond
    ((null? plist) #f)
    ((equal? key (car plist)) (cadr plist))
    (else (plist-find (cddr plist) key))))

(define-syntax let-settings
  (syntax-rules ()
    ((_ ((keyword ...) plist) . body)
     (let ((plist* plist))
       (let ((keyword (plist-find plist* 'keyword)) ...) . body)))))
