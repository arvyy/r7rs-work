(define-syntax while
  (syntax-rules ()
    ((while expr . body)
     (let loop ()
       (when expr
         (let () . body)
         (loop))))))

(define-syntax until
  (syntax-rules ()
    ((until expr . body)
     (let loop ()
       (unless expr
         (let () . body)
         (loop))))))


(define-syntax do-times
  (syntax-rules (start end step result)
    ((do-times var (start start*) (end end*) (step step*) (result result*) . body)
     (begin
       (let ((s start*) (e end*) (t step*))
         (let loop ((var s))
           (when (< var e)
             (let () . body)
             (loop (+ var t)))))
       result*))))

(define-syntax do-list
  (syntax-rules ()
    ((_ var lst . body)
     (let loop ((l lst))
       (cond
        ((null? l) #t)
        (else (let ((var (car l)))
                (let () . body)
                (loop (cdr l)))))))))

;; from https://mumble.net/~campbell/tmp/tagbody.scm
(define-syntax tagged-begin
  (syntax-rules ()
    ((tagged-begin ?b ...)
     (%tagbody tagbody-start ((tagbody-start #f)) ?b ...))))

(define-syntax %tagbody
  (syntax-rules ()
    ((%tagbody ?start-tag ((?tag ?b ...) ...))
     ((call-with-current-continuation
       (lambda (tagbody-trampoline)
         (define (?tag) (tagbody-trampoline (lambda () ?b ...)))
         ...
         (?start-tag)))))
    ((%tagbody ?start-tag ((?tag ?b ...) . ?tags) ?b0 . ?b1+)
     (syntactic-name? ?b0
                      (%tagbody ?start-tag ((?b0 #f) (?tag ?b ... (go ?b0)) . ?tags) . ?b1+)
                      (%tagbody ?start-tag ((?tag ?b ... ?b0) . ?tags) . ?b1+)))))

(define-syntax syntactic-name?
  (syntax-rules ()
    ((syntactic-name? (?a . ?d) ?if-yes ?if-no) ?if-no)
    ((syntactic-name? #(?v ...) ?if-yes ?if-no) ?if-no)
    ((syntactic-name? ?datum ?if-yes ?if-no)
     (let-syntax ((test-ellipsis
                   (syntax-rules ()
                     ((test-ellipsis (??variable ?datum) ??yes ??no) ??yes)
                     ((test-ellipsis ??otherwise ??yes ??no) ??no))))
       (test-ellipsis (magical mystery list)
                      ?if-yes
                      (let-syntax
                          ((test-name
                            (syntax-rules ()
                              ((test-name ?datum ??yes ??no) ??yes)
                              ((test-name ??otherwise ??yes ??no) ??no))))
                        (test-name magical-mystery-name ?if-yes ?if-no)))))))
