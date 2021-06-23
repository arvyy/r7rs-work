(define (string-interpolate base-string mapping)
  (interp (string->list base-string)
          '()
          mapping))

;; recursive dispatcher
(define (interp chars res/rev mapping)
  (cond
   ;; end of input -- reverse and return
   ((null? chars) (list->string (reverse res/rev)))

   ;; dollar -- check next char and dispatch
   ((equal? #\$ (car chars))
    (let* ((chars* (cdr chars))
           (s (if (null? chars*) #f (car chars*))))
      (cond
       ((not s) (error "'$' as a last character in base-string"))
       ((equal? #\[ s) (let-values (((rest-chars new-res/rev) (handle-interpolation (cdr chars*) res/rev mapping)))
                       (interp rest-chars new-res/rev mapping)))
       ((equal? #\; s) (let ((rest-chars (handle-line-comment (cdr chars*))))
                       (interp rest-chars res/rev mapping)))
       ((equal? #\| s) (let ((new-res/rev (handle-preceding-trim res/rev)))
                         (interp (cdr chars*) new-res/rev mapping)))
       ((equal? #\# s) (let ((rest-chars (handle-block-comment chars*)))
                         (interp rest-chars res/rev mapping)))
       ((equal? #\$ s) (interp (cdr chars*) (cons s res/rev) mapping))
       (else (error (string-append "Bad char after '$': " (string s)))))))

   ;; otherwise just add char literally
   (else (interp (cdr chars)
                 (cons (car chars) res/rev)
                 mapping))))

;; handle interpolating
;; chars is rest of input as char list immediately after $[
;; res/rev is the reversed current output
;; returns 2 values: rest-chars and new-res/rev
(define (handle-interpolation chars res/rev mapping)
  ;; extract key and rest chars after it
  (define-values (key rest-chars)
    (let loop ((chars chars)
               (key '()))
      (cond
       ((null? chars) (error "Unexpected end of base-string while parsing '$['"))
       (else (let ((c (car chars)))
               (if (equal? c #\])
                   (values (list->string (reverse key))
                           (cdr chars))
                   (loop (cdr chars)
                         (cons (car chars) key))))))))

  ;; lookup string value
  (define str-value
    (let ((port (open-output-string)))
      (display (mapping-ref mapping key) port)
      (get-output-string port)))

  ;; add string value to reversed result list
  (define new-res/rev (append (reverse (string->list str-value)) res/rev))

  (values rest-chars
          new-res/rev))

;; handle line comment
;; chars is rest of input as char list immediately after $;
;; returns rest-chars
(define (handle-line-comment chars)
  (cond
   ((null? chars) chars)
   ((equal? (car chars) #\newline) chars)
   (else (handle-line-comment (cdr chars)))))

;; handle block comment
;; chars is rest of input as char list after $ but before #
;; returns rest-chars
(define (handle-block-comment chars)

  (define (follows? char1 char2 chars)
    (and (not (null? chars))
         (equal? char1 (car chars))
         (not (null? (cdr chars)))
         (equal? char2 (cadr chars))))

  (define (block-start? chars)
    (follows? #\# #\| chars))

  (define (block-end? chars)
    (follows? #\| #\# chars))

  ;; we know it started with '$#', but should check that '|' follows it
  (unless (block-start? chars)
    (error "Expected '|' after '$#' (block comment)"))

  (let loop ((chars chars)
             (level 0))
    (cond
     ((null? chars) (error "Unexpected end of base string while inside block comment"))
     ((block-start? chars) (loop (cddr chars) (+ level 1)))
     ((block-end? chars) (if (<= level 1)
                             (cddr chars) ;; closing last level -- return
                             (loop (cddr chars)
                                   (- level 1))))
     (else (loop (cdr chars) level)))))

;; handle whitespace trimming
;; res/rev is the reversed current output
;; returns new-res/rev
(define (handle-preceding-trim res/rev)
  (cond
   ((null? res/rev) res/rev)
   (else (let ((c (car res/rev)))
           (cond
            ((equal? #\space c) (handle-preceding-trim (cdr res/rev)))
            ((equal? #\newline c) res/rev)
            (else (error (string-append "non-space char found between newline and '$|': " (string c)))))))))
