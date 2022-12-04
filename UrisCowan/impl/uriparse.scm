;; URI parsing logic using patterns defined in uriparsepatterns.scm

(define parse-scheme
  (lambda (input-string start-index)
      (define match (match-part input-string start-index scheme-pattern))
      (if match
          (values (substring input-string start-index (- match 1)) match)
          (values #f start-index))))

(define parse-userinfo
  (lambda (input-string start-index)
      (define match (match-part input-string start-index userinfo-pattern))
      (if match
          (values (substring input-string start-index (- match 1)) match)
          (values #f start-index))))

(define parse-host
  (lambda (input-string start-index)
      (define match (match-part input-string start-index host-pattern))
      (if match
          (values (substring input-string start-index match) match)
          (values #f start-index))))

(define parse-port
  (lambda (input-string start-index)
      (define match (match-part input-string start-index port-pattern))
      (if match
          (values (string->number (substring input-string (+ 1 start-index) match)) match)
          (values #f start-index))))

(define parse-path/after-authority
  (let ((pattern (repeat-pattern
                  (seq-pattern
                   (char-pattern #\/)
                   segment-pattern))))
    (lambda (input-string start-index)
      (define match (match-part input-string start-index pattern))
      (if match
          (values (substring input-string start-index match) match)
          (values "" start-index)))))

(define parse-path/after-scheme
  (let* ((path-rootless-pattern (seq-pattern segment-nz-pattern
                                             (repeat-pattern (seq-pattern (char-pattern #\/)
                                                                          segment-pattern))))
         (pattern (or-pattern
                   path-absolute-pattern
                   path-rootless-pattern)))
    (lambda (input-string start-index)
      (define match (match-part input-string start-index pattern))
      (if match
          (values (substring input-string start-index match) match)
          (values "" start-index)))))

(define parse-path/no-scheme
  (let* ((path-noscheme-pattern (seq-pattern segment-nz-nc-pattern
                                             (repeat-pattern (seq-pattern (char-pattern #\/)
                                                                          segment-pattern))))
         (pattern (or-pattern
                   path-absolute-pattern
                   path-noscheme-pattern)))
    (lambda (input-string start-index)
      (define match (match-part input-string start-index pattern))
      (if match
          (values (substring input-string start-index match) match)
          (values "" start-index)))))

(define parse-query
  (lambda (input-string start-index)
      (define match (match-part input-string start-index query-pattern))
      (if match
          (values (substring input-string (+ 1 start-index) match) match)
          (values #f start-index))))

(define parse-fragment
  (lambda (input-string start-index)
      (define match (match-part input-string start-index fragment-pattern))
      (if match
          (values (substring input-string (+ 1 start-index) match) match)
          (values #f start-index))))

(define (parse-authority str start-index)
  (cond
   ((match-part str start-index (seq-pattern (char-pattern #\/) (char-pattern #\/))) =>
    (lambda (index)
      (let*-values (((userinfo index) (parse-userinfo str index))
                    ((host index) (parse-host str index))
                    ((port index) (parse-port str index)))
        (values (substring str (+ 2 start-index) index) index))))
   (else (values #f start-index))))

(define (parse-specific str start-index)
  (let*-values (((authority index) (parse-authority str start-index))
                ((path index) (if authority
                                  (parse-path/after-authority str index)
                                  (parse-path/after-scheme str index)))
                ((query index) (parse-query str index)))
    (values (substring str start-index index) index)))

(define (whole->children whole)
  (let*-values (((scheme index) (parse-scheme whole 0))
                ((specific index) (parse-specific whole index))
                ((fragment index) (parse-fragment whole index)))
    (values scheme specific fragment)))

(define (whole<-children scheme specific fragment)
  (string-append
   (if scheme
       (string-append scheme ":")
       "")
   (if specific
       specific
       "")
   (if fragment
       (string-append "#" fragment)
       "")))

(define (specific->children specific)
  (let*-values (((authority index) (parse-authority specific 0))
                ((path index) (if authority
                                  (parse-path/after-authority specific index)
                                  (parse-path/after-scheme specific index)))
                ((query index) (parse-query specific index)))
    (values authority path query)))

(define (specific<-children authority path query)
  (when (and query
             (not path)
             (not authority))
    (validation-error))
  (string-append
   (if authority
       (string-append "//" authority)
       "")
   (if path
       path
       "")
   (if query
       (string-append "?" query)
       "")))

(define (authority->children authority)
  (if authority
      (let*-values (((userinfo index) (parse-userinfo authority 0))
                    ((host index) (parse-host authority index))
                    ((port index) (parse-port authority index)))
        (unless host
          (validation-error))
        (values userinfo host port))
      (values #f #f #f)))

(define (authority<-children userinfo host port)
  (unless host
    (validation-error))
  (string-append
       (if userinfo
           (string-append userinfo "@")
           "")
       host
       (if port
           (string-append ":" (number->string port))
           "")))

(define (userinfo->children userinfo)
  (if userinfo
      (let ((colon-index (index-of userinfo #\:)))
        (if colon-index
            (values (substring userinfo 0 colon-index) (substring userinfo (+ 1 colon-index) (string-length userinfo)))
            (values userinfo #f)))
      (values #f #f)))

(define (userinfo<-children username password)
  (cond
   ((and username password) (string-append username ":" password))
   (password (validation-error))
   (username username)))

(define relations
  (list (list whole (list scheme specific fragment) whole->children whole<-children)
        (list specific (list authority path query) specific->children specific<-children)
        (list authority (list userinfo host port) authority->children authority<-children)
        (list userinfo (list username password) userinfo->children userinfo<-children)))

(define (get-part uri index)
  (define data (uri-data uri))
  (define computed (uri-computed uri))
  (define v (vector-ref data index))
  (cond
   ((vector-ref computed index)
    (vector-ref data index))
   (else
    ;; operate on a copy, so that if exception
    ;; is raised, the old state is preserved
    (let ((new-data (vector-copy data))
          (new-computed (vector-copy computed)))
      (compute-part! new-data new-computed index)
      (set-uri-data! uri new-data)
      (set-uri-computed! uri new-computed)
      (vector-ref new-data index)))))

(define (compute-part! data computed index)
  (cond
   ((compute-part-from-parent! data computed index))
   ((compute-part-from-children! data computed index))))


;; if needed recursively tries to compute value from parent and mutate it in the passed data vector
;; return #t if succeeded, #f if not
(define (compute-part-from-parent! data computed index)
  (define (get-parent-value! pindex success fail)
    (cond
     ((vector-ref computed pindex) (success (vector-ref data pindex)))
     (else
      (let ((computed? (compute-part-from-parent! data computed pindex)))
        (if computed?
            (success (vector-ref data pindex))
            (fail))))))
  (define (find-parent-relation success fail)
    (define parent (find (lambda (e)
                           (member index (cadr e)))
                         relations))
    (cond
     (parent => success)
     (else (fail))))
  (define (apply-parent->children! relation parent-value)
    (define-values children-vals ((list-ref relation 2) parent-value))
    (for-each
     (lambda (index val)
       (vector-set! computed index #t)
       (vector-set! data index val))
     (list-ref relation 1)
     children-vals))

  (find-parent-relation
   (lambda (parent-relation)
     (get-parent-value! (list-ref parent-relation 0)
                        (lambda (parent-value)
                          (apply-parent->children! parent-relation parent-value)
                          #t)
                        (lambda ()
                          #f)))
   (lambda ()
     #f)))

;; if needed recursively tries to compute value from children and mutate it in the passed data vector
(define (compute-part-from-children! data computed index)
  (define (get-child-value! cindex)
    (cond
     ((vector-ref computed cindex) (vector-ref data cindex))
     (else
      (begin
        (compute-part-from-children! data computed cindex)
        (vector-ref data cindex)))))
  (define (apply-parent<-children! relation)
    (define child-indeces (list-ref relation 1))
    (define child-values (map get-child-value! child-indeces))
    (define all-f (every (lambda (v) (not v)) child-values))
    (define fn (list-ref relation 3))
    (define value (if all-f #f (apply fn child-values)))
    (vector-set! data index value)
    (vector-set! computed index #t))
  (define rel (find (lambda (e) (= index (list-ref e 0))) relations))
  (cond
   (rel (apply-parent<-children! rel))
   (else (begin
      (vector-set! data index #f)
      (vector-set! computed index #t)))))
