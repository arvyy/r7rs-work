(define whole 0)
(define scheme 1)
(define specific 2)
(define fragment 3)
(define authority 4)
(define path 5)
(define query 6)
(define userinfo 7)
(define host 8)
(define port 9)
(define username 10)
(define password 11)

(define (key->index key)
  (case key
    ((whole) whole)
    ((scheme) scheme)
    ((specific) specific)
    ((fragment) fragment)
    ((authority) authority)
    ((path) path)
    ((query) query)
    ((userinfo) userinfo)
    ((host) host)
    ((port) port)
    ((username) username)
    ((password) password)
    (else #f)))

(define (match-part input-string start-index pattern)
  (cond
   ((>= start-index (string-length input-string))
    #f)
   ((pattern input-string start-index) => values)
   (else #f)))

(define (or-pattern . patterns)
  (lambda (str index)
    (let loop ((patterns patterns))
      (cond
       ((null? patterns) #f)
       (else (let ((p (car patterns)))
               (cond
                ((p str index) => values)
                (else (loop (cdr patterns))))))))))

(define (seq-pattern . patterns)
  (lambda (str start-index)
    (let loop ((index start-index)
               (patterns patterns))
      (cond
       ((null? patterns) index)
       (else (let ((p (car patterns)))
               (cond
                ((>= index (string-length str)) #f)
                ((p str index) => (lambda (new-index)
                                    (loop new-index (cdr patterns))))
                (else #f))))))))

(define (repeat-pattern pattern)
  (lambda (str index)
    (let loop ((index index))
      (cond
       ((>= index (string-length str)) index)
       ((pattern str index) => (lambda (new-index)
                                 (loop new-index)))
       (else index)))))

(define (char-predicate-pattern predicate)
  (lambda (str index)
    (if (predicate (string-ref str index))
        (+ 1 index)
        #f)))

(define (char-pattern char)
  (lambda (str index)
    (if (char=? char (string-ref str index))
        (+ 1 index)
        #f)))

(define (char-range-pattern char1 char2)
  (let ((n1 (char->integer char1))
        (n2 (char->integer char2)))
    (lambda (str index)
      (if (<= n1 (char->integer (string-ref str index)) n2)
          (+ 1 index)
          #f))))

(define %-encoding-pattern
  (let* ((ranges (vector #\A #\F #\a #\f #\0 #\9))
         (ranges (vector-map char->integer ranges)))
    (define (hex? char)
      (define codepoint (char->integer char))
      (or (<= (vector-ref ranges 0) codepoint (vector-ref ranges 1))
          (<= (vector-ref ranges 2) codepoint (vector-ref ranges 3))
          (<= (vector-ref ranges 4) codepoint (vector-ref ranges 5))))
    (lambda (str index)
      (cond
       ((>= (+ index 2) (string-length str))
        #f)
       ((and (char=? #\% (string-ref str index))
             (hex? (string-ref str (+ 1 index)))
             (hex? (string-ref str (+ 1 index))))
        (+ 3 index))
       (else #f)))))

(define gendelim-pattern
  (let ((chars '(#\: #\/ #\? #\# #\[ #\] #\@)))
    (lambda (str index)
      (if (member (string-ref str index) chars)
          (+ 1 index)
          #f))))

(define subdelim-pattern
  (let ((chars '(#\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=)))
    (lambda (str index)
      (if (member (string-ref str index) chars)
          (+ 1 index)
          #f))))

(define unreserved-pattern
  (let ((chars '(#\- #\. #\_ #\~)))
    (lambda (str index)
      (define c (string-ref str index))
      (if (or (char-alphabetic? c)
              (char-numeric? c)
              (member c chars))
          (+ 1 index)
          #f))))

(define parse-scheme
  (let* ((allowed-char? (lambda (c)
                         (or (char-alphabetic? c) (char-numeric? c) (char=? c #\+) (char=? c #\-) (char=? c #\.))))
         (pattern (seq-pattern
                   (char-predicate-pattern char-alphabetic?)
                   (repeat-pattern
                    (char-predicate-pattern allowed-char?))
                   (char-pattern #\:))))
    (lambda (input-string start-index)
      (define match (match-part input-string start-index pattern))
      (if match
          (values (substring input-string start-index (- match 1)) match)
          (values #f start-index)))))

(define parse-userinfo
  (let ((pattern (seq-pattern
                  (repeat-pattern
                   (or-pattern unreserved-pattern
                               %-encoding-pattern
                               subdelim-pattern))
                  (char-pattern #\@))))
    (lambda (input-string start-index)
      (define match (match-part input-string start-index pattern))
      (if match
          (values (substring input-string start-index (- match 1)) match)
          (values #f start-index)))))

(define ipv4-pattern
  (let* ((digit (char-predicate-pattern char-numeric?))
         (period (char-pattern #\.))
         (dec-octet (or-pattern
                     (seq-pattern (char-pattern #\2)
                                  (char-pattern #\5)
                                  (char-range-pattern #\0 #\5))
                     (seq-pattern (char-pattern #\2)
                                  (char-range-pattern #\0 #\4)
                                  digit)
                     (seq-pattern (char-pattern #\1)
                                  digit
                                  digit)
                     (seq-pattern (char-range-pattern #\1 #\9)
                                  digit)
                     digit)))
    (seq-pattern
     dec-octet period
     dec-octet period
     dec-octet period
     dec-octet)))


(define ipv6-pattern
  (seq-pattern
   (char-pattern #\[)
   (repeat-pattern
    (or-pattern
     unreserved-pattern
     (char-pattern #\:)))
   (char-pattern #\])))

(define parse-host
  (let ((pattern (or-pattern
                  ipv4-pattern
                  ipv6-pattern
                  (repeat-pattern
                   (or-pattern
                    unreserved-pattern
                    %-encoding-pattern
                    subdelim-pattern)))))
    (lambda (input-string start-index)
      (define match (match-part input-string start-index pattern))
      (if match
          (values (substring input-string start-index match) match)
          (values #f start-index)))))

(define parse-port
  (let ((pattern (seq-pattern
                  (char-pattern #\:)
                  (repeat-pattern
                   (char-predicate-pattern char-numeric?)))))
    (lambda (input-string start-index)
      (define match (match-part input-string start-index pattern))
      (if match
          (values (string->number (substring input-string (+ 1 start-index) match)) match)
          (values #f start-index)))))

(define pchar-pattern
  (or-pattern
   unreserved-pattern
   %-encoding-pattern
   subdelim-pattern
   (char-pattern #\:)
   (char-pattern #\@)))

(define segment-pattern
  (repeat-pattern pchar-pattern))

(define segment-nz-pattern
  (seq-pattern
   pchar-pattern
   (repeat-pattern pchar-pattern)))

(define segment-nz-nc-pattern
  (let ((pchar-except-colon (or-pattern
                             unreserved-pattern
                             %-encoding-pattern
                             subdelim-pattern
                             (char-pattern #\@))))
    (seq-pattern
     pchar-except-colon
     (repeat-pattern pchar-except-colon))))

(define parse-path/after-authority
  (let ((pattern (repeat-pattern
                  (seq-pattern
                   (char-pattern #\/)
                   segment-pattern))))
    (lambda (input-string start-index)
      (define match (match-part input-string start-index pattern))
      (if match
          (values (substring input-string start-index match) match)
          (values #f start-index)))))

(define path-absolute-pattern
  (or-pattern (seq-pattern (char-pattern #\/)
                           segment-nz-pattern
                           (repeat-pattern
                            (seq-pattern
                             (char-pattern #\/)
                             segment-pattern)))
              (char-pattern #\/)))

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
          (values #f start-index)))))

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
          (values #f start-index)))))

(define parse-query
  (let ((pattern (seq-pattern
                  (char-pattern #\?)
                  (repeat-pattern
                   (or-pattern
                    pchar-pattern
                    (char-pattern #\/)
                    (char-pattern #\?))))))
    (lambda (input-string start-index)
      (define match (match-part input-string start-index pattern))
      (if match
          (values (substring input-string (+ 1 start-index) match) match)
          (values #f start-index)))))

(define parse-fragment
  (let ((pattern (seq-pattern
                  (char-pattern #\#)
                  (repeat-pattern
                   (or-pattern
                    pchar-pattern
                    (char-pattern #\/)
                    (char-pattern #\?))))))
    (lambda (input-string start-index)
      (define match (match-part input-string start-index pattern))
      (if match
          (values (substring input-string (+ 1 start-index) match) match)
          (values #f start-index)))))

(define (parse-authority str start-index)
  (cond
   ((match-part str start-index (seq-pattern (char-pattern #\/) (char-pattern #\/))) =>
    (lambda (index)
      (let*-values (((userinfo index) (parse-userinfo str index))
                    ((host index) (parse-host str index))
                    ((port index) (parse-port str index)))
        (values (substring str start-index index) index))))
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
  (string-append
   (if authority
       (string-append "//" authority)
       "")
   path
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
  (string-append
   (if userinfo
       (string-append userinf "@")
       "")
   host
   (if port
       (string-append ":" (number->string port))
       "")))

(define (index-of str char)
  (define len (string-length str))
  (let loop ((i 0))
    (cond
     ((>= i len) #f)
     ((char=? char (string-ref str i)) i)
     (else (loop (+ 1 i))))))

(define (userinfo->children userinfo)
  (if userinfo
      (let ((colon-index (index-of userinfo #\:)))
        (if colon-index
            (values (substring userinfo 0 colon-index) (substring userinf (+ 1 colon-index) (string-length userinfo)))
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

(define-record-type <uri>
  (make-uri-private data computed)
  uri-object?
  (data uri-data set-uri-data!)
  (computed uri-computed set-uri-computed!))

(define-record-type <parse-error>
  (make-parse-error)
  uri-error?)

(define (validation-error)
  (raise (make-parse-error)))

(define (make-uri-object . args)
  (define data (make-vector 12 #f))
  (define computed (make-vector 12 #f))
  (let loop ((args args))
    (cond
     ((null? args) (make-uri-private data computed))
     ((null? (cdr args)) (validation-error))
     (else (let ((key (car args))
                 (value (cadr args)))
             (unless (symbol? key)
               (validation-error))
             (unless (or (string? value) (not value))
               (validation-error))
             (let ((index (key->index key)))
               (unless key
                 (validation-error))
               (vector-set! data index value)
               (vector-set! computed index #t)
               (loop (cddr args))))))))

(define (make-getter index)
  (lambda (uri)
    (get-part uri index)))

(define uri-whole (make-getter whole))
(define uri-scheme (make-getter scheme))
(define uri-specific (make-getter specific))
(define uri-authority (make-getter authority))
(define uri-userinfo (make-getter userinfo))
(define uri-username (make-getter username))
(define uri-password (make-getter password))
(define uri-host (make-getter host))
(define uri-port (make-getter port))
(define uri-path (make-getter path))
(define uri-query (make-getter query))
(define uri-fragment (make-getter fragment))

(define (decode-% str exception)
  (define len (string-length str))
  (let loop ((chunks '())
             (chunk-start 0)
             (i 0))
    (cond
     ((>= i len)
      (apply string-append (reverse (cons (substring str chunk-start i) chunks))))
     ((char=? #\% (string-ref str i))
      (begin
        (unless (%-encoding-pattern str i)
          (validation-error))
        (let* ((codepoint-str (substring (+ i 1) (+ i 3)))
               (codepoint (string->number codepoint-str 16))
               (char (number->char codepoint)))
          (if (member char exception)
              (loop (append (string-downcase (substring str i (+ i 3))) (substring str chunk-start i) chunks) (+ 3 i) (+ 3 i))
              (loop (append (string char) (substring str chunk-start i) chunks) (+ 3 i) (+ 3 i))))))
     (else (loop chunks chunk-start (+ 1 i))))))

(define (string->uri-object str)
  (make-uri-object 'whole (decode-% str '(#\: #\/ #\? #\# #\[ #\] #\@ #\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=))))

(define (uri-parse-path uri)
  (define path (uri-path uri))
  (define len (string-length path))
  (let loop ((chunks '())
             (chunk-start 0)
             (i 0))
    (cond
     ((>= i len) (reverse chunks))
     ((char=? (string-ref path i) #\/)
      (loop (cons (substring path chunk-start i)
                  chunks)
            (chunk-start (+ 1 i))
            (+ 1 i)))
     (else (loop chunks chunk-start (+ 1 i))))))

(define (string-split str chars)
  (define len (string-length str))
  (let loop ((i 0)
             (chunks '())
             (chunk-start 0))
    (cond
     ((>= i len) (reverse (cons (substring str chunk-start i) chunks)))
     (else (let ((char (string-ref str i)))
             (if (member char chars)
                 (loop (+ 1 i) (cons (substring chunk-start chunk-start i) chunks) (+ 1 i))
                 (loop (+ 1 i) chunks chunk-start)))))))

(define uri-parse-query
  (case-lambda
    ((uri) (uri-parse-query uri #t))
    ((uri plus)
     (define query (uri-query uri))
     (define entries (string-split query '(#\; #\&)))
     (map
      (lambda (e)
        (define equal-index (index-of e #\=))
        (if equal-index
            (cons (string->symbol (substring e 0 equal-index)) (substring e (+ 1 equal-index) (string-length e)))
            (cons (string->symbol e) "")))
      entries))))

(define (uri-merge uri base-uri)
  ;;TODO
  #f)

(define (uri-parse-data uri)
  ;;TODO
  #f)
