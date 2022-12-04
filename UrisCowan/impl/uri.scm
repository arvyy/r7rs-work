;; general uri stuff that didn't warrant its own file

;; uri is represented as a vector; below are indeces of various components
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
             (unless (or
                      (and (equal? 'port key) (number? value))
                      (string? value)
                      (not value))
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

(define (uri-reference? uri)
  (call/cc (lambda (k)
             (with-exception-handler
                 (lambda (err)
                   (when (uri-error? err)
                     (k #f)))
               (lambda ()
                 (uri-whole uri)
                 #t)))))

(define (uri-absolute? uri)
  (and (uri-reference? uri)
       (uri-scheme uri)
       (not (uri-fragment uri))))

(define (uri-relative-reference? uri)
  (and (uri-reference? uri)
       (not (uri-scheme uri))))


(define (string->uri-object str)
  ;; if uri is a data, do not decode it here but instead do it in uri-parse-data
  (define is-data? (string-starts-with str "data:"))
  (make-uri-object 'whole (if is-data? str (decode-% str '(#\: #\/ #\? #\# #\[ #\] #\@ #\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=)))))

(define (uri-parse-path uri)
  (define path (or (uri-path uri) ""))
  (define chunks (string-split path '(#\/)))
  (define absolute? (and (> (string-length path) 0) (char=? #\/ (string-ref path 0))))
  (cond
   (absolute? (cdr chunks))
   ((equal? "" (car chunks)) chunks)
   (else (cons "" chunks))))

(define uri-parse-query
  (case-lambda
    ((uri) (uri-parse-query uri #t))
    ((uri plus)
     (define query (uri-query uri))
     (cond
      ((not query) '())
      (else (let ((entries (string-split query '(#\; #\&))))
              (map
               (lambda (e)
                 (define equal-index (index-of e #\=))
                 (if equal-index
                     (cons (string->symbol (substring e 0 equal-index)) (substring e (+ 1 equal-index) (string-length e)))
                     (cons (string->symbol e) "")))
               entries)))))))

(define (uri-merge uri base-uri)
  ;;TODO
  #f)
