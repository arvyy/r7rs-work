(define-record-type <path-error>
  (path-error message irritants)
  path-error?
  (message path-error-message)
  (irritants path-error-irritants))

(define (perror message . objs)
  (raise (path-error message objs)))

;; returns list, where each element is an integer (count of separator instances) or a string
;; raise error on empty input
(define (pathname-split-by separator pathname)
  (define chars (string->list pathname))
  (when (null? chars)
    (perror "Empty path"))
  (let loop ((chars chars)
             (res/rev '())
             (current-separator-count 0)
             (current-part-chars/rev '()))
    (if (null? chars)
        (let ((res-final/rev (cond
                              ((< 0 current-separator-count)
                               (cons current-separator-count res/rev))
                              ((not (null? current-part-chars/rev))
                               (cons (list->string (reverse current-part-chars/rev)) res/rev))
                              (else res/rev))))
          (reverse res-final/rev))
        (let* ((char (car chars))
               (sep? (equal? char separator))
               (new-current-sep-count (if sep?
                                          (+ 1 current-separator-count)
                                          0))
               (new-current-part-chars/rev (if sep?
                                               '()
                                               (cons char current-part-chars/rev)))
               (new-res/rev (cond
                             ((and (< 0 current-separator-count)
                                   (= 0 new-current-sep-count))
                              (cons current-separator-count res/rev))
                             ((and (not (null? current-part-chars/rev))
                                   (null? new-current-part-chars/rev))
                              (cons (list->string (reverse current-part-chars/rev)) res/rev))
                             (else res/rev))))
          (loop (cdr chars)
                new-res/rev
                new-current-sep-count
                new-current-part-chars/rev)))))

(define (windows-drive-start? str)
  (and (string? str)
       (>= (string-length str) 2)
       (char-ci<=? #\a (string-ref str 0) #\z)
       (char=? #\: (string-ref str 1))))

(define (parse-windows-drive lst)
  (define drive+cmp? (car lst))
  (define drive (string-downcase (substring drive+cmp? 0 2)))
  (define after-drive-component (substring drive+cmp? 2 (string-length drive+cmp?)))
  (define rest
    (if (equal? "" after-drive-component)
        (cdr lst)
        (cons after-drive-component (cdr lst))))
  (values drive rest))

(define (parse-pathname posix str)
  (define separator (if posix #\/ #\\))
  (let*-values
      (((lst) (pathname-split-by separator str))
       ((drive lst)
        (cond
         ((and (equal? 2 (car lst))
               (> (length lst) 5))
          (values (string-append "//" (list-ref lst 1) "/" (list-ref lst 3))
                  (list-tail lst 4)))
         ((and (not posix)
               (windows-drive-start? (car lst)))
          (parse-windows-drive lst))
         (else (values ""
                       lst))))
       ((root lst)
        (if (number? (car lst))
            (values "/" (cdr lst))
            (values "" lst)))
       ((parts)
        (filter
         (lambda (el)
           (not (or (number? el)
                    (equal? el "."))))
         lst)))
    (append (list drive root) parts)))

(define (parse-posix-pathname str)
  (parse-pathname #t str))

(define (has-invalid-windows-char? str)
  (call/cc
   (lambda (k)
     (string-for-each
      (lambda (char)
        (case char
          ((#\< #\> #\" #\: #\| #\? #\*) (k char))))
      str)
     #f)))

(define (parse-windows-pathname str)
  (define path (parse-pathname #f str))
  (for-each
   (lambda (component)
     (cond
      ((has-invalid-windows-char? component) =>
       (lambda (char)
         (perror "Path contains invalid character" (string char))))))
   (cddr path))
  path)

(define (validate-components-have-no-separator path separator)
  (define sep-char (string-ref separator 0))
  (for-each
   (lambda (cmp)
     (string-for-each
      (lambda (char)
        (when (equal? char sep-char)
          (perror "Path component contains separator" path cmp separator)))
      cmp))
   (cddr path)))

(define (components-join path separator)
  (define lst (cddr path))
  (validate-components-have-no-separator path separator)
  (if (null? lst)
      ""
      (let loop ((lst (cdr lst))
                 (str (car lst)))
        (if (null? lst)
            str
            (loop (cdr lst)
                  (string-append str
                                 separator
                                 (car lst)))))))

(define posix-pathname
  (case-lambda
    ((path) (posix-pathname path (lambda (drive) (perror "No drive mapper given to map" drive))))
    ((path drive-mapper)
     (define drive (if (equal? "" (car path))
                       ""
                       (drive-mapper (car path))))
     (define root (cadr path))
     (define components (components-join path "/"))
     (string-append drive root components))))

(define (windows-pathname path)
  (define (convert-slash char)
    (if (char=? char #\/)
        #\\
        char))
  (define drive
    (string-map
     convert-slash
     (car path)))
  (define root
    (string-map
     convert-slash
     (cadr path)))
  (string-append drive root (components-join path "\\")))

(define (pathname path)
  (define windows?
    (find
     (lambda (feature)
       (equal? feature 'windows))
     (features)))
  (define proc (if windows?
                   windows-pathname
                   posix-pathname))
  (proc path))

(define (path->file-uri path)
  (define drive (car path))
  (define drive-defined? (not (equal? "" drive)))
  (define unc?
    (and (> (string-length drive) 2)
         (equal? "//" (substring drive 0 2))))
  (when (equal? "" (cadr path))
    (perror "Cannot convert not-absolute path to file uri" path))
  (cond
   (unc? (string-append "file:" (car path) "/" (components-join path "/")))
   (drive-defined? (string-append "file:///" (car path) "/" (components-join path "/")))
   (else (string-append "file:///" (components-join path "/")))))

(define reserved-names
  (append
   (list "con" "prn" "aux" "nul")
   (map
    (lambda (i)
      (string-append "com" (number->string i)))
    (iota 10))
   (map
    (lambda (i)
      (string-append "lpt" (number->string i)))
    (iota 10))))

(define (reserved-component? str)
  (define l (string-length str))
  (define str-lowercase (string-downcase str))
  (call/cc
   (lambda (k)
     (when (has-invalid-windows-char? str-lowercase)
       (k #t))
     (when (equal? #\. (string-ref str (- l 1)))
       (k #t))
     (for-each
      (lambda (name)
        (define name-l (string-length name))
        (when (equal? name str-lowercase)
          (k #t))
        (when (and
               (>= l (+ 1 name-l))
               (equal? (string-append name ".")
                       (substring str-lowercase 0 (+ 1 name-l))))
          (k #t)))
      reserved-names)
     #f)))

(define (path-reserved? path)
  (fold (lambda (el acc)
          (or acc
              (and
               (not (equal? "" el))
               (reserved-component? el))))
        #f
        path))

(define (path-absolute-posix? path)
  (not (equal? "" (cadr path))))

(define (path-absolute-windows? path)
  (and (not (equal? "" (car path)))
       (not (equal? "" (cadr path)))))

(define (portable-letter? char)
  (or (char<=? #\a char #\z)
      (char-numeric? char)
      (char=? #\- char)))

(define (index-of str char)
  (define l (string-length str))
  (fold
   (lambda (el index acc)
     (if (char=? char el)
         index
         acc))
   #f
   (string->list str)
   (iota l)))

(define (portable-name str max-length)
  (and (<= 1 (string-length str) max-length)
       (fold
        (lambda (el acc)
          (and acc (portable-letter? el)))
        #t
        (string->list str))))

(define (portable-filename str)
  (define l (string-length str))
  (define period-index (index-of str #\.))
  (if period-index
      (and (portable-name (substring str 0 period-index) 8)
           (> l (+ 1 period-index))
           (portable-name (substring str (+ 1 period-index) l) 3))
      (portable-name str 8)))

(define (portable-foldername str)
  (portable-name str 8))

(define (portable-components components)
  (cond
   ((null? components) #t)
   ((null? (cdr components)) (portable-filename (car components)))
   (else (and (portable-foldername (car components))
              (portable-components (cdr components))))))

(define (path-portable? path)
  (and (not (path-reserved? path))
       (equal? "" (car path))
       (equal? "" (cadr path))
       (portable-components (cddr path))))

(define (path-parent path)
  (if (null? (cddr path))
      path
      (append (list (car path)
                    (cadr path))
              (drop-right (cddr path) 1))))

(define (path-filename path)
  (if (null? (cddr path))
      #f
      (car (take-right (cddr path) 1))))

(define (path-relative-to path1 path2)
  (define same-root
    (and (equal? (car path1) (car path2))
         (equal? (cadr path1) (cadr path2))))
  (if (not same-root)
      #f
      (let loop ((cmps1 (cddr path1))
                 (cmps2 (cddr path2)))
        (cond
         ((null? cmps1) #f)
         ((null? cmps2)
          (append (list "" "") cmps1))
         ((equal? (car cmps1) (car cmps2))
          (loop (cdr cmps1) (cdr cmps2)))
         (else #f)))))

(define (name+suffix path callback)
  (let* ((cmps (cddr path))
         (last (and (not (null? cmps))
                    (car (take-right cmps 1))))
         (period-index (and last
                            (index-of last #\.)))
         (period-index (and period-index
                            (> period-index 0)
                            period-index)))
    (if period-index
        (callback (substring last 0 period-index)
                  (substring last (+ 1 period-index) (string-length last)))
        (callback last #f))))

(define (path-suffix path)
  (name+suffix path (lambda (name suffix)
                      suffix)))

(define (path-with-suffix path new-suffix)
  (name+suffix path (lambda (name suffix)
                      (if name
                          (append (drop-right path 1)
                                  (list (string-append name "." new-suffix)))
                          #f))))

(define (path-without-suffix path)
  (name+suffix path (lambda (name suffix)
                      (if suffix
                          (append (drop-right path 1) (list name))
                          path))))

(define (path-join basepath path1 . paths)
  (let loop ((basepath basepath)
             (paths (cons path1 paths)))
    (cond
     ((null? paths) basepath)
     (else (let ((p (car paths))
                 (rest (cdr paths)))
             (cond
              ((not (equal? "" (car p))) (loop p rest))
              ((not (equal? "" (cadr p))) (loop (append (list (car basepath)) (cdr p))
                                                rest))
              (else (loop (append basepath (cddr p))
                          rest))))))))

(define (path-with-filename path filename)
  (when (null? (cddr path))
    (perror "No file component" path))
  (append (drop-right path 1) (list filename)))

(define (path-normalize path)
  (let loop ((cmps (cddr path))
             (res/rev '()))
    (cond
     ((null? cmps) (append (take path 2) (reverse res/rev)))
     (else (let ((cmp (car cmps)))
             (cond
              ((and (equal? ".." cmp)
                    (not (null? res/rev))
                    (not (equal? ".." (car res/rev))))
               (loop (cdr cmps)
                     (cdr res/rev)))
              (else
               (loop (cdr cmps)
                     (cons cmp res/rev)))))))))
