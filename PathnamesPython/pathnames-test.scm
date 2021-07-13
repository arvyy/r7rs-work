(cond-expand
  (guile
   (import (scheme base)
           (pathnames)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (pathnames)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (pathnames)
           (srfi 64))))


(test-begin "pathnames")

(test-group
 "parse-posix-pathname"

 (test-equal
     '("" "/" "etc" "passwd")
   (parse-posix-pathname "/etc/passwd"))

 (test-equal
     '("" "" "foo")
   (parse-posix-pathname "foo"))

 (test-equal
     '("" "" "foo" "bar")
   (parse-posix-pathname "foo/./bar"))

 (test-equal
     '("//foo/bar" "/" "baz")
   (parse-posix-pathname "//foo/bar/baz")))

(test-group
 "parse-windows-pathname"

 (test-equal
     '("c:" "/" "Windows")
   (parse-windows-pathname "C:\\Windows"))

 (test-equal
     '("//host/share" "/" "dir" "file")
   (parse-windows-pathname "\\\\host\\share\\dir\\file"))

 (test-equal
     '("" "/" "Windows" "System32" "stop.exe")
   (parse-windows-pathname "\\Windows\\System32\\stop.exe"))

 (test-equal
     '("" "" "foo")
   (parse-windows-pathname "foo"))

 (test-equal
     '("c:" "" "foo")
   (parse-windows-pathname "C:foo"))

 (for-each
  (lambda (illegal-char)
    (call/cc
     (lambda (k)
       (with-exception-handler
           (lambda (err)
             (test-assert (path-error? err))
             (k #t))
         (lambda ()
           (parse-windows-pathname (string-append "C:\\" illegal-char))
           (test-assert #f))))))
  '("<" ">" "\"" ":" "|" "?" "*")))

(test-group
 "posix-pathname"

 (test-equal
     "/etc/passwd"
   (posix-pathname '("" "/" "etc" "passwd")))

 (test-equal
     "foo"
   (posix-pathname '("" "" "foo")))

 (test-equal
     "//foo/bar/baz"
   (posix-pathname '("//foo/bar" "/" "baz") (lambda (drive) drive)))

 (call/cc
  (lambda (k)
    (with-exception-handler
        (lambda (err)
          (test-assert (path-error? err))
          (k #t))
      (lambda ()
        (posix-pathname '("" "/" "foo/bar"))
        (test-assert #f)))))

 (test-equal
     "/mnt/c/foo"
   (posix-pathname '("c:" "/" "foo") (lambda (drive)
                                       (if (equal? "c:" drive)
                                           "/mnt/c"
                                           (test-assert #f))))))

(test-group
 "windows-pathname"

 (test-equal
     "c:\\Windows"
   (windows-pathname '("c:" "/" "Windows")))

 (test-equal
     "\\\\host\\share\\dir\\file"
   (windows-pathname '("//host/share" "/" "dir" "file")))

 (test-equal
     "\\Windows\\System32\\stop.exe"
   (windows-pathname '("" "/" "Windows" "System32" "stop.exe")))

 (test-equal
     "foo"
   (windows-pathname '("" "" "foo")))

 (test-equal
     "c:foo"
   (windows-pathname '("c:" "" "foo")))

 (call/cc
  (lambda (k)
    (with-exception-handler
        (lambda (err)
          (test-assert (path-error? err))
          (k #t))
      (lambda ()
        (windows-pathname '("" "/" "foo\\bar"))
        (test-assert #f))))))

(test-group
 "pathname"

 (define expected
   (cond-expand
     (windows "\\dir\\file")
     ((not windows) "/dir/file")))
 (test-equal
     expected
   (pathname '("" "/" "dir" "file"))))

(test-group
 "path->file-uri"

 (test-equal
     "file:///foo"
   (path->file-uri '("" "/" "foo")))

 (test-equal
     "file://foo/bar/baz"
   (path->file-uri '("//foo/bar" "/" "baz")))

 (call/cc
  (lambda (k)
    (with-exception-handler
        (lambda (err)
          (test-assert (path-error? err))
          (k #t))
      (lambda ()
        (path->file-uri '("" "" "foo")))))))

(test-group
 "path-reserved?"

 (define reserved
   '("con"
     "CON"
     "prn.foo"
     "aux"
     "nul.foo"
     "com0"
     "CoM3"
     "lpt9"
     "foo."
     "FOO."))

 (define not-reserved
   '("foo"
     "nulfoo"
     "foo.bar"))

 (for-each
  (lambda (name)
    (test-assert (path-reserved? `("" "/" ,name))))
  reserved)

 (for-each
  (lambda (name)
    (test-assert (not (path-reserved? `("" "/" ,name)))))
  not-reserved))

(test-group
 "path-absolute-posix?"

 (test-assert (path-absolute-posix? '("//foo/bar" "/" "baz")))
 (test-assert (path-absolute-posix? '("" "/" "baz")))
 (test-assert (not (path-absolute-posix? '("" "" "baz")))))

(test-group
 "path-absolute-windows?"

 (test-assert (path-absolute-windows? '("//foo/bar" "/" "baz")))
 (test-assert (not (path-absolute-windows? '("" "/" "baz"))))
 (test-assert (not (path-absolute-windows? '("" "" "baz")))))

(test-group
 "path-portable?"

 (test-assert (path-portable? '("" "" "foo-1" "filename.ext")))
 (test-assert (path-portable? '("" "" "foo-1" "filename")))
 (test-assert (not (path-portable? '("" "" "con" "filename.ext"))))
 (test-assert (not (path-portable? '("" "/" "foo" "filename.ext"))))
 (test-assert (not (path-portable? '("c:" "" "foo" "filename.ext"))))
 (test-assert (not (path-portable? '("" "" "foo_" "filename.ext"))))
 (test-assert (not (path-portable? '("" "" "" "filename.ext"))))
 (test-assert (not (path-portable? '("" "" "foo" "filenametoolong.ext"))))
 (test-assert (not (path-portable? '("" "" "foo" "filenametoolong"))))
 (test-assert (not (path-portable? '("" "" "foo" "filename.exttoolong"))))
 (test-assert (not (path-portable? '("" "" "foo" "file.ext.ext"))))
 (test-assert (not (path-portable? '("" "" "FOO" "file.ext.ext")))))

(test-group
 "path-parent"

 (test-equal '("" "/" "foo" "bar")
   (path-parent '("" "/" "foo" "bar" "baz")))

 (test-equal '("" "/")
   (path-parent '("" "/"))))

(test-group
 "path-filename"

 (test-equal #f
   (path-filename '("" "/")))

 (test-equal "bar"
   (path-filename '("" "/" "foo" "bar"))))

(test-group
 "path-match"

 (test-assert
     (path-match '("" "" "foo" "bar")
                 '("" "" "foo" "bar")))

 (test-assert
     (path-match '("" "" "foo" "bar")
                 '("" "" "bar")))

 (test-assert
     (path-match '("" "/" "foo" "bar")
                 '("" "/" "foo" "bar")))

 (test-assert
     (not (path-match '("" "/" "foo" "bar")
                      '("" "/" "bar"))))

 (test-assert
     (path-match '("" "" "foo" "bar")
                 '("" "" "fo?" "b??")))

 (test-assert
     (not (path-match '("" "" "foo" "bar")
                      '("" "" "f?" "bar"))))

 (test-assert
     (path-match '("" "" "foo" "bar")
                 '("" "" "foo" "b[ar][ar]")))

 (test-assert
     (path-match '("" "" "foo" "bar")
                 '("" "" "foo" "b*r")))

 (test-assert
     (path-match '("" "" "foo" "bar")
                 '("" "" "foo" "b*")))

 (test-assert
     (path-match '("" "" "foo" "bar")
                 '("" "" "foo" "*r")))

 (test-assert
     (path-match '("" "" "foo" "bar")
                 '("" "" "foo" "*")))

 (test-assert
     (path-match '("" "" "foo" "bar")
                 '("" "" "foo" "**")))

 (test-assert
     (path-match '("" "" "foo" "baaaaaabb")
                 '("" "" "foo" "b*b*b"))))

(test-group
 "path-relative-to"

 (test-equal '("" "" "bar" "baz")
   (path-relative-to '("" "" "foo" "bar" "baz")
                     '("" "" "foo")))

 (test-equal '("" "" "bar" "baz")
   (path-relative-to '("" "/" "foo" "bar" "baz")
                     '("" "/" "foo")))

 (test-equal '("" "" "bar" "baz")
   (path-relative-to '("c:" "/" "foo" "bar" "baz")
                     '("c:" "/" "foo")))

 (test-equal #f
   (path-relative-to '("" "/" "foo" "bar" "baz")
                     '("c:" "/" "foo")))

 (test-equal #f
   (path-relative-to '("" "/" "foo" "bar" "baz")
                     '("" "" "foo"))))

(test-group
 "path-suffix"

 (test-equal "exe"
   (path-suffix '("c:" "/" "Windows" "file.exe")))

 (test-equal #f
   (path-suffix '("c:" "/")))

 (test-equal #f
   (path-suffix '("c:" "/" "file")))

 (test-equal #f
   (path-suffix '("c:" "/" ".file"))))

(test-group
 "path-with-suffix"

 (test-equal '("c:" "/" "Windows" "file.exe2")
   (path-with-suffix '("c:" "/" "Windows" "file.exe") "exe2"))

 (test-equal #f
   (path-with-suffix '("c:" "/") "exe2"))

 (test-equal '("c:" "/" "file.exe2")
   (path-with-suffix '("c:" "/" "file") "exe2"))

 (test-equal '("c:" "/" ".file.exe2")
   (path-with-suffix '("c:" "/" ".file") "exe2")))

(test-group
 "path-without-suffix"

 (test-equal '("c:" "/" "Windows" "file")
   (path-without-suffix '("c:" "/" "Windows" "file.exe")))

 (test-equal '("c:" "/")
   (path-without-suffix '("c:" "/")))

 (test-equal '("c:" "/" "file")
   (path-without-suffix '("c:" "/" "file")))

 (test-equal '("c:" "/" ".file")
   (path-without-suffix '("c:" "/" ".file"))))

(test-group
 "path-join"

 (test-equal '("c:" "/" "bar")
   (path-join '("d:" "/" "baz") '("c:" "/" "bar")))

 (test-equal '("c:" "/" "bar")
   (path-join '("c:" "/" "baz") '("" "/" "bar")))

 (test-equal '("c:" "/" "foo" "bar")
   (path-join '("c:" "/" "foo") '("" "" "bar")))

 (test-equal '("c:" "/" "foo" "bar" "baz")
   (path-join '("c:" "/" "foo")
              '("" "" "bar")
              '("" "" "baz"))))

(test-group
 "path-with-filename"

 (test-equal '("" "/" "bar")
   (path-with-filename '("" "/" "foo") "bar"))

 (call/cc
  (lambda (k)
    (with-exception-handler
        (lambda (err)
          (test-assert (path-error? err))
          (k #t))
      (lambda ()
        (path-with-filename '("" "/") "bar")
        (test-assert #f))))))

(test-group
 "path-normalize"

 (test-equal '("" "" "foo" "bar")
   (path-normalize '("" "" "foo" "bar")))

 (test-equal '("" "" ".." "..")
   (path-normalize '("" "" ".." "..")))

 (test-equal '("" "" "foo" "bar")
   (path-normalize '("" "" "foo" "bar" ".." ".." "foo" "bar")))

 (test-equal '("" "" ".." "foo")
   (path-normalize '("" "" "foo" "bar" ".." ".." ".." "foo"))))

(test-end)
