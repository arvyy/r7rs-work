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
   (windows-pathname '("c:" "" "foo"))))

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

(test-end)
