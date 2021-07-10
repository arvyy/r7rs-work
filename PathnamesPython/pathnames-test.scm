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
   (parse-windows-pathname "C:foo")))

(test-end)
