(import (scheme base)
        (scheme file)
        (date-time)
        (date-time tzfile)
        (srfi 64))


(test-begin "date-time")

(test-group "Test tzfile reader"
    (let ((tzfile (let ((in (open-binary-input-file "testdata/Vilnius")))
                    (dynamic-wind
                      (lambda _ #f)
                      (lambda _ (read-tz-file in))
                      (lambda _ (close-input-port in))))))
      (test-equal 2 (tzfile-version tzfile))
      (test-assert (vector? (tzfile-time-transitions tzfile)))
      (test-assert (vector? (tzfile-leap-corrections tzfile)))))

(test-end)
