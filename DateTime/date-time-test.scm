(import (scheme base)
        (scheme file)
        (date-time)
        (date-time tzfile)
        (date-time leapsecondsfile)
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

(test-group "Test leapsecond file reader"
    (let ((leapseconds (let ((in (open-input-file "testdata/leap-seconds.list")))
                             (dynamic-wind
                               (lambda _ #f)
                               (lambda _ (read-leapseconds-file in))
                               (lambda _ (close-input-port in))))))
      (test-equal 28 (vector-length leapseconds))
      (test-equal 2272060800 (car (vector-ref leapseconds 0)))
      (test-equal 10 (cdr (vector-ref leapseconds 0)))
      (test-equal 3692217600 (car (vector-ref leapseconds 27)))
      (test-equal 37 (cdr (vector-ref leapseconds 27)))))

(test-end)
