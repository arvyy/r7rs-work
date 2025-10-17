(import (scheme base)
        (scheme file)
        (date-time)
        (date-time tzfile)
        (date-time leapsecondsfile)
        (srfi 64)
        (gauche base) ;; for debugging, TODO remove
        )


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

(test-group "Date constructor, getters"
    (test-assert (date? (make-date 2020 1 1)))
    (test-assert (date? (make-date 2020 2 29)))
    (test-assert (date? (make-date 2000 2 29)))
    (test-error (make-date 2020 1 32))
    (test-error (make-date 2020 1 -1))
    (test-error (make-date 2020 0 1))
    (test-error (make-date 2020 13 1))
    (test-error (make-date 2020.1 1 1))
    (test-error (make-date 2020 1.1 1))
    (test-error (make-date 2020 1 1.1))
    (test-error (make-date 1900 2 29))
    (test-equal 2000 (date-year (make-date 2000 1 2)))
    (test-equal 1 (date-month (make-date 2000 1 2)))
    (test-equal 2 (date-day (make-date 2000 1 2)))
    (let-values (((y m d) (date-ymd (make-date 2000 1 2))))
      (test-equal 2000 y)
      (test-equal 1 m)
      (test-equal 2 d)))

(test-group "Date->ISO8601"
    (test-equal "2020-01-01" (date->iso8601 (make-date 2020 1 1)))
    (test-equal "2020-11-12" (date->iso8601 (make-date 2020 11 12)))
    (test-equal "0000-01-01" (date->iso8601 (make-date 0 1 1)))
    (test-equal "-0001-01-01" (date->iso8601 (make-date -1 1 1)))
    (test-equal "99999-01-01" (date->iso8601 (make-date 99999 1 1))))

(test-group "Week & weekday calculation"
    (test-equal 0 (date-weekday (make-date 2025 10 12)))
    (test-equal 1 (date-weekday (make-date 2025 10 13)))
    (test-equal 7 (date-iso-weekday (make-date 2025 10 12)))
    (test-equal 1 (date-iso-weekday (make-date 2025 10 13)))

    (test-equal 42 (date-iso-week (make-date 2025 10 14)))
    (test-equal 2025 (date-iso-week-year (make-date 2025 10 14)))

    (test-equal 1 (date-iso-week (make-date 2008 12 29)))
    (test-equal 2009 (date-iso-week-year (make-date 2008 12 29)))

    (test-equal 53 (date-iso-week (make-date 2010 1 2)))
    (test-equal 2009 (date-iso-week-year (make-date 2010 1 2))))

(test-group "Rata Die"
    (test-equal 1 (date->rata-die (make-date 1 1 1)))
    (test-equal 739539 (date->rata-die (make-date 2025 10 15)))
    (test-equal -1 (date->rata-die (make-date 0 12 30)))
    (test-equal -365 (date->rata-die (make-date 0 1 1)))
    (test-equal -366 (date->rata-die (make-date -1 12 31)))

    (test-assert (date=? (make-date 1 1 1) (rata-die->date 1)))
    (test-assert (date=? (make-date 2025 10 15) (rata-die->date 739539)))
    (test-assert (date=? (make-date 0 12 30) (rata-die->date -1)))
    (test-assert (date=? (make-date 0 1 1) (rata-die->date -365)))
    (test-assert (date=? (make-date -1 12 31) (rata-die->date -366))))

(test-group "Date comparators"
    (test-assert (not (date=? (make-date 2021 1 1) (make-date 2020 1 1))))
    (test-assert (not (date=? (make-date 2020 2 1) (make-date 2020 1 1))))
    (test-assert (not (date=? (make-date 2020 1 2) (make-date 2020 1 1))))
    (test-assert (date=? (make-date 2020 1 1) (make-date 2020 1 1)))

    (test-assert (date<? (make-date 2020 2 2) (make-date 2021 1 1)))
    (test-assert (date<? (make-date 2020 2 2) (make-date 2020 3 1)))
    (test-assert (date<? (make-date 2020 2 2) (make-date 2020 2 3)))
    (test-assert (not (date<? (make-date 2020 2 2) (make-date 2020 2 2))))

    (test-assert (date<=? (make-date 2020 2 2) (make-date 2021 1 1)))
    (test-assert (date<=? (make-date 2020 2 2) (make-date 2020 3 1)))
    (test-assert (date<=? (make-date 2020 2 2) (make-date 2020 2 3)))
    (test-assert (date<=? (make-date 2020 2 2) (make-date 2020 2 2)))

    (test-assert (date>? (make-date 2021 1 1) (make-date 2020 2 2)))
    (test-assert (date>? (make-date 2020 3 1) (make-date 2020 2 2)))
    (test-assert (date>? (make-date 2020 2 3) (make-date 2020 2 2)))
    (test-assert (not (date>? (make-date 2020 2 2) (make-date 2020 2 2))))

    (test-assert (date>=? (make-date 2021 1 1) (make-date 2020 2 2)))
    (test-assert (date>=? (make-date 2020 3 1) (make-date 2020 2 2)))
    (test-assert (date>=? (make-date 2020 2 3) (make-date 2020 2 2)))
    (test-assert (date>=? (make-date 2020 2 2) (make-date 2020 2 2))))

(test-end)
