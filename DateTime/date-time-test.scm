(import (scheme base)
        (scheme file)
        (date-time)
        (date-time tzfile)
        (date-time leapsecondsfile)
        (srfi 64)
        (gauche base) ;; for debugging, TODO remove
        )

;; test helpers
(define (timestamp-equal? t1 t2)
    (and (date=? (timestamp-date t1) (timestamp-date t2))
         (= (timestamp-hour t1) (timestamp-hour t2))
         (= (timestamp-minute t1) (timestamp-minute t2))
         (= (timestamp-second t1) (timestamp-second t2))
         (eq? (timestamp-timezone t1) (timestamp-timezone t2))))

(define (dt-equal? dt1 dt2)
    (and (= (dt-years dt1) (dt-years dt2))
         (= (dt-months dt1) (dt-months dt2))
         (= (dt-weeks dt1) (dt-weeks dt2))
         (= (dt-days dt1) (dt-days dt2))
         (= (dt-hours dt1) (dt-hours dt2))
         (= (dt-minutes dt1) (dt-minutes dt2))
         (= (dt-seconds dt1) (dt-seconds dt2))))

(test-begin "Date-Time")

;;; timezone / leap seconds files

(test-group "Test tzfile reader"
    (let ((tzfile (let ((in (open-binary-input-file "testdata/timezones/Europe/Vilnius")))
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

;;; dates

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

(test-group "Date->ISO-8601"
    (test-equal "2020-01-01" (date->iso-8601 (make-date 2020 1 1)))
    (test-equal "2020-11-12" (date->iso-8601 (make-date 2020 11 12)))
    (test-equal "0000-01-01" (date->iso-8601 (make-date 0 1 1)))
    (test-equal "-0001-01-01" (date->iso-8601 (make-date -1 1 1)))
    (test-equal "99999-01-01" (date->iso-8601 (make-date 99999 1 1))))

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

(test-group "MJD"
    (test-equal 0 (date->mjd (make-date 1858 11 17)))
    (test-equal 60965 (date->mjd (make-date 2025 10 17)))

    (test-assert (date=? (make-date 1858 11 17) (mjd->date 0)))
    (test-assert (date=? (make-date 2025 10 17) (mjd->date 60965))))

(test-group "Date comparators"
    (test-assert (not (date=? (make-date 2021 1 1) (make-date 2020 1 1))))
    (test-assert (not (date=? (make-date 2020 2 1) (make-date 2020 1 1))))
    (test-assert (not (date=? (make-date 2020 1 2) (make-date 2020 1 1))))
    (test-assert (date=? (make-date 2020 1 1) (make-date 2020 1 1)))

    (test-assert (date<? (make-date 2020 2 2) (make-date 2021 1 1)))
    (test-assert (date<? (make-date 2020 2 2) (make-date 2020 3 1)))
    (test-assert (date<? (make-date 2020 2 2) (make-date 2020 2 3)))
    (test-assert (not (date<? (make-date 2020 2 2) (make-date 2020 2 2))))
    (test-assert (not (date<? (make-date 2021 1 1) (make-date 2020 2 2))))
    (test-assert (not (date<? (make-date 2020 3 1) (make-date 2020 2 2))))
    (test-assert (not (date<? (make-date 2020 2 3) (make-date 2020 2 2))))

    (test-assert (date<=? (make-date 2020 2 2) (make-date 2021 1 1)))
    (test-assert (date<=? (make-date 2020 2 2) (make-date 2020 3 1)))
    (test-assert (date<=? (make-date 2020 2 2) (make-date 2020 2 3)))
    (test-assert (date<=? (make-date 2020 2 2) (make-date 2020 2 2)))
    (test-assert (not (date<=? (make-date 2021 1 1) (make-date 2020 2 2))))
    (test-assert (not (date<=? (make-date 2020 3 1) (make-date 2020 2 2))))
    (test-assert (not (date<=? (make-date 2020 2 3) (make-date 2020 2 2))))

    (test-assert (date>? (make-date 2021 1 1) (make-date 2020 2 2)))
    (test-assert (date>? (make-date 2020 3 1) (make-date 2020 2 2)))
    (test-assert (date>? (make-date 2020 2 3) (make-date 2020 2 2)))
    (test-assert (not (date>? (make-date 2020 2 2) (make-date 2020 2 2))))
    (test-assert (not (date>? (make-date 2020 2 2) (make-date 2021 1 1))))
    (test-assert (not (date>? (make-date 2020 2 2) (make-date 2020 3 1))))
    (test-assert (not (date>? (make-date 2020 2 2) (make-date 2020 2 3))))

    (test-assert (date>=? (make-date 2021 1 1) (make-date 2020 2 2)))
    (test-assert (date>=? (make-date 2020 3 1) (make-date 2020 2 2)))
    (test-assert (date>=? (make-date 2020 2 3) (make-date 2020 2 2)))
    (test-assert (date>=? (make-date 2020 2 2) (make-date 2020 2 2)))
    (test-assert (not (date>=? (make-date 2020 2 2) (make-date 2021 1 1))))
    (test-assert (not (date>=? (make-date 2020 2 2) (make-date 2020 3 1))))
    (test-assert (not (date>=? (make-date 2020 2 2) (make-date 2020 2 3)))))

;; clock-time

(test-group "Clock time"
    (test-assert (clock-time? (make-clock-time 23 0 11/10)))
    (test-error (make-clock-time 24 1 1))
    (test-error (make-clock-time -1 1 1))
    (test-error (make-clock-time 23 100 1))
    (test-error (make-clock-time 23 -1 1))
    (test-error (make-clock-time 23 1 -1))
    (test-error (make-clock-time 23 1 1+1i))

    (test-equal 23 (clock-time-hour (make-clock-time 23 1 2)))
    (test-equal 1 (clock-time-minute (make-clock-time 23 1 2)))
    (test-equal 2 (clock-time-second (make-clock-time 23 1 2)))

    (let-values (((h m s) (clock-time-hms (make-clock-time 23 1 2))))
      (test-equal 23 h)
      (test-equal 1 m)
      (test-equal 2 s)))

;; moment

(test-group "Moment constructors, getters"
    (test-assert (moment? (make-moment (make-date 2025 1 1) 100)))
    (test-error (make-moment #f 100))
    (test-error (make-moment (make-date 2025 1 1) -1))
    (test-error (make-moment (make-date 2025 1 1) 86401))

    (test-assert (date=? (make-date 2025 1 1) (moment-date (make-moment (make-date 2025 1 1) 1))))
    (test-equal 1 (moment-second-of-day (make-moment (make-date 2025 1 1) 1))))

(test-group "Moment comparators"
    (test-assert (not (moment=? (make-moment (make-date 2024 1 1) 1) (make-moment (make-date 2025 1 1) 1))))
    (test-assert (not (moment=? (make-moment (make-date 2025 1 1) 2) (make-moment (make-date 2025 1 1) 1))))
    (test-assert (moment=? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 1)))

    (test-assert (moment<? (make-moment (make-date 2024 1 1) 2) (make-moment (make-date 2025 1 1) 1)))
    (test-assert (moment<? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 2)))
    (test-assert (not (moment<? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 1))))
    (test-assert (not (moment<? (make-moment (make-date 2026 1 1) 1) (make-moment (make-date 2025 1 1) 1))))
    (test-assert (not (moment<? (make-moment (make-date 2025 1 1) 2) (make-moment (make-date 2025 1 1) 1))))

    (test-assert (moment<=? (make-moment (make-date 2024 1 1) 2) (make-moment (make-date 2025 1 1) 1)))
    (test-assert (moment<=? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 2)))
    (test-assert (moment<=? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 1)))
    (test-assert (not (moment<=? (make-moment (make-date 2026 1 1) 1) (make-moment (make-date 2025 1 1) 1))))
    (test-assert (not (moment<=? (make-moment (make-date 2025 1 1) 2) (make-moment (make-date 2025 1 1) 1))))

    (test-assert (moment>? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2024 1 1) 2)))
    (test-assert (moment>? (make-moment (make-date 2025 1 1) 2) (make-moment (make-date 2025 1 1) 1)))
    (test-assert (not (moment>? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 1))))
    (test-assert (not (moment>? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2026 1 1) 1))))
    (test-assert (not (moment>? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 2))))

    (test-assert (moment>=? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2024 1 1) 2)))
    (test-assert (moment>=? (make-moment (make-date 2025 1 1) 2) (make-moment (make-date 2025 1 1) 1)))
    (test-assert (moment>=? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 1)))
    (test-assert (not (moment>=? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2026 1 1) 1))))
    (test-assert (not (moment>=? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 2)))))

(test-group "Timestamp constructors, getters"
    (test-assert (timestamp? (make-timestamp 2020 1 1 10 0 0 (tz-timezone "Europe/Vilnius"))))
    (test-assert (timestamp? (make-timestamp 2020 1 1 10 0 0 (tz-timezone "Europe/Vilnius") 0)))
    (test-assert (timestamp? (date+clock-time->timestamp (make-date 2020 1 1) (make-clock-time 10 0 0) (tz-timezone "Europe/Vilnius"))))
    (test-assert (timestamp? (date+clock-time->timestamp (make-date 2020 1 1) (make-clock-time 10 0 0) (tz-timezone "Europe/Vilnius") 0)))
    (test-error (make-timestamp 2020 1 1 10 0 0 (tz-timezone "Europe/Vilnius") 1))

    ;; clock is moved forward at 3:00, such time doesn't exist
    (test-error (make-timestamp 2025 3 30 3 30 0 (tz-timezone "Europe/Vilnius")))
    ;; clock is moved backwards at 4:00, this time happens twice, check if fold = 1 works
    (test-assert (timestamp? (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius") 0)))
    (test-assert (timestamp? (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius") 1)))

    ;; invalid leapsecond
    (test-error (make-timestamp 2015 7 1 2 58 60 (tz-timezone "Europe/Vilnius")))
    ;; valid leapsecond
    (test-assert (timestamp? (make-timestamp 2015 7 1 2 59 60 (tz-timezone "Europe/Vilnius"))))

    (test-assert (date=? (make-date 2020 1 1) (timestamp-date (make-timestamp 2020 1 1 10 0 0 (tz-timezone "Europe/Vilnius")))))
    (test-equal 2020 (timestamp-year (make-timestamp 2020 1 1 10 0 0 (tz-timezone "Europe/Vilnius"))))
    (test-equal 1 (timestamp-month (make-timestamp 2020 1 2 10 0 0 (tz-timezone "Europe/Vilnius"))))
    (test-equal 2 (timestamp-day (make-timestamp 2020 1 2 10 0 0 (tz-timezone "Europe/Vilnius"))))
    (let-values (((y m d) (timestamp-ymd (make-timestamp 2020 1 2 10 0 0 (tz-timezone "Europe/Vilnius")))))
        (test-equal 2020 y)
        (test-equal 1 m)
        (test-equal 2 d))

    (test-equal 10 (clock-time-hour (timestamp-clock-time (make-timestamp 2020 1 1 10 1 2 (tz-timezone "Europe/Vilnius")))))
    (test-equal 1 (clock-time-minute (timestamp-clock-time (make-timestamp 2020 1 1 10 1 2 (tz-timezone "Europe/Vilnius")))))
    (test-equal 2 (clock-time-second (timestamp-clock-time (make-timestamp 2020 1 1 10 1 2 (tz-timezone "Europe/Vilnius")))))
    (test-equal 10 (timestamp-hour (make-timestamp 2020 1 1 10 1 2 (tz-timezone "Europe/Vilnius"))))
    (test-equal 1 (timestamp-minute (make-timestamp 2020 1 1 10 1 2 (tz-timezone "Europe/Vilnius"))))
    (test-equal 2 (timestamp-second (make-timestamp 2020 1 1 10 1 2 (tz-timezone "Europe/Vilnius"))))
    (let-values (((h m s) (timestamp-hms (make-timestamp 2020 1 1 10 1 2 (tz-timezone "Europe/Vilnius")))))
        (test-equal 10 h)
        (test-equal 1 m)
        (test-equal 2 s))

    (test-equal (tz-timezone "Europe/Vilnius") (timestamp-timezone (make-timestamp 2020 1 1 10 1 2 (tz-timezone "Europe/Vilnius"))))
    (test-equal 0 (timestamp-fold (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius"))))
    (test-equal 1 (timestamp-fold (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius") 1))))

(test-group "Moment to/from timestamp conversion"
    ;; non-leap case
    (let ((moment (make-moment (make-date 2026 2 3) (+ (* 1 3600) (* 2 60) 3 37)))
          (timestamp (make-timestamp 2026 2 3 3 2 3 (tz-timezone "Europe/Vilnius"))))
      (test-assert (moment=? moment (timestamp->moment timestamp)))
      (test-assert (timestamp-equal? timestamp (moment->timestamp moment (tz-timezone "Europe/Vilnius")))))

    ;; leap case
    (let ((moment (make-moment (make-date 2015 7 1) (+ 35)))
          (timestamp (make-timestamp 2015 7 1 2 59 (+ 60) (tz-timezone "Europe/Vilnius"))))
      (test-assert (moment=? moment (timestamp->moment timestamp)))
      (test-assert (timestamp-equal? timestamp (moment->timestamp moment (tz-timezone "Europe/Vilnius"))))))

(test-group "Timestamp to/from posix"
    (let ((t1 (make-timestamp 2015 6 30 23 59 59 utc-timezone))
          (t2 (make-timestamp 2015 6 30 23 59 60 utc-timezone))
          (t3 (make-timestamp 2015 7 1 2 59 59 (tz-timezone "Europe/Vilnius")))
          (posix 1435708799))
      (test-equal posix (timestamp->utc-posix-time t1))
      (test-equal posix (timestamp->utc-posix-time t2))
      (test-equal posix (timestamp->utc-posix-time t3))
      (test-assert (timestamp-equal? t1 (posix-time->utc-timestamp posix)))))

(test-group "Timestamp timezone conversion"
    (let ((t1 (make-timestamp 2026 2 3 5 0 0 (tz-timezone "Europe/Vilnius")))
          (t2 (make-timestamp 2026 2 2 22 0 0 (tz-timezone "America/New_York"))))
      (test-assert (timestamp-equal? t1 (timestamp-in-timezone t2 (tz-timezone "Europe/Vilnius"))))
      (test-assert (timestamp-equal? t2 (timestamp-in-timezone t1 (tz-timezone "America/New_York"))))))

(test-group "Timestamp timezone offset"
    (let ((t1 (make-timestamp 2026 2 3 5 0 0 (tz-timezone "Europe/Vilnius")))
          (dt1 (hours-dt 2))
          (t2 (make-timestamp 2026 2 2 22 0 0 (tz-timezone "America/New_York")))
          (dt2 (hours-dt -5)))
      (test-assert (dt-equal? dt1 (timestamp-timezone-offset t1)))
      (test-assert (dt-equal? dt2 (timestamp-timezone-offset t2)))))

(test-group "Timestamp->ISO-8601"
    ;; TODO
    #t)

;; timezone

(test-group "Timezones"
    (test-assert (timezone? utc-timezone))
    (test-assert (timezone? (utc-offset-timezone (hours-dt 1))))
    (test-assert (timezone? (system-timezone)))
    (test-assert (list? (tz-timezones)))
    (test-assert (timezone? (tz-timezone (list-ref (tz-timezones) 0)))))

;; dt

(test-group "dt"
    (let* ((dt (dt+ (years-dt 1)
                    (years-dt 10)
                    (months-dt 2)
                    (months-dt 10)
                    (weeks-dt 3)
                    (weeks-dt 10)
                    (days-dt 4)
                    (days-dt 10)
                    (hours-dt 5)
                    (hours-dt 10)
                    (minutes-dt 6)
                    (minutes-dt 10)
                    (seconds-dt 7)
                    (seconds-dt 10)))
           (dt* (dt-negate dt)))
      (test-assert (dt? dt))
      (test-equal 11 (dt-years dt))
      (test-equal 12 (dt-months dt))
      (test-equal 13 (dt-weeks dt))
      (test-equal 14 (dt-days dt))
      (test-equal 15 (dt-hours dt))
      (test-equal 16 (dt-minutes dt))
      (test-equal 17 (dt-seconds dt))
      (test-equal -11 (dt-years dt*))
      (test-equal -12 (dt-months dt*))
      (test-equal -13 (dt-weeks dt*))
      (test-equal -14 (dt-days dt*))
      (test-equal -15 (dt-hours dt*))
      (test-equal -16 (dt-minutes dt*))
      (test-equal -17 (dt-seconds dt*))))

;; date arithmetic

(test-group "date+"
    (test-assert (date=? (make-date 2021 12 31) (date+ (make-date 2021 1 1) (dt+ (days-dt -1) (years-dt 1)))))
    (test-assert (date=? (make-date 2021 2 28) (date+ (make-date 2021 1 31) (months-dt 1))))
    (test-assert (date=? (make-date 2021 1 8) (date+ (make-date 2021 1 1) (weeks-dt 1))))
    (test-assert (date=? (make-date 2022 2 1) (date+ (make-date 2021 1 1) (months-dt 13))))
    (test-assert (date=? (make-date 2022 2 1) (date+ (make-date 2021 1 1) (days-dt 396))))
    (test-error (date+ (make-date 2021 1 1) (hours-dt 1)))
    (test-error (date+ (make-date 2021 1 1) (minutes-dt 1)))
    (test-error (date+ (make-date 2021 1 1) (seconds-dt 1))))

(test-end)
