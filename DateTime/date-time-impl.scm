;; errors
(define-record-type <date-time-error>
  (make-date-time-error message args)
  date-time-error?
  (message date-time-error-message)
  (args date-time-error-args))

(define (date-error message . args)
  (raise (make-date-time-error message args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <date>
    (make-date* year month day)
    date?
    (year date-year)
    (month date-month)
    (day date-day))

;; for debuggging, TODO remove
(define-method write-object ((date <date>) port)
  (write (date->iso8601 date) port))

(define-method write-object ((err <date-time-error>) port)
  (write (cons (date-time-error-message err) (date-time-error-args err)) port))

(define (days-in-month is-leap-year month succ fail)
  (case month
      ((1 3 5 7 8 10 12) (succ 31))
      ((4 6 9 11)        (succ 30))
      ((2)               (succ (if is-leap-year 29 28)))
      (else (fail))))

(define (leap-year? year)
  (cond
    ((not (= 0 (floor-remainder year 4))) #f)
    ((not (= 0 (floor-remainder year 100))) #t)
    (else (= 0 (floor-remainder year 400)))))

(define (valid-date? date)
  (let* ((y (date-year date))
         (m (date-month date))
         (d (date-day date)))
    (and (integer? y)
         (integer? m)
         (integer? d)
         (<= 1 m 12)
         (days-in-month (leap-year? y) m
                        (lambda (d*) (<= 1 d d*))
                        (lambda () #f)))))

(define (make-date year month day)
  (let ((d (make-date* year month day)))
    (unless (valid-date? d)
      (date-error "make-date called with invalid parameters" year month day))
    d))

(define (date-ymd d)
  (unless (date? d)
    (date-error "date-ymd called with invalid parameters" d))
  (values (date-year d) (date-month d) (date-day d)))

(define (date-weekday date)
  (unless (date? date)
    (date-error "date-weekday called with invalid parameters" date))
  (call-with-values
    (lambda () (date-ymd date))
    (lambda (year month day)
      (let ((num (zeller-congruence year month day)))
        ;; Zeller's congruence returns 0 as saturday, need to shift so that 0 is sunday
        (floor-remainder (+ num 6) 7)))))

(define (date-iso-weekday date)
  (unless (date? date)
    (date-error "date-iso-weekday called with invalid parameters" date))
  (let ((weekday (date-weekday date)))
    ;; conver from [0 sunday,  6  saturday] to [1 monday, 7 sunday]
    (if (= 0 weekday) 7 weekday)))

;; https://en.wikipedia.org/wiki/Zeller%27s_congruence
(define (zeller-congruence year month day)
  ;; variables named to match formula in wiki page
  (let* ((q day)
         (m (if (>= month 3) month (+ month 12)))
         (adjYear (if (>= month 3) year (- year 1)))
         (K (floor-remainder adjYear 100))
         (J (floor-quotient adjYear 100)))
    (floor-remainder
      (+ q
         (floor-quotient (* 13 (+ m 1))
                         5)
         K
         (floor-quotient K 4)
         (floor-quotient J 4)
         (- (* 2 J)))
      7)))

(define (days-since-year-start date)
  (define-values (year month day) (date-ymd date))
  (define is-leap (leap-year? year))
  (let loop ((days 0)
             (i 1))
    (if (= i month)
        (+ days day)
        (days-in-month is-leap i
                       (lambda (d) (loop (+ days d) (+ 1 i)))
                       (lambda () (error "Internal date-time implementation bug"))))))

(define (apply-days-after-year-start year days)
  (define is-leap (leap-year? year))
  (let loop ((days days)
             (month 1))
    (days-in-month is-leap month
                   (lambda (d) (if (> days d)
                                   (loop (- days d)
                                         (+ 1 month))
                                   (make-date year month days)))
                   (lambda () (error "Internal date-time implementation bug")))))

(define (date-iso-week date)
  (unless (date? date)
    (date-error "date-iso-week called with invalid parameters" date))
  (call-with-values
    (lambda () (date-iso-week* date))
    (lambda (year week) week)))

(define (date-iso-week-year date)
  (unless (date? date)
    (date-error "date-iso-week-year called with invalid parameters" date))
  (call-with-values
    (lambda () (date-iso-week* date))
    (lambda (year week) year)))

;; returns two values -- week year and week number for the given date
(define (date-iso-week* date)
  ;; 3 cases to consider
  ;; if it's <= 3rd jan, it might belong to last year's week year
  ;; if it's >= 29 dec, it might belong to next year's week year
  ;; or it is this year's week year
  (define-values (year month day) (date-ymd date))
  (define weekday (date-iso-weekday date))
  (define year-offset
    (cond
      ((and (= month 1) (<= day 3) (>= (- weekday day) 4)) -1)
      ((and (= month 12) (>= day 29) (>= (- day weekday) 28)) 1)
      (else 0)))

  (case year-offset
    ((1)
     (values (+ 1 year) 1))
    ((-1)
     (let* ((day-of-week-on-jan1 (date-iso-weekday (make-date* (- (date-year date) 1) 1 1)))
            ;; first week is the one which has first thursday in the new year
            ;; offset is amount to add to `days` to make relative to monday of first week
            (offset (if (>= 5 day-of-week-on-jan1)
                        (- day-of-week-on-jan1 1)
                        (- 1 day-of-week-on-jan1)))
            (day-count (+ offset
                          (if (leap-year? (- year 1)) 366 365)
                          day)))
       (values (- year 1)
               (+ 1 (floor-quotient day-count 7)))))
    (else
      (let* ((day-of-week-on-jan1 (date-iso-weekday (make-date* (date-year date) 1 1)))
             ;; first week is the one which has first thursday in the new year
             ;; offset is amount to add to `days` to make relative to monday of first week (which could be in prev year)
             (offset (if (>= 5 day-of-week-on-jan1)
                         (- day-of-week-on-jan1 1)
                         (- 1 day-of-week-on-jan1)))
             (day-count (+ (days-since-year-start date) offset)))
        (values year
                (+ 1 (floor-quotient day-count 7)))))))

(define (left-pad str c min-length)
  (define pad-size (max 0 (- min-length (string-length str))))
  (if (> pad-size 0)
      (let ((pad (make-string pad-size c)))
        (string-append pad str))
      str))

(define (date->iso8601 date)
  (unless (date? date)
    (date-error "date-iso8601 called with invalid parameters" date))
  (let-values (((y m d) (date-ymd date)))
    (string-append
      (if (< y 0) "-" "")
      (left-pad (number->string (abs y)) #\0 4)
      "-"
      (left-pad (number->string m) #\0 2)
      "-"
      (left-pad (number->string d) #\0 2))))

(define (date=? date1 date2)
  (unless (and (date? date1) (date? date2))
    (date-error "date=? called with invalid parameters" date1 date2))
  (let-values (((y1 m1 d1) (date-ymd date1))
               ((y2 m2 d2) (date-ymd date2)))
    (and (= y1 y2) (= m1 m2) (= d1 d2))))

(define (date<? date1 date2)
  (unless (and (date? date1) (date? date2))
    (date-error "date<? called with invalid parameters" date1 date2))
  (let-values (((y1 m1 d1) (date-ymd date1))
               ((y2 m2 d2) (date-ymd date2)))
    (cond
      ((< y1 y2) #t)
      ((> y1 y2) #f)
      ((< m1 m2) #t)
      ((> m1 m2) #f)
      (else (< d1 d2)))))

(define (date<=? date1 date2)
  (unless (and (date? date1) (date? date2))
    (date-error "date<=? called with invalid parameters" date1 date2))
  (let-values (((y1 m1 d1) (date-ymd date1))
               ((y2 m2 d2) (date-ymd date2)))
    (cond
      ((< y1 y2) #t)
      ((> y1 y2) #f)
      ((< m1 m2) #t)
      ((> m1 m2) #f)
      (else (<= d1 d2)))))

(define (date>? date1 date2)
  (unless (and (date? date1) (date? date2))
    (date-error "date>? called with invalid parameters" date1 date2))
  (let-values (((y1 m1 d1) (date-ymd date1))
               ((y2 m2 d2) (date-ymd date2)))
    (cond
      ((> y1 y2) #t)
      ((< y1 y2) #f)
      ((> m1 m2) #t)
      ((< m1 m2) #f)
      (else (> d1 d2)))))

(define (date>=? date1 date2)
  (unless (and (date? date1) (date? date2))
    (date-error "date>=? called with invalid parameters" date1 date2))
  (let-values (((y1 m1 d1) (date-ymd date1))
               ((y2 m2 d2) (date-ymd date2)))
    (cond
      ((> y1 y2) #t)
      ((< y1 y2) #f)
      ((> m1 m2) #t)
      ((< m1 m2) #f)
      (else (>= d1 d2)))))

;; helpers for converting between iso date and rata / julian
(define days/4y   (+ (* 4 365) 1))
(define days/100y (+ (* 100 365) 24))
(define days/400y (+ (* 4 days/100y) 1))

;; handles only dates with positive year
(define (date->rata-die* date)
  (define (rd-for-year year)
    (let* (;; subtract the precalculated days of current year; those will be added by days-since-year-start
           (curr-year-adjust (if (leap-year? year) -366 -365))
           (part400s (* days/400y (floor-quotient year 400)))
           (year (floor-remainder year 400))
           (part100s (* days/100y (floor-quotient year 100)))
           (year (floor-remainder year 100))
           (part4s (* days/4y (floor-quotient year 4)))
           (year (floor-remainder year 4))
           (rest (* year 365)))
        (+ part400s part100s part4s rest curr-year-adjust)))
  (let ((year (date-year date)))
    (+ (rd-for-year year) (days-since-year-start date))))

(define (date->rata-die date)
  (unless (date? date)
    (date-error "date->rata-die called with invalid parameters" date))
  (let ((year (date-year date)))
    (if (>= year 0)
        (date->rata-die* date)
        ;; slowpath for negative date: compute n such that n*400 + year > 0
        ;; then the result is difference between rd of n*400 + 1 and n*400 + year
        (let* ((month (date-month date))
               (day (date-day date))
               (n (+ 1 (floor-quotient (abs year) 400)))
               (rd1 (date->rata-die* (make-date (+ year (* n 400)) month day)))
               (rd2 (date->rata-die* (make-date (+ 1 (* n 400)) 1 1))))
          (+ 1 (- rd1 rd2))))))

;; handles only positive RD
(define (rata-die->date* rd)
  (let* ((rd (- rd 1))
         (part400s (* 400 (floor-quotient rd days/400y)))
         (rd (floor-remainder rd days/400y))
         (part100s (* 100 (floor-quotient rd days/100y)))
         (rd (floor-remainder rd days/100y))
         (part4s (* 4 (floor-quotient rd days/4y)))
         (rd (floor-remainder rd days/4y))
         (rest (floor-quotient rd 365))
         (rd (floor-remainder rd 365))
         (year (+ part400s part100s part4s rest)))
    (apply-days-after-year-start (+ 1 year) (+ 1 rd))))

(define (rata-die->date rd)
  (unless (integer? rd)
    (date-error "rata-die->date called with invalid parameters" rd))
  (if (>= rd 1)
      (rata-die->date* rd)
      ;; slowpath for rd less than 1: compute n such that n*days/400y + rd > 0
      ;; then the result is computing date for n*days/400y + rd and
      ;; moving result's year value backwards by n * 400
      (let* ((n (+ 1 (floor-quotient (abs rd) days/400y)))
             (date* (rata-die->date* (+ (* n days/400y) rd)))
             (year (- (date-year date*) (* n 400))))
        (make-date* year (date-month date*) (date-day date*)))))

(define mjd-rd-offset (- 2400000 1721424))
(define unix-epoch-rd (date->rata-die (make-date 1970 1 1)))

(define (date->mjd date)
  (unless (date? date)
    (date-error "date->mjd called with invalid parameters" date))
  (- (date->rata-die date) mjd-rd-offset))

(define (mjd->date mjd)
  (unless (integer? mjd)
    (date-error "mjd->date called with invalid parameters" mjd))
  (rata-die->date (+ mjd-rd-offset mjd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clock times
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <clock-time>
  (make-clock-time* hour minute second)
  clock-time?
  (hour clock-time-hour)
  (minute clock-time-minute)
  (second clock-time-second))

(define (make-clock-time hour minute second)
  (unless (and (integer? hour)
               (<= 0 hour 23)
               (integer? minute)
               (<= 0 minute 59)
               (rational? second)
               (exact? second)
               (<= 0 second)
               (< second 61))
    (date-error "make-clock-time called with invalid parameters" hour minute second))
  (make-clock-time* hour minute second))

(define (clock-time-hms clock-time)
  (unless (clock-time? clock-time)
    (date-error "clock-time-hms called with invalid parameters" clock-time))
  (values (clock-time-hour clock-time)
          (clock-time-minute clock-time)
          (clock-time-second clock-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <moment>
  (make-moment* date second)
  moment?
  (date moment-date)
  (second moment-second-of-day))

(define (make-moment date second)
  (unless (and (date? date)
               (real? second)
               (exact? second)
               (<= 0 second)
               (< second 86400))
    (date-error "make-moment called with invalid parameters" date second))
  (make-moment* date second))

(define (moment=? m1 m2)
  (unless (and (moment? m1) (moment? m2))
    (date-error "moment=? called with invalid parameters" m1 m2))
  (and (date=? (moment-date m1) (moment-date m2))
       (= (moment-second-of-day m1) (moment-second-of-day m2))))

(define (moment<? m1 m2)
  (unless (and (moment? m1) (moment? m2))
    (date-error "moment<? called with invalid parameters" m1 m2))
  (or (date<? (moment-date m1) (moment-date m2))
      (and (date=? (moment-date m1) (moment-date m2))
           (< (moment-second-of-day m1) (moment-second-of-day m2)))))

(define (moment<=? m1 m2)
  (unless (and (moment? m1) (moment? m2))
    (date-error "moment<=? called with invalid parameters" m1 m2))
  (or (date<? (moment-date m1) (moment-date m2))
      (and (date=? (moment-date m1) (moment-date m2))
           (<= (moment-second-of-day m1) (moment-second-of-day m2)))))

(define (moment>? m1 m2)
  (unless (and (moment? m1) (moment? m2))
    (date-error "moment>? called with invalid parameters" m1 m2))
  (or (date>? (moment-date m1) (moment-date m2))
      (and (date=? (moment-date m1) (moment-date m2))
           (> (moment-second-of-day m1) (moment-second-of-day m2)))))

(define (moment>=? m1 m2)
  (unless (and (moment? m1) (moment? m2))
    (date-error "moment>=? called with invalid parameters" m1 m2))
  (or (date>? (moment-date m1) (moment-date m2))
      (and (date=? (moment-date m1) (moment-date m2))
           (>= (moment-second-of-day m1) (moment-second-of-day m2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timestamp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <timestamp>
    (make-timestamp* date time timezone fold)
    timestamp?
    (date timestamp-date)s
    (time timestamp-clock-time)
    (timezone timestamp-timezone)
    (fold timestamp-fold))

(define (timestamp-ymd timestamp)
    (unless (timestamp? timestamp)
        (date-error "timestamp-ymd called with invalid parameters" timestamp))
    (date-ymd (timestamp-date timestamp)))

(define (timestamp-year timestamp)
    (unless (timestamp? timestamp)
        (date-error "timestamp-year called with invalid parameters" timestamp))
    (date-year (timestamp-date timestamp)))

(define (timestamp-month timestamp)
    (unless (timestamp? timestamp)
        (date-error "timestamp-month called with invalid parameters" timestamp))
    (date-month (timestamp-date timestamp)))

(define (timestamp-day timestamp)
    (unless (timestamp? timestamp)
        (date-error "timestamp-day called with invalid parameters" timestamp))
    (date-day (timestamp-date timestamp)))

(define (timestamp-hms timestamp)
    (unless (timestamp? timestamp)
        (date-error "timestamp-hms called with invalid parameters" timestamp))
    (clock-time-hms (timestamp-clock-time timestamp)))

(define (timestamp-hour timestamp)
    (unless (timestamp? timestamp)
        (date-error "timestamp-hour called with invalid parameters" timestamp))
    (clock-time-hour (timestamp-clock-time timestamp)))

(define (timestamp-minute timestamp)
    (unless (timestamp? timestamp)
        (date-error "timestamp-minute called with invalid parameters" timestamp))
    (clock-time-minute (timestamp-clock-time timestamp)))

(define (timestamp-second timestamp)
    (unless (timestamp? timestamp)
        (date-error "timestamp-second called with invalid parameters" timestamp))
    (clock-time-second (timestamp-clock-time timestamp)))

(define make-timestamp
    (case-lambda
        ((year month day hour minute second timezone)
         (date+clock-time->timestamp (make-date year month day) (make-clock-time hour minute second) timezone 0))
        ((year month day hour minute second timezone fold)
         (date+clock-time->timestamp (make-date year month day) (make-clock-time hour minute second) timezone fold))))

(define date+clock-time->timestamp
    (case-lambda
        ((date time timezone)
         (date+clock-time->timestamp date time timezone 0))
        ((date time timezone fold)
         (unless (and (date? date)
                      (clock-time? time)
                      (timezone? timezone)
                      (number? fold)
                      (or (= 0 fold) (= 1 fold)))
           (date-error "date+clock-time->timestamp called with invalid parameters" date time timezone fold))
        (let ((timestamp (make-timestamp* date time timezone fold)))
          (validate-timestamp-time-in-timezone
              timestamp
              (lambda _ #t)
              (lambda _ (date-error "date+clock-time->timestamp called with invalid local time for the given timezone" date time timezone fold))
              (lambda _ (date-error "date+clock-time->timestamp called with invalid fold value for given local time in the given timezone" date time timezone fold)))
          (validate-timestamp-leapsecond timestamp
              (lambda _ #t)
              (lambda _ (date-error "date+clock-time->timestamp called with invalid (leap) second value" date time timezone fold)))
          timestamp))))

;; returns count of seconds (excluding leap) since 1970-01-01 in current timezone (ie, not UTC timezone)
;; used to find offset from timezone rules vector
(define (timestamp->local-timepoint timestamp)
    (define days (- (date->rata-die (timestamp-date timestamp)) unix-epoch-rd))
    (define-values (h m s) (timestamp-hms timestamp))
    (+ (* days 86400) (* h 3600) (* m 60) s))

;; not all hours / minutes are valid during transition between DST
;; if fold is 1, also tests if there is possible time overlap
(define (validate-timestamp-time-in-timezone timestamp ok err-bad-time err-bad-fold)
    (let* ((local-timepoint (timestamp->local-timepoint timestamp))
           (tz (timestamp-timezone timestamp))
           (offset (find-offset tz local-timepoint))
           (fold (timestamp-fold timestamp)))
      (cond
        ((= 0 (vector-length offset)) (err-bad-time))
        ((and (= 1 fold) (< (vector-length offset) 2)) (err-bad-fold))
        (else (ok)))))

(define (validate-timestamp-leapsecond timestamp ok err)
    ;; TODO
    (ok))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timezones
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (utc-offset-timezone dt)
  (unless (and (dt? dt)
               (= 0 (dt-years dt))
               (= 0 (dt-months dt))
               (= 0 (dt-weeks dt))
               (= 0 (dt-days dt))
               (= 0 (dt-seconds dt)))
    (date-error "utc-offset-timezone called with invalid parameters" dt))
  (let ((h (dt-hours dt))
        (m (dt-minutes dt)))
    (create-static-timezone (+ (* 3600 h) (* 60 m)))))

(define tzmap '())

(define (tz-timezones)
  ;; TODO
  '("Vilnius"))

(define (tz-timezone name)
  (unless (string? name)
    (date-error "tz-timezone called with invalid parameters" name))
  (cond
    ((assoc name tzmap) => cdr)
    ((not (member name (tz-timezones))) (date-error "tz-timezone called with invalid parameters" name))
    (else
      (let ((tz (load-timezone! name)))
        (set! tzmap (cons (cons name tz) tzmap))
        tz))))

(define (load-timezone! name)
  (define port #f)
  (dynamic-wind
    ;; TODO
    (lambda () (set! port (open-binary-input-file (string-append "./testdata/" name))))
    (lambda () (tzfile->timezone (read-tz-file port)))
    (lambda () (close-port port))))

(define (tzfile->timezone tzf)
  (create-timezone-from-tz-transitions (tzfile-time-transitions tzf)))

(define (system-timezone)
  ;; TODO
  (tz-timezone "Vilnius"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time deltas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <dt>
  (make-dt years months weeks days hours minutes seconds)
  dt?
  (years dt-years)
  (months dt-months)
  (weeks dt-weeks)
  (days dt-days)
  (hours dt-hours)
  (minutes dt-minutes)
  (seconds dt-seconds))

(define (years-dt v)
    (unless (integer? v)
        (date-error "years-dt called with invalid parameters" v))
    (make-dt v 0 0 0 0 0 0))

(define (months-dt v)
    (unless (integer? v)
        (date-error "months-dt called with invalid parameters" v))
    (make-dt 0 v 0 0 0 0 0))

(define (weeks-dt v)
    (unless (integer? v)
        (date-error "weeks-dt called with invalid parameters" v))
    (make-dt 0 0 v 0 0 0 0))

(define (days-dt v)
    (unless (integer? v)
        (date-error "days-dt called with invalid parameters" v))
    (make-dt 0 0 0 v 0 0 0))

(define (hours-dt v)
    (unless (integer? v)
        (date-error "hours-dt called with invalid parameters" v))
    (make-dt 0 0 0 0 v 0 0))

(define (minutes-dt v)
    (unless (integer? v)
        (date-error "minutes-dt called with invalid parameters" v))
    (make-dt 0 0 0 0 0 v 0))

(define (seconds-dt v)
    (unless (exact? v)
        (date-error "seconds-dt called with invalid parameters" v))
    (make-dt 0 0 0 0 0 0 v))

(define (dt+ . dts)
    (define (sum-component getter)
        (fold (lambda (dt sum) (+ sum (getter dt)))
              0
              dts))
    (for-each (lambda (obj)
                (unless (dt? obj)
                    (date-error "dt+ called with invalid parameters" dts)))
               dts)
    (make-dt
        (sum-component dt-years)
        (sum-component dt-months)
        (sum-component dt-weeks)
        (sum-component dt-days)
        (sum-component dt-hours)
        (sum-component dt-minutes)
        (sum-component dt-seconds)))

(define (dt-negate dt)
    (unless (dt? v)
        (date-error "dt-negate called with invalid parameters" dt))
    (make-dt
        (- (dt-years dt))
        (- (dt-months dt))
        (- (dt-weeks dt))
        (- (dt-days dt))
        (- (dt-hours dt))
        (- (dt-minutes dt))
        (- (dt-seconds dt))))
