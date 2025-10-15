;; errors
(define-record-type <date-time-error>
  (make-date-time-error message args)
  date-time-error?
  (message date-time-error-message)
  (args date-time-error-args))

(define (date-error message . args)
  (raise (make-date-time-error message args)))

;; date
(define-record-type <date>
    (make-date* year month day)
    date?
    (year date-year)
    (month date-month)
    (day date-day))

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
