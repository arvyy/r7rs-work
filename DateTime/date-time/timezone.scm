(define-record-type <rules-timezone>
    (rules-timezone rules)
    timezone?
    ;; rules is a vector of ranges, each entry is itself a vector: #(local-start local-end offset)
    ;; start is inclusive, end is exclusive
    ;; start and end can be #f if they denote unbound range, otherwise its a timespoint integer
    ;; offset is value in seconds of the offset from UTC
    (rules rules))

(define (create-static-timezone offset)
    (rules-timezone (vector (vector #f #f offset))) )

(define (transition->local-timestamp t)
    (+ (time-transition-timepoint t) (time-transition-offset t)))

(define (create-timezone-from-tz-transitions transitions)
    ;; first transition is effectively ignored, because its offset is treated and for the entire period before it
    ;; treat specially if the transition has just one element
    (if (= (vector-length transitions) 1)
        (create-static-timezone (time-transition-offset (vector-ref transitions 0)))
        (do ((rules (let ((v (make-vector (vector-length transitions)))
                          (t1 (vector-ref transitions 0))
                          (t2 (vector-ref transitions 1)))
                      (vector-set! v 0 (vector #f (+ (time-transition-timepoint t2) (time-transition-offset t1)) (time-transition-offset t1)))
                      v))
             (i 1 (+ i 1))
             (l (vector-length transitions)))
            ((>= i l) (rules-timezone rules))
          (let* ((transition (vector-ref transitions i))
                 (start-point (+ (time-transition-timepoint transition) (time-transition-offset transition)))
                 (end-point (if (>= (+ 1 i) l)
                                #f
                                (+ (time-transition-timepoint (vector-ref transitions (+ i 1))) (time-transition-offset transition))))
                 (offset (time-transition-offset transition))
                 (rule (vector start-point end-point offset)))
            (vector-set! rules i rule)))))

;; returns -1 if given timepoint is before the rule range
;; returns 1 if given timepoint is after the rule range
;; returns 0 if given timepoint is inside the rule range
(define (test-rule-range rule local-timepoint)
    (let ((t1 (vector-ref rule 0))
          (t2 (vector-ref rule 1)))
      (cond
          ((< local-timepoint t1) -1)
          ((>= local-timepoint t2) 1)
          (else 0))))

;; binary search to find an index for a rule that matches given local-timepoint
;; if multiple ranges match doesn't guarantee which one will be returned
;; may return #f for non existing local timepoints (such as during DST transition)
(define (find-rule-index rules start-range end-range local-timepoint)
    (if (= start-range end-range)
        (if (= 0 (test-rule-range (vector-ref rules start-range) local-timepoint))
            start-range
            #f)
        (let* ((midpoint (floor (/ (+ start-range end-range) 2)))
               (test-result (test-rule-range (vector-ref rules midpoint) local-timepoint)))
          (case test-result
              ((-1) (find-rule-index rules start-range midpoint local-timepoint))
              ((0)  midpoint)
              ((1)  (find-rule-index rules (+ 1 midpoint) end-range local-timepoint))))))

(define (find-offset timezone local-timepoint)
    ;; test both value at index and adjacent ones in case of overlap
    (let* ((r (rules timezone))
           (index (find-rule-index r 0 (vector-length r) local-timepoint))
           (t1 (if (and index
                        (> index 0)
                        (= 0 (test-rule-range (vector-ref r (- index 1)) local-timepoint)))
                    (vector (vector-ref r (- index 1)))
                    (vector)))
           (t2 (if index
                   (vector (vector-ref r index))
                   (vector)))
           (t3 (if (and index
                        (< index (- (vector-length r) 1))
                        (= 0 (test-rule-range (vector-ref r (+ index 1)) local-timepoint)))
                    (vector (vector-ref r (+ index 1)))
                    (vector))))
      (vector-append t1 t2 t3)))
