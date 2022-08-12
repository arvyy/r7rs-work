(define-record-type <heap>
  (make-heap-private comparator count key data)
  heap?
  (comparator heap-comparator)
  (count heap-count set-heap-count!)
  (key heap-key)
  (data heap-data set-heap-data!))

(define (make-heap comparator size key)
  (assume (comparator? comparator))
  (assume (or 
            (not size)
            (and (integer? size) (>= size 0))))
  (assume (or (not key) (procedure? key)))
  (make-heap-private comparator 
                     0 
                     (or key (lambda (x) x)) 
                     (make-vector (or size 10) #f)))

(define (make-ordered? heap)
  (define fn (heap-key heap))
  (define cmp (heap-comparator heap))
  (lambda (a b)
    (<=? cmp (fn a) (fn b))))

(define (grow-if-full heap)
  (define count (heap-count heap))
  (define data (heap-data heap))
  (when (= (vector-length data) count)
    (let ((new-vec (make-vector (* 2 count) #f)))
      (do ((i 0 (+ i 1)))
          ((< i count))
        (vector-set! new-vec i (vector-ref data i)))
      (set-heap-data! heap new-vec))))

(define (parent-index index)
  (floor (/ (- index 1) 2)))

(define (child-index index)
  (+ (* 2 index) 1))

(define (bubble ordered? vec index)
  (cond
    ((= index 0) #t)
    (else (let* ((pindex (parent-index index))
                 (parent (vector-ref vec pindex))
                 (child (vector-ref vec index))
                 (should-swap (not (ordered? parent child))))
            (when should-swap
              (begin
                (vector-set! vec index parent)
                (vector-set! vec pindex child)
                (bubble ordered? vec pindex)))))))

(define (sink ordered? count vec index)
  (define cindex (child-index index))
  (cond
    ((>= cindex count) #t)
    (else (let* ((parent (vector-ref vec index))
                 (child (vector-ref vec cindex))
                 (should-swap (not (ordered? parent child))))
            (when should-swap
              (begin
                (vector-set! vec index child)
                (vector-set! vec cindex parent)
                (sink ordered? count vec cindex)))))))

(define (heap-insert! heap element)
  (assume (heap? heap))
  (grow-if-full heap)
  (let ((count (heap-count heap))
        (ordered? (make-ordered? heap))
        (data (heap-data heap)))
    (vector-set! data count element)
    (bubble ordered? data count)
    (set-heap-count! heap (+ 1 count))))

(define (heap-pop! heap)
  (assume (heap? heap))
  (when (heap-empty? heap)
    (error "heap-pop! called on empty heap"))
  (let* ((data (heap-data heap))
         (el (vector-ref data 0))
         (count (heap-count heap))
         (ordered? (make-ordered? heap))
         (new-count (- count 1)))
    (cond
      ((zero? new-count) (vector-set! data 0 #f))
      (else (begin
              (vector-set! data 0 (vector-ref data (- count 1)))
              (vector-set! data new-count #f)
              (sink ordered? new-count data 0))))
    (set-heap-count! heap new-count)
    el))

(define (heap-top heap)
  (assume (heap? heap))
  (when (heap-empty? heap)
    (error "heap-top called on empty heap"))
  (vector-ref (heap-data heap) 0))

(define (heap-delete! heap element max-deletes)
  (assume (heap? heap))
  (let ((data (heap-data heap))
        (ordered? (make-ordered? heap))
        (matches? (let* ((cmp (heap-comparator heap))
                         (fn (heap-key heap))
                         (element* (fn element)))
                    (lambda (a)
                      (=? cmp (fn a) element*)))))
    (let loop ((index 0)
               (count (heap-count heap))
               (deleted 0))
      (cond
        ((or (and (integer? max-deletes)
                  (>= deleted max-deletes))
             (>= index count))
         (begin
           (set-heap-count! heap count)
           deleted))
        ((matches? (vector-ref data index))
         (begin
           (heap-delete-index ordered? count data index)
           (loop index
                 (- count 1)
                 (+ deleted 1))))
        (else
          (loop (+ index 1)
                count
                deleted))))))

(define (heap-delete-index ordered? count vec index)
  (define new-count (- count 1))
  (vector-set! vec index (vector-ref vec new-count))
  (vector-set! vec new-count #f)
  (sink ordered? new-count vec index))

(define (heap-map proc heap)
  (assume (heap? heap))
  (let ((new-heap (make-heap (heap-comparator heap) (heap-size heap) (heap-key heap)))
        (data (heap-data heap))
        (count (heap-count heap)))
    (let loop ((i 0))
      (if (>= i count)
        new-heap
        (begin
          (heap-insert! new-heap (proc (vector-ref data i)))
          (loop (+ i 1)))))))

(define (heap-empty? heap)
  (assume (heap? heap))
  (= 0 (heap-count heap)))

(define (heap-size heap)
  (assume (heap? heap))
  (vector-length (heap-data heap)))
