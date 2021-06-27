(define (handle-overflow num)
  (define vec (bits->vector num))
  (if (> (vector-length vec) 32)
      (vector->bits (vector-copy vec 0 32))
      num))

(define (bytevector->int v)
  (let* ((vecs (map (lambda (index)
                      (let* ((byte-bits (bits->vector (bytevector-u8-ref v index)))
                             (l (vector-length byte-bits))
                             (byte-bits (if (= 8 l)
                                            byte-bits
                                            (vector-append byte-bits (make-vector (- 8 l) #f)))))
                        byte-bits))
                    (reverse (iota (bytevector-length v)))))
         (bits-vec (apply vector-append vecs)))
    (vector->bits bits-vec)))

(define (long->bytevector i)
  (let* ((vec (vector-reverse-copy (bits->vector i)))
         (vec (vector-append (make-vector (- 64 (vector-length vec)) #f) vec))
         (vecs (map
                (lambda (byte-index)
                  (vector-reverse-copy vec (* byte-index 8) (* (+ 1 byte-index) 8)))
                (iota 8)))
         (bytes (map vector->bits vecs)))
    (apply bytevector bytes)))

(define (sha/pad input)
  (let* ((l (bytevector-length input))
         (boundary (* 64 (+ 1 (quotient l 64))))
         (boundary (if (<= (- boundary l) 9)
                       (+ boundary 64)
                       boundary))
         (pad-1 #u8(#x80))
         (pad-0s (make-bytevector (- boundary l 9) 0))
         (pad-length (long->bytevector (* 8 l)))
         (result (bytevector-append input pad-1 pad-0s pad-length)))
    (unless (= 0 (remainder (bytevector-length result) 64))
      (error "wrong size after pad" (bytevector-length result)))
    result))

(define (sha/f t B C D)
  (cond
   ((<= t 19) (bitwise-ior (bitwise-and B C)
                           (bitwise-and (bitwise-not B) D)))
   ((<= t 39) (bitwise-xor B C D))
   ((<= t 59) (bitwise-ior (bitwise-and B C)
                           (bitwise-and B D)
                           (bitwise-and C D)))
   ((<= t 79) (bitwise-xor B C D))
   (else (error "shouldn't happen"))))

(define (sha/K t)
  (cond
   ((<= t 19) #x5a827999)
   ((<= t 39) #x6ed9eba1)
   ((<= t 59) #x8f1bbcdc)
   ((<= t 79) #xca62c1d6)
   (else (error "shouldn't happen"))))

(define (sha/S^n n x)
  (let* ((n (- 32 n))
         (vec (bits->vector x))
         (l (vector-length vec))
         (vec (if (= 32 l)
                  vec
                  (vector-append vec (make-vector (- 32 l) #f))))
         (vec1 (vector-copy vec 0 n))
         (vec2 (vector-copy vec n 32)))
    (vector->bits (vector-append vec2 vec1))))

(define (block->words input block-start)
  (vector-map
   (lambda (index)
     (bytevector->int (bytevector-copy input
                                       (+ block-start (* 4 index))
                                       (+ block-start (* 4 (+ 1 index))))))
   (list->vector (iota 16))))

(define (sha/process input block-start H)
  (define W (vector-append (block->words input block-start)
                           (make-vector 64 0)))
  (define A (vector-ref H 0))
  (define B (vector-ref H 1))
  (define C (vector-ref H 2))
  (define D (vector-ref H 3))
  (define E (vector-ref H 4))

  (do ((t 16 (+ 1 t)))
      ((> t 79))
    (vector-set! W t (sha/S^n 1 (bitwise-xor (vector-ref W (- t 3))
                                             (vector-ref W (- t 8))
                                             (vector-ref W (- t 14))
                                             (vector-ref W (- t 16))))))

  (do ((t 0 (+ 1 t)))
      ((> t 79))
    (let ((temp (+ (sha/S^n 5 A)
                   (sha/f t B C D)
                   E
                   (vector-ref W t)
                   (sha/K t))))
      (set! E D)
      (set! D C)
      (set! C (sha/S^n 30 B))
      (set! B A)
      (set! A (handle-overflow temp))))

  (let ((new-H (vector (handle-overflow (+ (vector-ref H 0) A))
                       (handle-overflow (+ (vector-ref H 1) B))
                       (handle-overflow (+ (vector-ref H 2) C))
                       (handle-overflow (+ (vector-ref H 3) D))
                       (handle-overflow (+ (vector-ref H 4) E))))
        (new-block-start (+ 64 block-start)))
    (if (>= new-block-start (bytevector-length input))
        new-H
        (sha/process input new-block-start new-H))))

(define (sha-1 input)
  (define rez (sha/process (sha/pad input) 0 (vector #x67452301
                                                     #xefcdab89
                                                     #x98badcfe
                                                     #x10325476
                                                     #xc3d2e1f0)))
  (apply string-append (vector->list (vector-map (lambda (num) (number->string num 16)) rez))))
