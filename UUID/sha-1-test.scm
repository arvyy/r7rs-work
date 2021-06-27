(cond-expand
  (chibi
   (import (scheme base)
           (scheme list)
           (scheme vector)
           (scheme bitwise)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (scheme list)
           (scheme vector)
           (scheme bitwise)
           (srfi 64))))

(include "sha-1.scm")

(test-begin "sha-1")

;; test cases taken from https://www.di-mgt.com.au/sha_testvectors.html
(test-equal "a9993e364706816aba3e25717850c26c9cd0d89d" (sha-1 (string->utf8 "abc")))
(test-equal "da39a3ee5e6b4b0d3255bfef95601890afd80709" (sha-1 (string->utf8 "")))
(test-equal "84983e441c3bd26ebaae4aa1f95129e5e54670f1" (sha-1 (string->utf8 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")))
(test-equal "a49b2446a02c645bf419f995b67091253a04a259" (sha-1 (string->utf8 "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu")))
; commented out, because takes long time to complete
;(test-equal "34aa973cd4c4daa4f61eeb2bdbad27316534016f" (sha-1 (string->utf8 (make-string 1000000 #\a))))


(test-end)
