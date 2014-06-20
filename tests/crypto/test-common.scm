

;; (include "md5.sps")
;; (include "sha-1.sps")
(include "uuid.sps")


(define (main-program)

  (define (s . data) (string-downcase (sha-1->string (apply sha-1 (map string->utf8 data)))))

  ;; (check (s "") => "da39a3ee5e6b4b0d3255bfef95601890afd80709")

  (write (make-random-bytevector 10))
  (newline)
  (write (random-positive-byte))
  (newline)
  (write (random-integer 1000))
  (newline)

  (check-report)
  #t)
