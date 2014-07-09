

(include "md5.sps")
(include "sha-1.sps")
(include "uuid.sps")
(include "crc.sps")
(include "aes.sps")
(include "arcfour.sps")


(define (main-program)

  (write (make-random-bytevector 10))
  (newline)
  (write (random-positive-byte))
  (newline)
  (write (random-integer 1000))
  (newline)

  (check-report)
  #t)
