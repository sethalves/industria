

;(include "md5.sps")
;(include "sha-1.sps")
;(include "uuid.sps")
;(include "crc.sps")
;(include "aes.sps")
;(include "arcfour.sps")
;(include "blowfish.sps")
(include "des.sps")

(define (bitwise-ifx ei1 ei2 ei3)
  (display "hi 0\n")

  (let* ((a (bitwise-and ei1 ei2))
         (xxx (begin
                (display "hi 1: ")
                (write ei1)
                (newline)))
         (c (bitwise-not ei1))
         (xxx   (display "hi 2\n"))
         (b (bitwise-and c ei3)))
    (display "hi 3\n")
    (let ((r (bitwise-ior a b)))
      (display "hi 4\n")
      r)
    ))

(define (copy-bit-fieldx to from start end)
  (display "copy-bit-field 0\n")
  (let* ((mask1 (arithmetic-shift -1 start))
         (xxx (display "copy-bit-field 1\n"))
         (mask2 (bitwise-not (arithmetic-shift -1 end)))
         (xxx (display "copy-bit-field 2\n"))
         (mask (bitwise-and mask1 mask2)))

    (display "end=") (write end) (newline)
    (display "mask1=") (write mask1) (newline)
    (display "mask2=") (write mask2) (newline)
    (display "mask=") (write mask) (newline)

    (display "copy-bit-field 3\n")
    (let ((r (bitwise-ifx mask (arithmetic-shift from start) to)))
      (display "copy-bit-field 4\n")
      r)
    ))

(define (main-program)

  ;; (let ((n 98413290)
  ;;       (start 0)
  ;;       (end 28)
  ;;       (field 196826580))
  ;;   (copy-bit-fieldx n start end field))

  (write (make-random-bytevector 10))
  (newline)
  (write (random-positive-byte))
  (newline)
  (write (random-integer 1000))
  (newline)

  (check-report)
  #t)
