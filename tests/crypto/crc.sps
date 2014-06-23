
(check (crc-32-self-test) => 'success)

(check (crc-16-self-test) => 'success)

(check (crc-16/ccitt-self-test) => 'success)

(check (crc-32c-self-test) => 'success)

(check (crc-24-self-test) => 'success)

(check (crc-64-self-test) => 'success)

;; Tests the other procedures

(check (crc-32c-finish
        (crc-32c-update (crc-32c-update (crc-32c-init)
                                        (string->utf8 "12345"))
                        (string->utf8 "6789")))
       => #xE3069283)

(check (crc-32c-finish
        (crc-32c-update (crc-32c-update (crc-32c-init)
                                        (string->utf8 "XX12345") 2)
                        (string->utf8 "XX6789XX")  2 6))
       => #xE3069283)

(check (crc-32c (string->utf8 "123456789"))
       => #xE3069283)

;; Test the syntax for defining new CRCs

(define-crc crc-test 24
  (decode-coefficients '(24 23 18 17 14 11 10 7 6 5 4 3 1 0))
  #xB704CE #f #f 0 #x21CF02
  crc-test-init crc-test-finish
  crc-test-self-test crc-test-width crc-test-update crc-test-table)  ;CRC-24

(check (crc-test-self-test) => 'success)

;; ;; And last a test for Adler-32
;; (check (adler-32-self-test) => 'success)

(check-report)
