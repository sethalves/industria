;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2011, 2012 Göran Weinholt <goran@weinholt.se>

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
;; #!r6rs

;; Syntax for defining procedures that calculate Cyclic Redundancy Codes.

;; Ross N. Williams, "A painless guide to CRC error detection
;; algorithms". http://www.ross.net/crc/crcpaper.html

;;; Simple usage with pre-defined CRCs

;; If you want to use one of the pre-defined CRCs

;; (define-crc crc-32)
;;     calculates the CRC table at expand-time and defines the
;;     procedures below

;; (crc-32 bytevector)
;;     returns the final CRC of the entire bytevector
;; (crc-32-init)
;;     returns an initial CRC state
;; (crc-32-update state bv)
;; (crc-32-update state bv start)
;; (crc-32-update state bv start end)
;;     returns a new state which includes the CRC on the given bytes
;; (crc-32-finish state)
;;     returns the final CRC
;; (crc-32-width)
;;     returns the bit-width of the CRC, e.g. 32 for CRC-32
;; (crc-32-self-test)
;;     returns 'sucess, 'failure, or 'no-self-test

;;; Advanced usage

;; Quick and possibly confusing guide to using define-crc with new
;; CRCs, for those who are too busy to read the above paper:

;; Syntax: (define-src name width polynomial init ref-in ref-out
;;                     xor-out check)

;; The width is the bitwise length of the polynomial. You might be
;; lead to believe that it should sometimes be 33, but if so you've
;; been counting the highest bit, which doesn't count.

;; The polynomial for CRC-16 is given sometimes given as x^16 + x^15 +
;; x^2 + 1. This translates to #b1000000000000101 (#x8005). Notice
;; that x^16 is absent. CRCs use polynomial division with modulo two
;; arithmetic (better known as XOR). Don't use the reversed polynomial
;; if you have one of those, instead set ref-in and ref-out properly.

;; After a CRC has been calculated it is sometimes XOR'd with a final
;; value, this is xor-out.

;; check is either the CRC of the ASCII string "123456789", or #f.

;; Syntax: (define-crc name (coefficients ...) init ref-in ref-out
;;                     xor-out check)

;; This is the slightly easier interface where you can simply specify
;; the powers of the coefficients. CRC-16 in this syntax becomes:

;; (define-crc crc-16 (16 15 2 0) #x0000 #t #t #x0000 #xBB3D)

;; Another example: the polynomial x^8 + x^2 + x + 1 in this syntax
;; becomes: (8 2 1 0)

(define-library (weinholt crypto crc)
  (export define-crc decode-coefficients

          crc-32 crc-32-init crc-32-finish
          crc-32-self-test crc-32-width crc-32-update

          crc-16 crc-16-init crc-16-finish crc-16-self-test
          crc-16-width crc-16-update

          crc-16/ccitt crc-16/ccitt-init crc-16/ccitt-finish
          crc-16/ccitt-self-test crc-16/ccitt-width crc-16/ccitt-update

          crc-32c crc-32c-init crc-32c-finish crc-32c-self-test
          crc-32c-width crc-32c-update

          crc-24 crc-24-init crc-24-finish crc-24-self-test
          crc-24-width crc-24-update

          crc-64 crc-64-init crc-64-finish crc-64-self-test
          crc-64-width crc-64-update

          crc-64/ecma-182-init crc-64/ecma-182-finish
          crc-64/ecma-182-self-test crc-64/ecma-182-width
          crc-64/ecma-182-update
          )
  (import (scheme base)
          (scheme case-lambda)
          (only (srfi 1) iota)
          ;; (for (only (srfi :1 lists) iota) expand)
          (srfi 60)
          (weinholt bytevectors)
          (weinholt r6rs-compatibility)
          )

  (cond-expand
   (chicken
    (import (numbers)))
   (else))

  (begin

    (define (decode-coefficients coeffs)
      (do ((i coeffs (cdr i))
           (p 0 (bitwise-ior p (arithmetic-shift 1 (car i)))))
          ((null? i) p)))


    (define (calc-table width ref-in poly)
      (define (calc-table-i index)
        (do ((bit 0 (+ bit 1))
             (r (arithmetic-shift index (- width 8))
                (if (bitwise-bit-set? r (- width 1))
                    (bitwise-xor (arithmetic-shift r 1) poly)
                    (arithmetic-shift r 1))))
            ((= bit 8)
             (bitwise-bit-field r 0 width))))
      (list->vector
       (map (lambda (index)
              (if ref-in
                  (bitwise-reverse-bit-field
                   (calc-table-i (bitwise-reverse-bit-field index 0 8))
                   0 width)
                  (calc-table-i index)))
            (iota 256))))


    (define-syntax define-crc
      (syntax-rules ()
        ((define-crc name width polynomial init ref-in ref-out xor-out check
           crc-init crc-finish crc-self-test crc-width crc-update crc-table)
         (begin
           (define (crc-init) init)
           (define (crc-finish r) (bitwise-xor r xor-out))

           (define (crc-self-test)
             (if check
                 (if (= (name (string->utf8 "123456789")) check)
                     'success 'failure)
                 'no-self-test))

           (define (crc-width) width)

           ;; XXX in the original r6rs version, this table
           ;; was generated at macro-expand time, rather than
           ;; here.
           (define crc-table (calc-table width ref-in polynomial))

           (define (crc-update r* bv . maybe-start+end)
             (let* ((start (if (pair? maybe-start+end)
                               (car maybe-start+end)
                               0))
                    (end (if (and (pair? maybe-start+end)
                                  (pair? (cdr maybe-start+end)))
                             (cadr maybe-start+end)
                             (bytevector-length bv))))
               (let ((t crc-table)
                     (mask (- (arithmetic-shift 1 (- width 8)) 1)))
                 (do ((i start (+ i 1))
                      (r r*
                         ;; TODO: implement the other ref-in ref-out
                         ;; combinations?
                         (cond ((and ref-in ref-out)
                                (bitwise-xor
                                 (arithmetic-shift r -8)
                                 (vector-ref
                                  t (bitwise-xor (bitwise-and r #xff)
                                                 (bytevector-u8-ref bv i)))))
                               ((and (not ref-in) (not ref-out))
                                (bitwise-xor
                                 (arithmetic-shift (bitwise-and mask r) 8)
                                 (vector-ref
                                  t (bitwise-xor
                                     (bytevector-u8-ref bv i)
                                     (bitwise-and
                                      (arithmetic-shift r (- 8 width))
                                      #xff)))))
                               (else (error "unimplemented reflection")))))
                     ((= i end) r)))))

           (define (name bv)
             (crc-finish (crc-update (crc-init) bv)))))))


    ;; Used for .ZIP, AUTODIN II, Ethernet, FDDI, PNG, MPEG-2
    ;; and various other things.
    (define-crc crc-32 32 #x04C11DB7 #xFFFFFFFF #t #t #xFFFFFFFF #xCBF43926
      crc-32-init crc-32-finish crc-32-self-test crc-32-width crc-32-update
      crc-32-table)

    (define-crc crc-16 16 #x8005 #x0000 #t #t #x0000 #xBB3D
      crc-16-init crc-16-finish crc-16-self-test crc-16-width crc-16-update
      crc-16-table)

    ;; Used by XMODEM, PPP and much more
    (define-crc crc-16/ccitt 16 #x1021 #xffff #f #f 0 #x29B1
      crc-16/ccitt-init crc-16/ccitt-finish crc-16/ccitt-self-test
      crc-16/ccitt-width crc-16/ccitt-update crc-16/ccitt-table)

    ;; CRC-32C specified in e.g. RFC4960 or RFC3385. Used by SCTP
    ;; and iSCSI. Finds more errors than CRC-32.
    (define-crc crc-32c 32 #x1EDC6F41 #xFFFFFFFF #t #t #xFFFFFFFF #xE3069283
      crc-32c-init crc-32c-finish crc-32c-self-test crc-32c-width
      crc-32c-update crc-32c-table)

    ;; OpenPGP, see RFC2440.
    (define-crc crc-24 24
      (decode-coefficients '(24 23 18 17 14 11 10 7 6 5 4 3 1 0))
      #xB704CE #f #f 0 #x21CF02
      crc-24-init crc-24-finish crc-24-self-test crc-24-width
      crc-24-update crc-24-table)

    (define-crc crc-64 64
      (decode-coefficients '(64 4 3 1 0))
      0 #t #t 0 #x46A5A9388A5BEFFE
      crc-64-init crc-64-finish crc-64-self-test crc-64-width
      crc-64-update crc-64-table)

    (define-crc crc-64/ecma-182
      64
      (decode-coefficients
       '(64 62 57 55 54 53 52 47 46 45 40 39 38 37 35 33 32 31
            29 27 24 23 22 21 19 17 13 12 10 9 7 4 1 0))
      #xFFFFFFFFFFFFFFFF #t #t #xFFFFFFFFFFFFFFFF
      #x995DC9BBDF1939FA
      crc-64/ecma-182-init crc-64/ecma-182-finish
      crc-64/ecma-182-self-test crc-64/ecma-182-width
      crc-64/ecma-182-update crc-64/ecma-182-table)


    ))
