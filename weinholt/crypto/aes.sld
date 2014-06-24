;; -*- mode: scheme; coding: utf-8 -*-
;; Advanced Encryption Standard (AES), FIPS-197.
;; Copyright © 2009, 2010, 2012 Göran Weinholt <goran@weinholt.se>

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

;; This is an implementation of the Rijndael cipher as parameterized
;; by AES (the block length is 16 bytes, keys are 128, 192 or 256 bits
;; long with 10, 12 and 14 rounds respectively).

;; For usage etc see the manual.

;;; Implementation details

;; The main operations in the encryption and decryption procedures are
;; bitwise-xor, bitwise-and and bitwise-arithmetic-shift-right. The
;; operands are 32-bit integers, mostly from vector-ref. If your
;; implementation does flow analysis then it might be beneficial to
;; switch to bytevectors and bytevector-u32-native-ref.

;; There's nothing original here, just a straightforward
;; implementation of some of the ideas presented in these papers:

;; @MISC{Daemen98aesproposal:,
;;     author = {Joan Daemen and Vincent Rijmen},
;;     title = {AES Proposal: Rijndael},
;;     year = {1998}
;; }

;; @misc{AES-FIPS,
;;    title = "Specification for the Advanced Encryption Standard (AES)",
;;    howpublished = "Federal Information Processing Standards Publication 197",
;;    year = "2001",
;;    url = "http://csrc.nist.gov/publications/fips/fips197/fips-197.pdf"
;; }

;; @inproceedings{ BS08,
;;     author      = {Daniel J. Bernstein and Peter Schwabe},
;;     title       = {New {AES} software speed records},
;;     year        = {2008},
;;     booktitle   = {Progress in Cryptology - {INDOCRYPT 2008}},
;;     series      = {Lecture Notes in Computer Science},
;;     volume      = {5365},
;;     pages       = {322--336},
;;     publisher   = {Springer},
;; }
;; http://www.cryptojedi.org/papers/aesspeed-20080926.pdf
;; http://cr.yp.to/aes-speed/aesspeed-20080926.pdf

;; @MISC{Trichina04secureand,
;;     author = {E. Trichina and L. Korkishko},
;;     title = {Secure and Efficient AES Software Implementation for Smart Cards},
;;     year = {2004}
;; }
;; http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.58.2363

(define-library (weinholt crypto aes)
  (export expand-aes-key aes-encrypt!
          reverse-aes-schedule aes-decrypt!
          clear-aes-schedule!
          aes-ctr!
          aes-cbc-encrypt! aes-cbc-decrypt!)
  (import (scheme base)
          (weinholt crypto aes private)
          (only (srfi 1) iota split-at concatenate)
          (srfi 60)
          (weinholt r6rs-compatibility)
          (scheme eval))

  (begin

  (define (byte b n)
    (bitwise-and #xff (arithmetic-shift b (* n -8))))

  (define (copy-byte b n)
    (bitwise-and (arithmetic-shift #xff (* n 8)) b))

  (define (uncat l n)
    (if (null? l)
        l
        (let-values (((this next) (split-at l n)))
          (cons this (uncat next n)))))

;;; Lookup tables

  ;; The math for these tables is in the private library.
  (define (rcon-table n)
    (list->vector
     (map (lambda (i)
            (arithmetic-shift (GFexpt 2 i) 24))
          (iota n))))

  (define (sbox-table t e0 e1 e2 e3)
    (define (table-entry i)
      (bitwise-ior (arithmetic-shift (GF* e0 (t i)) 24)
                   (arithmetic-shift (GF* e1 (t i)) 16)
                   (arithmetic-shift (GF* e2 (t i)) 8)
                   (GF* e3 (t i))))
    (list->vector (map table-entry (iota 256))))

  (define (table-S e0 e1 e2 e3)
    (sbox-table S-box e0 e1 e2 e3))

  (define (table-invS e0 e1 e2 e3)
    (sbox-table inv-S-box e0 e1 e2 e3))


  ;; Various values for the round constant (expt 2 n) in GF(2⁸).
  (define rcon (rcon-table 20))
  ;; Tables for multiplication and SubBytes from [Daemen98aesproposal]
  (define te0 (table-S #b00000010 #b00000001 #b00000001 #b00000011))
  (define te1 (table-S #b00000011 #b00000010 #b00000001 #b00000001))
  (define te2 (table-S #b00000001 #b00000011 #b00000010 #b00000001))
  (define te3 (table-S #b00000001 #b00000001 #b00000011 #b00000010))
  (define te4 (table-S 1 1 1 1))
  (define td0 (table-invS #b00001110 #b00001001 #b00001101 #b00001011))
  (define td1 (table-invS #b00001011 #b00001110 #b00001001 #b00001101))
  (define td2 (table-invS #b00001101 #b00001011 #b00001110 #b00001001))
  (define td3 (table-invS #b00001001 #b00001101 #b00001011 #b00001110))
  (define td4 (table-invS 1 1 1 1))

;;; Enciphering

  (define (expand-aes-key key)
    (let* ((rounds (case (bytevector-length key)
                     ((128/8) 10)
                     ((192/8) 12)
                     ((256/8) 14)
                     (else (error 'expand-encryption-key "bad key size"
                                  (* 8 (bytevector-length key))))))
           (ret (make-vector (* 4 (+ 1 rounds))))
           (len (quotient (bytevector-length key) 4)))
      (do ((i 0 (+ i 1)))
          ((= i len))
        (vector-set! ret i (bytevector-u32-ref key (* i 4) 'big)))
      (do ((schedlen (vector-length ret))
           (i len (+ i 1)))
          ((= i schedlen) ret)
        (let ((Wi-Nk (vector-ref ret (- i len)))
              (Wi-1 (vector-ref ret (- i 1))))
          (cond ((zero? (modulo i len))
                 (vector-set!
                  ret i
                  (bitwise-xor
                   Wi-Nk
                   (vector-ref rcon (quotient (- i len) len))
                   (copy-byte (vector-ref te4 (byte Wi-1 2)) 3)
                   (copy-byte (vector-ref te4 (byte Wi-1 1)) 2)
                   (copy-byte (vector-ref te4 (byte Wi-1 0)) 1)
                   (copy-byte (vector-ref te4 (byte Wi-1 3)) 0))))
                ((and (> len 6) (= (modulo i len) 4))
                 (vector-set!
                  ret i
                  (bitwise-xor
                   Wi-Nk
                   (copy-byte (vector-ref te4 (byte Wi-1 3)) 3)
                   (copy-byte (vector-ref te4 (byte Wi-1 2)) 2)
                   (copy-byte (vector-ref te4 (byte Wi-1 1)) 1)
                   (copy-byte (vector-ref te4 (byte Wi-1 0)) 0))))
                (else
                 (vector-set! ret i (bitwise-xor Wi-Nk Wi-1))))))))


  (define (aes-encrypt! in in-start out out-start key-schedule)
    ;; First add the first round key. Then do n-1 rounds of
    ;; SubBytes, ShiftRows, MixColumns and AddRoundKey.
    (do ((len (vector-length key-schedule))
         (i 4 (+ i 4))
         (a0 (bitwise-xor (vector-ref key-schedule 0)
                          (bytevector-u32-ref in in-start 'big))
             (bitwise-xor (vector-ref key-schedule i)
                          (vector-ref te0 (byte a0 3))
                          (vector-ref te1 (byte a1 2))
                          (vector-ref te2 (byte a2 1))
                          (vector-ref te3 (byte a3 0))))
         (a1 (bitwise-xor (vector-ref key-schedule 1)
                          (bytevector-u32-ref in (+ in-start 4) 'big))
             (bitwise-xor (vector-ref key-schedule (+ i 1))
                          (vector-ref te0 (byte a1 3))
                          (vector-ref te1 (byte a2 2))
                          (vector-ref te2 (byte a3 1))
                          (vector-ref te3 (byte a0 0))))
         (a2 (bitwise-xor (vector-ref key-schedule 2)
                          (bytevector-u32-ref in (+ in-start 8) 'big))
             (bitwise-xor (vector-ref key-schedule (+ i 2))
                          (vector-ref te0 (byte a2 3))
                          (vector-ref te1 (byte a3 2))
                          (vector-ref te2 (byte a0 1))
                          (vector-ref te3 (byte a1 0))))
         (a3 (bitwise-xor (vector-ref key-schedule 3)
                          (bytevector-u32-ref in (+ in-start 12) 'big))
             (bitwise-xor (vector-ref key-schedule (+ i 3))
                          (vector-ref te0 (byte a3 3))
                          (vector-ref te1 (byte a0 2))
                          (vector-ref te2 (byte a1 1))
                          (vector-ref te3 (byte a2 0)))))
        ((= i (- len 4))
         ;; Finally do a round of SubBytes, ShiftRows and AddRoundKey.
         (bytevector-u32-set! out out-start
                              (bitwise-xor
                               (vector-ref key-schedule i)
                               (copy-byte (vector-ref te4 (byte a0 3)) 3)
                               (copy-byte (vector-ref te4 (byte a1 2)) 2)
                               (copy-byte (vector-ref te4 (byte a2 1)) 1)
                               (copy-byte (vector-ref te4 (byte a3 0)) 0))
                              'big)
         (bytevector-u32-set! out (+ out-start 4)
                              (bitwise-xor
                               (vector-ref key-schedule (+ i 1))
                               (copy-byte (vector-ref te4 (byte a1 3)) 3)
                               (copy-byte (vector-ref te4 (byte a2 2)) 2)
                               (copy-byte (vector-ref te4 (byte a3 1)) 1)
                               (copy-byte (vector-ref te4 (byte a0 0)) 0))
                              'big)
         (bytevector-u32-set! out (+ out-start 8)
                              (bitwise-xor
                               (vector-ref key-schedule (+ i 2))
                               (copy-byte (vector-ref te4 (byte a2 3)) 3)
                               (copy-byte (vector-ref te4 (byte a3 2)) 2)
                               (copy-byte (vector-ref te4 (byte a0 1)) 1)
                               (copy-byte (vector-ref te4 (byte a1 0)) 0))
                              'big)
         (bytevector-u32-set! out (+ out-start 12)
                              (bitwise-xor
                               (vector-ref key-schedule (+ i 3))
                               (copy-byte (vector-ref te4 (byte a3 3)) 3)
                               (copy-byte (vector-ref te4 (byte a0 2)) 2)
                               (copy-byte (vector-ref te4 (byte a1 1)) 1)
                               (copy-byte (vector-ref te4 (byte a2 0)) 0))
                              'big))))


;;; Deciphering

  (define (reverse-aes-schedule key)
    ;; Reverse the key schedule, then do InvMixColumns
    (do ((ret (list->vector             ;XXX: spills key material as garbage
               (concatenate (reverse (uncat (vector->list key) 4)))))
         (i 4 (+ i 1)))
        ((= i (- (vector-length ret) 4))
         ret)
      (let ((temp (vector-ref ret i)))
        (vector-set! ret i
                     (bitwise-xor
                      (vector-ref td0 (copy-byte (vector-ref te4 (byte temp 3)) 0))
                      (vector-ref td1 (copy-byte (vector-ref te4 (byte temp 2)) 0))
                      (vector-ref td2 (copy-byte (vector-ref te4 (byte temp 1)) 0))
                      (vector-ref td3 (copy-byte (vector-ref te4 (byte temp 0)) 0)))))))

  (define (aes-decrypt! in in-start out out-start key-schedule)
    ;; First add the first round key. Then do n-1 rounds of
    ;; InvSubBytes, InvShiftRows, InvMixColumns and AddRoundKey.
    (do ((len (vector-length key-schedule))
         (i 4 (+ i 4))
         (a0 (bitwise-xor (vector-ref key-schedule 0)
                          (bytevector-u32-ref in in-start 'big))
             (bitwise-xor (vector-ref key-schedule i)
                          (vector-ref td0 (byte a0 3))
                          (vector-ref td1 (byte a3 2))
                          (vector-ref td2 (byte a2 1))
                          (vector-ref td3 (byte a1 0))))
         (a1 (bitwise-xor (vector-ref key-schedule 1)
                          (bytevector-u32-ref in (+ in-start 4) 'big))
             (bitwise-xor (vector-ref key-schedule (+ i 1))
                          (vector-ref td0 (byte a1 3))
                          (vector-ref td1 (byte a0 2))
                          (vector-ref td2 (byte a3 1))
                          (vector-ref td3 (byte a2 0))))
         (a2 (bitwise-xor (vector-ref key-schedule 2)
                          (bytevector-u32-ref in (+ in-start 8) 'big))
             (bitwise-xor (vector-ref key-schedule (+ i 2))
                          (vector-ref td0 (byte a2 3))
                          (vector-ref td1 (byte a1 2))
                          (vector-ref td2 (byte a0 1))
                          (vector-ref td3 (byte a3 0))))
         (a3 (bitwise-xor (vector-ref key-schedule 3)
                          (bytevector-u32-ref in (+ in-start 12) 'big))
             (bitwise-xor (vector-ref key-schedule (+ i 3))
                          (vector-ref td0 (byte a3 3))
                          (vector-ref td1 (byte a2 2))
                          (vector-ref td2 (byte a1 1))
                          (vector-ref td3 (byte a0 0)))))
        ((= i (- len 4))
         ;; Finally do a round of InvSubBytes, InvShiftRows and AddRoundKey.
         (bytevector-u32-set! out out-start
                              (bitwise-xor
                               (vector-ref key-schedule i)
                               (copy-byte (vector-ref td4 (byte a0 3)) 3)
                               (copy-byte (vector-ref td4 (byte a3 2)) 2)
                               (copy-byte (vector-ref td4 (byte a2 1)) 1)
                               (copy-byte (vector-ref td4 (byte a1 0)) 0))
                              'big)
         (bytevector-u32-set! out (+ out-start 4)
                              (bitwise-xor
                               (vector-ref key-schedule (+ i 1))
                               (copy-byte (vector-ref td4 (byte a1 3)) 3)
                               (copy-byte (vector-ref td4 (byte a0 2)) 2)
                               (copy-byte (vector-ref td4 (byte a3 1)) 1)
                               (copy-byte (vector-ref td4 (byte a2 0)) 0))
                              'big)
         (bytevector-u32-set! out (+ out-start 8)
                              (bitwise-xor
                               (vector-ref key-schedule (+ i 2))
                               (copy-byte (vector-ref td4 (byte a2 3)) 3)
                               (copy-byte (vector-ref td4 (byte a1 2)) 2)
                               (copy-byte (vector-ref td4 (byte a0 1)) 1)
                               (copy-byte (vector-ref td4 (byte a3 0)) 0))
                              'big)
         (bytevector-u32-set! out (+ out-start 12)
                              (bitwise-xor
                               (vector-ref key-schedule (+ i 3))
                               (copy-byte (vector-ref td4 (byte a3 3)) 3)
                               (copy-byte (vector-ref td4 (byte a2 2)) 2)
                               (copy-byte (vector-ref td4 (byte a1 1)) 1)
                               (copy-byte (vector-ref td4 (byte a0 0)) 0))
                              'big))))

;;;

  (define (clear-aes-schedule! sched)
    (vector-fill! sched 0))

;;; CTR mode

  (define (aes-ctr! source source-start target target-start len sched ctr)
    (do ((block (make-bytevector 16))
         ;; XXX: ctr should wrap at 2^128-1. Will it *ever* wrap? Stay
         ;; tuned to find out!
         (ctr ctr (+ ctr 1))
         (s source-start (+ s 16))
         (t target-start (+ t 16)))
        ((>= s (+ source-start len))
         ctr)
      (bytevector-uint-set! block 0 ctr 'big 16)
      (aes-encrypt! block 0 block 0 sched)
      (do ((end (min (+ s 16) (+ source-start len)))
           (i 0 (+ i 1))
           (s s (+ s 1))
           (t t (+ t 1)))
          ((= s end))
        (bytevector-u8-set! target t (bitwise-xor (bytevector-u8-ref block i)
                                            (bytevector-u8-ref source s))))))

;;; CBC mode

  (define (aes-cbc-encrypt! source source-start target target-start len sched iv)
    (unless (zero? (bitwise-and len 15))
      (error 'aes-cbc-encrypt!
             "The length has to be an integer multiple of 16" len))
    (do ((ss source-start (+ ss 16))
         (ts target-start (+ ts 16))
         (len len (- len 16)))
        ((< len 16))
      (do ((i 0 (+ i 1)))
          ((= i 16))
        (bytevector-u8-set! iv i
                            (bitwise-xor (bytevector-u8-ref iv i)
                                   (bytevector-u8-ref source (+ ss i)))))
      (aes-encrypt! iv 0 target ts sched)
      (bytevector-copy! iv 0 target ts (+ ts 16))))

  (define (aes-cbc-decrypt! source source-start target target-start len sched iv)
    (unless (zero? (bitwise-and len 15))
      (error 'aes-cbc-decrypt!
             "The length has to be an integer multiple of 16" len))
    (do ((buf (make-bytevector 16))
         (ss source-start (+ ss 16))
         (ts target-start (+ ts 16))
         (len len (- len 16)))
        ((< len 16))
      (aes-decrypt! source ss buf 0 sched)
      (do ((i 0 (+ i 1)))
          ((= i 16))
        (bytevector-u8-set! buf i
                            (bitwise-xor (bytevector-u8-ref iv i)
                                         (bytevector-u8-ref buf i))))
      (bytevector-copy! iv 0 source ss (+ ss 16))
      (bytevector-copy! target ts buf 0 16)))

  ))
