;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2012 Göran Weinholt <goran@weinholt.se>

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

;; Describes how the calculations in GF(2⁸) work, more or less:

;; INPROCEEDINGS{Win96afast,
;;     author = {Erik De Win and Antoon Bosselaers and Servaas Vanderberghe and Peter De Gersem and Joos Vandewalle},
;;     title = {A Fast Software Implementation for Arithmetic Operations in GF(2^n)},
;;     booktitle = {},
;;     year = {1996},
;;     pages = {65--76},
;;     publisher = {Springer-Verlag}
;; }

(define-library (weinholt crypto aes private)
  (export S-box inv-S-box GFexpt GF*)
  (import (scheme base)
          (srfi 60)
          (weinholt r6rs-compatibility))

  (begin

  ;; Calculations in GF(2⁸)... all children need to learn their
  ;; GF(2⁸) logarithm tables by heart.
  (define alog
    (do ((alog (make-bytevector 256))
         (p 1 (let ((p (bitwise-xor p (arithmetic-shift p 1))))
                (if (bitwise-bit-set? p 8)
                    (bitwise-xor p #b100011011) ;subtract X⁸+X⁴+X³+X+1
                    p)))
         (i 0 (+ i 1)))
        ((= i 256)
         (lambda (i) (bytevector-u8-ref alog i)))
      (bytevector-u8-set! alog i p)))

  (define ilog                          ;called `log' in [Win96afast]
    (do ((ilog (make-bytevector 256))
         (i 0 (+ i 1)))
        ((= i 256)
         (lambda (i) (bytevector-u8-ref ilog i)))
      (bytevector-u8-set! ilog (alog i) i)))

  (define (GF* a b)
    (if (or (zero? a) (zero? b))
        0
        (alog (modulo (+ (ilog a) (ilog b)) 255))))

  (define (GFexpt a n)
    (if (zero? n) 1
        (GF* a (GFexpt a (- n 1)))))

  (define (GFinv a)
    (if (zero? a)
        0
        (alog (modulo (- (ilog a)) 255))))

  ;; What follows is from Rijndael

  (define (affine-transform b)
    (define (bit x i)
      (bitwise-bit-field x i (+ i 1)))
    (do ((c #b01100011)
         (i 0 (+ i 1))
         (tmp 0 (bitwise-ior (arithmetic-shift
                              (bitwise-xor (bit b i)
                                           (bit b (modulo (+ i 4) 8))
                                           (bit b (modulo (+ i 5) 8))
                                           (bit b (modulo (+ i 6) 8))
                                           (bit b (modulo (+ i 7) 8))
                                           (bit c i))
                              i)
                             tmp)))
        ((= i 8) tmp)))

  (define S-box                         ;for SubBytes
    (do ((S (make-bytevector 256))
         (i 0 (+ i 1)))
        ((= i 256)
         (lambda (i) (bytevector-u8-ref S i)))
      (bytevector-u8-set! S i (affine-transform (GFinv i)))))

  (define inv-S-box                     ;for InvSubBytes
    (do ((invS (make-bytevector 256))
         (i 0 (+ i 1)))
        ((= i 256)
         (lambda (i) (bytevector-u8-ref invS i)))
      (bytevector-u8-set! invS (affine-transform (GFinv i)) i)))

  ))
