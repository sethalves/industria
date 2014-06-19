;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2010 Göran Weinholt <goran@weinholt.se>

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

;; (define (SubjectAltName)
;;   `(sequence-of 1 +inf.0 ,(GeneralName)))

;; (define (GeneralName)
;;   `(choice #;(otherName (implicit context 0 ,(OtherName)))
;;            (rfc822Name (implicit context 1 ia5-string))
;;            (dNSName (implicit context 2 ia5-string))
;;            #;etc...))

;; (define (main-program)

;;   (display "-----------\n")
;;   (write (der:translate
;;           (der:decode (bytevector 48 30 130 15 119 119 119 46 119 101
;;                                   105 110 104 111 108 116 46 115 101
;;                                   130 11 119 101 105 110 104 111 108
;;                                   116 46 115 101))
;;           (SubjectAltName)))
;;   (newline)
;;   (display "-----------\n")


;;   (check
;;    (der:translate (der:decode (bytevector 48 30 130 15 119 119 119 46 119 101
;;                                           105 110 104 111 108 116 46 115 101
;;                                           130 11 119 101 105 110 104 111 108
;;                                           116 46 115 101))
;;                   (SubjectAltName))
;;    => '("www.weinholt.se" "weinholt.se"))

;;   ;; TODO: needs more tests, to say the least.

;;   (check-report)
;;   #t)





;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2008, 2009, 2010, 2011 Göran Weinholt <goran@weinholt.se>

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

;; Test suite for the (weinholt struct pack) library.

;; This program also tests the host implementation's bytevector
;; procedures quite thoroughly, as it turns out.


(define x
  (random-source-randomize! default-random-source))

(define (check-pack expect fmt . values)
  (display "\nFormat: ") (write fmt)
  (display "\nValues: ") (write values) (newline)
  (let ()
    (check (apply pack fmt values) => expect)
    (check (eval `(pack ,fmt ,@values)
                 (environment '(scheme base) '(weinholt struct pack))
                 ;; (interaction-environment)
                 )
           => expect)
    (check (call-with-values (lambda () (unpack fmt expect)) list)
           => values)
    (check (eval `(call-with-values (lambda () (unpack ,fmt ',expect)) list)
                 (environment '(scheme base) '(weinholt struct pack))
                 ;; (interaction-environment)
                 )
           => values)
    #f))

(define (print . x) (for-each display x) (newline))

(define (random-integer n set ref)
  (let ((bv (make-bytevector n)))
    (do ((i 0 (+ i 1)))
        ((= i n) (ref bv 0 (native-endianness)))
      (bytevector-u8-set! bv i (random 256)))))

(define (random-float n set ref)
  (let ((v (inexact (/ (random 10000)
                       (+ 1 (random 100)))))
        (bv (make-bytevector n)))
    (set bv 0 v (native-endianness))
    (ref bv 0 (native-endianness))))

(define types
  (vector (vector #\c
                  (lambda (b i e) (bytevector-s8-ref b i))
                  (lambda (b i v e) (bytevector-s8-set! b i v))
                  1 #f)
          (vector #\C
                  (lambda (b i e) (bytevector-u8-ref b i))
                  (lambda (b i v e) (bytevector-u8-set! b i v))
                  1 #f)
          (vector #\s bytevector-s16-ref bytevector-s16-set! 2 #f)
          (vector #\S bytevector-u16-ref bytevector-u16-set! 2 #f)
          (vector #\l bytevector-s32-ref bytevector-s32-set! 4 #f)
          (vector #\L bytevector-u32-ref bytevector-u32-set! 4 #f)
          (vector #\q bytevector-s64-ref bytevector-s64-set! 8 #f)
          (vector #\Q bytevector-u64-ref bytevector-u64-set! 8 #f)
          (vector #\f bytevector-ieee-single-ref bytevector-ieee-single-set! 4 'float)
          (vector #\d bytevector-ieee-double-ref bytevector-ieee-double-set! 8 'float)))

(define (random-test endianness)
  "Construct random format strings and values, alongside a bytevector
that is expected to contain the values encoded correctly according to
the format string. Then see if pack/unpack gives the expected result."
  (let ((p (open-output-bytevector))
        (count (random 7)))
    (let lp ((i 0)
             (align #t)
             (codes (case endianness
                      ((little) '(#\<))
                      ((big) (if (zero? (random 2))
                                 '(#\!) '(#\>)))
                      ((native) '(#\=))))
             (values '())
             (o 0))
      (let ((t (vector-ref types (random (vector-length types)))))
        (if (= i count)
            (let ((fmt (list->string (reverse codes)))
                  (values (reverse values))
                  (bv (get-output-bytevector p)))
              (apply check-pack bv fmt values))
            (let* ((v ((if (eq? (vector-ref t 4) 'float)
                           random-float
                           random-integer)
                       (vector-ref t 3) (vector-ref t 2) (vector-ref t 1)))
                   (padsize (random 5))
                   (pad (make-list padsize #\x))
                   (rep (random 5))
                   (repcode (string->list (number->string rep)))
                   (bv (make-bytevector (vector-ref t 3)))
                   (new-align (if (zero? (random 3)) (not align) align))
                   (no (if align (roundb o (vector-ref t 3)) o)))
              ;; align
              (write-bytevector (make-bytevector (- no o) 0) p)
              ((vector-ref t 2) bv 0 v (if (eq? endianness 'native)
                                           (native-endianness)
                                           endianness))
              (do ((i 0 (+ i 1))
                   (m (if (zero? rep) 1 rep)))
                  ((= i m))
                ;; values
                (write-bytevector bv p))
              ;; "x"
              (write-bytevector (make-bytevector padsize 0) p)
              (cond ((zero? rep)
                     (lp (+ i 1) new-align
                         (append pad
                                 (if (boolean=? align new-align) '()
                                     (if new-align '(#\a) '(#\u)))
                                 (cons (vector-ref t 0) codes))
                         (cons v values)
                         (+ no padsize (vector-ref t 3))))
                    (else
                     (lp (+ i 1) new-align
                         (append pad
                                 (if (boolean=? align new-align) '()
                                     (if new-align '(#\a) '(#\u)))
                                 (cons (vector-ref t 0)
                                       (append repcode codes)))
                         (append (make-list rep v) values)
                         (+ no padsize (* rep (vector-ref t 3))))))))))))


(define (main-program)

  (check (unpack "!C" '#u8(1 2 3 4 6 0) 5) => 0)
  (check (unpack "!S" '#u8(1 2 3 4 0 0) 4) => 0)

  (check (let ((bv (make-bytevector 4 0))) (pack! "!S" bv 2 #xffee) bv)
         => '#u8(0 0 #xff #xee))

  (check (let-values ((x (get-unpack (open-input-bytevector #u8(4 3 2 1 2 1 1 #xff #xff))
                                     "<LSC")))
           x)
         => '(#x01020304 #x0102 #x01))

  (check (apply get-unpack (open-input-bytevector #u8(#xff))
                "c" '())
         => -1)

  (check (unpack "C" #u8(0 1) 1) => 1)

  (check (let ((offset 1)) (unpack "C" #u8(0 1) offset)) => 1)

  (check (let ((offset 1))
           (unpack "!uxxS" #u8(5 5 5 0 1) offset))
         => 1)

  (check (pack "!SS" 1 2) => #u8(0 1 0 2))

  (check (let ((bv (make-bytevector 6 #xff))
               (offset 1))
           (pack! "!SS" bv offset 1 2)
           bv)
         => #u8(#xff 0 0 1 0 2))

  (check (let ((bv (make-bytevector 9 #xff)))
           (pack! "<ucQ" bv (+ 0) 1 2)
           bv)
         => #u8(1 2 0 0 0 0 0 0 0))

  (check (map unpack
              '("C" "!S" "!xxC")
              '(#u8(1) #u8(0 1) #u8(42 42 1)))
         => '(1 1 1))

  (check (map pack
              '("C" "!S" "!xxC")
              '(1 1 1))
         => '(#u8(1) #u8(0 1) #u8(0 0 1)))

  (check (pack "4C" 1 2 3 4)
         =>
         (pack "CCCC" 1 2 3 4))

  (check (pack (car '("4C")) 1 2 3 4)
         =>
         #u8(1 2 3 4))

  (check (let-values ((x (unpack "4C" #u8(1 2 3 4)))) x)
         => '(1 2 3 4))

  (check (let-values ((x (unpack (car '("4C")) #u8(1 2 3 4)))) x)
         => '(1 2 3 4))

  (check (let ((zero 0)
               (bv (make-bytevector 8 1)))
           (pack! "!L" bv (+ zero 2) #xFFFFFFFF)
           bv)
         => #u8(1 1 0 0 255 255 255 255))

  (do ((i 0 (+ i 1)))
      ((= i 50))
    (random-test 'native))

  (do ((i 0 (+ i 1)))
      ((= i 50))
    (random-test 'big))

  (do ((i 0 (+ i 1)))
      ((= i 50))
    (random-test 'little))


  (check-pack '#u8() "")
  (check-pack '#u8(0) "x")
  (check-pack '#u8(0 0 0) "3x")
  (check-pack '#u8() "0x")
  (check-pack '#u8() "!0x")
  (check-pack '#u8(#xff) "c" -1)
  (check-pack '#u8(0 #xff) "xC" 255)

  (check-pack '#u8(0 1 0 0 0 0 0 2 0 0 0 0 0 0 0 3) "!SLQ" 1 2 3)
  (check-pack '#u8(0 0 0 0 0 0 0 1 0 0 0 2 0 3) "!QLS" 1 2 3)
  (check-pack '#u8(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 3) "!SQL" 1 2 3)
  (check-pack '#u8(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 3) ">SQL" 1 2 3)
  (check-pack '#u8(1 0 0 0 0 0 0 0 2 0 0 0 3 0) "<QLS" 1 2 3)

  (check-pack '#u8(4 1 0) "u!C S" 4 #x100)
  (check-pack '#u8(4 0 1 0) "u!CaS" 4 #x100)

  (check-pack '#u8(4 0 0 1 0) "u!C L" 4 #x100)

  (check-pack '#u8(4 0 0 0 0 0 0 1 0) "u!C Q" 4 #x100)

  (check-report)

  )
