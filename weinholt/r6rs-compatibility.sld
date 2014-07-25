(define-library (weinholt r6rs-compatibility)
  (export
   assert
   bitwise-length
   utf16->string
   for-all
   exists
   bitwise-bit-set?
   bitwise-bit-field

   bytevector-fill!

   native-endianness

   bytevector-ieee-single-native-ref
   bytevector-ieee-single-ref
   bytevector-ieee-double-native-ref
   bytevector-ieee-double-ref
   bytevector-ieee-single-native-set!
   bytevector-ieee-single-set!
   bytevector-ieee-double-native-set!
   bytevector-ieee-double-set!

   ;; bytevector-u8-ref
   bytevector-s8-ref
   bytevector-u8-native-ref
   bytevector-s8-native-ref
   ;; bytevector-u8-set!
   bytevector-s8-set!
   bytevector-u8-native-set!
   bytevector-s8-native-set!

   bytevector-u16-ref
   bytevector-s16-ref
   bytevector-u16-native-ref
   bytevector-s16-native-ref
   bytevector-u16-set!
   bytevector-s16-set!
   bytevector-u16-native-set!
   bytevector-s16-native-set!

   bytevector-u32-ref
   bytevector-s32-ref
   bytevector-u32-native-ref
   bytevector-s32-native-ref
   bytevector-u32-set!
   bytevector-s32-set!
   bytevector-u32-native-set!
   bytevector-s32-native-set!

   bytevector-u64-ref
   bytevector-s64-ref
   bytevector-u64-native-ref
   bytevector-s64-native-ref
   bytevector-u64-set!
   bytevector-s64-set!
   bytevector-u64-native-set!
   bytevector-s64-native-set!

   bytevector-uint-ref
   bytevector-sint-ref
   bytevector-uint-set!
   bytevector-sint-set!

   call-with-bytevector-output-port
   call-with-string-output-port

   bitwise-bit-field
   bitwise-bit-count
   bitwise-reverse-bit-field
   bitwise-rotate-bit-field

   fxarithmetic-shift-left
   fxarithmetic-shift-right
   bitwise-arithmetic-shift-left
   bitwise-arithmetic-shift-right
   fx+
   fx-
   fx*
   fx=?
   fx>=?
   fxbit-count
   fxbit-field
   fxior
   fxand
   )
  (import (scheme base)
          (scheme write)
          (srfi 60))

  (cond-expand
   (chicken
    ;; without this, chicken has trouble with large numbers near the
    ;; fixnum limit -- for example 9223372036854775808
    (import (numbers)))
   (else))

  (begin

    (define-syntax assert
      (syntax-rules ()
        ((_ e)
         (if (not e)
             (error "Assertion failed" `e e)))))

    (define (bitwise-length i)
      (do ((result 0 (+ result 1))
           (bits (if (negative? i)
                     (bitwise-not i)
                     i)
                 (arithmetic-shift bits -1)))
          ((zero? bits)
           result)))


    (define (utf16->string bv endian)
      (error "write utf16->string"))


    ;; from sagittarius
    (define (list-transpose+ . rest)
      (let ((len (length (car rest))))
        (let loop ((i 0)
                   (rest rest)
                   (r '()))
          (if (= i len)
              (reverse r)
              (loop (+ i 1)
                    (let loop ((r '())
                               (p rest))
                      (if (null? p)
                          (reverse r)
                          (loop (cons (cdr (car p)) r) (cdr p))))
                    (cons
                     (let loop ((r '())
                                (p rest))
                       (if (null? p)
                           (reverse r)
                           (loop (cons (car (car p)) r) (cdr p))))
                     r))))))


    ;; from sagittarius
    (define (for-all pred lst1 . lst2)
      (define (for-all-n pred list-of-lists)
        (let ((argc (length list-of-lists)))
          (define (collect-cdr lst)
            (let loop ((lst lst))
              (cond ((null? lst) '())
                    ((null? (cdar lst)) (loop (cdr lst)))
                    (else (cons (cdar lst) (loop (cdr lst)))))))
          (define (collect-car lst)
            (let loop ((lst lst))
              (cond ((null? lst) '())
                    ((pair? (car lst))
                     (cons (caar lst) (loop (cdr lst))))
                    (else
                     (error "for-all -- traversal reached to non-pair element."
                            (car lst))))))

          (let loop ((head (collect-car list-of-lists)) (rest (collect-cdr list-of-lists)))
            (or (= (length head) argc)
                (error "for-all -- expected same length chains of pairs" list-of-lists))
            (if (null? rest)
                (apply pred head)
                (and (apply pred head)
                     (loop (collect-car rest) (collect-cdr rest)))))))

      (define (for-all-n-quick pred lst)
        (or (null? lst)
            (let loop ((head (car lst)) (rest (cdr lst)))
              (if (null? rest)
                  (apply pred head)
                  (and (apply pred head)
                       (loop (car rest) (cdr rest)))))))

      (define (for-all-1 pred lst)
        (cond ((null? lst) #t)
              ((pair? lst)
               (let loop ((head (car lst)) (rest (cdr lst)))
                 (cond ((null? rest) (pred head))
                       ((pair? rest)
                        (and (pred head)
                             (loop (car rest) (cdr rest))))
                       (else
                        (and (pred head)
                             (error "for-all -- traversal reached to non-pair element" rest))))))
              (else
               (error "for-all -- expected chain of pairs" (list pred lst)))))

      (cond ((null? lst2)
             (for-all-1 pred lst1))
            ((apply list-transpose+ lst1 lst2)
             => (lambda (lst) (for-all-n-quick pred lst)))
            (else
             (for-all-n pred (cons lst1 lst2)))))

    ;; from sagittarius
    (define (exists pred lst1 . lst2)
      (define (exists-1 pred lst)
        (cond ((null? lst) #f)
              ((pair? lst)
               (let loop ((head (car lst)) (rest (cdr lst)))
                 (cond ((null? rest) (pred head))
                       ((pred head))
                       ((pair? rest) (loop (car rest) (cdr rest)))
                       (else
                        (error "exists -- traversal reached to non-pair element"
                               rest)))))
              (else
               (error "exists -- expected chain of pairs." (list pred lst)))))
      (define (exists-n-quick pred lst)
        (and (pair? lst)
             (let loop ((head (car lst)) (rest (cdr lst)))
               (if (null? rest)
                   (apply pred head)
                   (or (apply pred head)
                       (loop (car rest) (cdr rest)))))))
      (define (exists-n pred list-of-lists)
        (let ((argc (length list-of-lists)))
          (define (collect-cdr lst)
            (let loop ((lst lst))
              (cond ((null? lst) '())
                    ((null? (cdar lst)) (loop (cdr lst)))
                    (else (cons (cdar lst) (loop (cdr lst)))))))
          (define (collect-car lst)
            (let loop ((lst lst))
              (cond ((null? lst) '())
                    ((pair? (car lst))
                     (cons (caar lst) (loop (cdr lst))))
                    (else
                     (error "exists -- traversal reached to non-pair"
                            (car lst))))))

          (let loop ((head (collect-car list-of-lists)) (rest (collect-cdr list-of-lists)))
            (or (= (length head) argc)
                (error "exists -- expected same length chains of pairs"
                       list-of-lists))
            (if (null? rest)
                (apply pred head)
                (or (apply pred head)
                    (loop (collect-car rest) (collect-cdr rest)))))))
      (cond ((null? lst2)
             (exists-1 pred lst1))
            ((apply list-transpose+ lst1 lst2)
             => (lambda (lst) (exists-n-quick pred lst)))
            (else
             (exists-n pred (cons lst1 lst2)))))


    (define (bitwise-bit-set? n i)
      (> (bitwise-and (arithmetic-shift n (- i)) 1) 0))


    (define (bitwise-bit-field ei1 ei2 ei3)
      (let ((mask (bitwise-not (arithmetic-shift -1 ei3))))
        (arithmetic-shift
         (bitwise-and ei1 mask)
         (- ei2))))


    (define (bytevector-fill! bv n)
      (let loop ((i 0))
        (cond ((= i (bytevector-length bv)) bv)
              (else
               (bytevector-u8-set! bv i n)
               (loop (+ i 1))))))


    (define (native-endianness) 'big)

    (define (ieee-754->number bits)
      (cond ;; scheme2js can't parse the +inf.0
       ;; ((= bits #x7f800000) +inf.0)
       ;; ((= bits #xff800000) -inf.0)
       ;; ((= (bitwise-and bits #xff800000) #xff800000) -nan.0)
       ;; ((= (bitwise-and bits #x7f800000) #x7f800000) +nan.0)
       (else
        (let* ((sign-bit (arithmetic-shift (bitwise-and bits #x80000000) -31))
               (exponent (arithmetic-shift (bitwise-and bits #x7f800000) -23))
               (fraction (bitwise-and bits #x007fffff))
               (fraction-as-number
                (let loop ((i #x00400000)
                           (v (/ 1.0 2.0))
                           (result 0))
                  (if (= i 0)
                      (if (= exponent 0) result (+ 1.0 result))
                      (loop (arithmetic-shift i -1)
                            (/ v 2.0)
                            (if (> (bitwise-and fraction i) 0)
                                (+ result v)
                                result))))))
          (* (if (= sign-bit 0) 1.0 -1.0)
             (expt 2 (- exponent 127)) fraction-as-number)))))


    (define (number->ieee-754 f32)
      (cond ;; ((eqv? f32 +inf.0) #x7f800000)
       ;; ((eqv? f32 -inf.0) #xff800000)
       ;; ((eqv? f32 +nan.0) #x7f800001)
       ((eqv? f32 0) #x00000000)
       ((eqv? f32 0.0) #x00000000)
       (else
        (let* ((sign-bit (if (< f32 0) 1 0))
               (f32 (if (< f32 0) (- f32) f32))
               )
          (let loop ((f32-shifted f32)
                     (exponent 0))
            (cond ((< f32-shifted 1.0)
                   (loop (* f32-shifted 2.0) (- exponent 1)))
                  ((>= f32-shifted 2.0)
                   (loop (/ f32-shifted 2.0) (+ exponent 1)))
                  (else
                   (let loop ((fraction (- f32-shifted 1.0))
                              (fraction-bits 0)
                              (pow2 #x400000))
                     (if (> pow2 0)
                         (cond ((>= (* fraction 2.0) 1.0)
                                (loop (- (* fraction 2.0) 1.0)
                                      (bitwise-ior pow2 fraction-bits)
                                      (arithmetic-shift pow2 -1)))
                               (else
                                (loop (* fraction 2.0)
                                      fraction-bits
                                      (arithmetic-shift pow2 -1))))
                         ;; done
                         (begin
                           ;; (cout "sign="
                           ;;       (number->string sign-bit 2) "\n")
                           ;; (cout "exponent="
                           ;;       (number->string (+ exponent 127) 2) "\n")
                           ;; (cout "fraction="
                           ;;       (number->string fraction-bits 2) "\n")
                           (bitwise-ior
                            (arithmetic-shift sign-bit 31)
                            (arithmetic-shift (+ exponent 127) 23)
                            fraction-bits))
                         )))))))))


    (define (bytevector-ieee-single-native-ref bv k)
      (bytevector-ieee-single-ref bv k (native-endianness)))

    (define (bytevector-ieee-single-ref bv k endianness)
      (ieee-754->number (bytevector-u32-ref bv k endianness)))

    (define (bytevector-ieee-double-native-ref bv k)
      (bytevector-ieee-double-ref bv k (native-endianness)))

    (define (bytevector-ieee-double-ref bv k endianness)
      ;; XXX ieee-754->number isn't right
      (ieee-754->number (bytevector-u64-ref bv k endianness)))


    (define (bytevector-ieee-single-native-set! bv k x)
      (bytevector-ieee-single-set! bv k x (native-endianness)))

    (define (bytevector-ieee-single-set! bv k n endianness)
      (bytevector-u32-set! bv k (number->ieee-754 n) endianness))

    (define (bytevector-ieee-double-native-set! bv k x)
      (bytevector-ieee-double-set! bv k x (native-endianness)))

    (define (bytevector-ieee-double-set! bv k n endianness)
      ;; XXX ieee-754->number isn't right
      (bytevector-u64-set! bv k (number->ieee-754 n) endianness))



    ;; (define (bytevector-u8-ref bv k endianness)
    ;;   (error "write bytevector-u8-ref"))

    (define (bytevector-s8-ref bv k)
      (let ((n (bytevector-u8-ref bv k)))
        (if (= (bitwise-and n #x80) 0)
            n
            (- (+ (bitwise-and (bitwise-not n) #xff) 1)))))

    (define (bytevector-u8-native-ref bv k)
      (bytevector-u8-ref bv k))

    (define (bytevector-s8-native-ref bv k)
      (bytevector-s8-ref bv k))

    ;; (define (bytevector-u8-set! bv k n)
    ;;   (error "write bytevector-u8-set!"))

    (define (bytevector-s8-set! bv k n)
      (bytevector-u8-set!
       bv k (if (>= n 0)
                n
                (+ (bitwise-and (bitwise-not (- n)) #xff) 1))))

    (define (bytevector-u8-native-set! bv k n)
      (bytevector-u8-set! bv k n))

    (define (bytevector-s8-native-set! bv k n)
      (bytevector-s8-set! bv k n))



    (define (bytevector-u16-ref bv k endianness)
      (case endianness
        ((big)
         (bitwise-ior (arithmetic-shift (bytevector-u8-ref bv k) 8)
                      (bytevector-u8-ref bv (+ k 1))))
        ((little)
         (bitwise-ior
          (bytevector-u8-ref bv k)
          (arithmetic-shift (bytevector-u8-ref bv (+ k 1)) 8)))))

    (define (bytevector-s16-ref bv k endianness)
      (let ((n (bytevector-u16-ref bv k endianness)))
        (if (= (bitwise-and n #x8000) 0)
            n
            (- (+ (bitwise-and (bitwise-not n) #xffff) 1)))))

    (define (bytevector-u16-native-ref bv k)
      (bytevector-u16-ref bv k (native-endianness)))

    (define (bytevector-s16-native-ref bv k)
      (bytevector-s16-ref bv k (native-endianness)))

    (define (bytevector-u16-set! bv k n endianness)
      (case endianness
        ((big)
         (bytevector-u8-set! bv k (bitwise-and (arithmetic-shift n -8) #xff))
         (bytevector-u8-set! bv (+ k 1) (bitwise-and n #xff)))
        ((little)
         (bytevector-u8-set! bv k (bitwise-and n #xff))
         (bytevector-u8-set!
          bv (+ k 1) (bitwise-and (arithmetic-shift n -8) #xff)))))

    (define (bytevector-s16-set! bv k n endianness)
      (bytevector-u16-set!
       bv k (if (>= n 0)
                n
                (+ (bitwise-and (bitwise-not (- n)) #xffff) 1))
       endianness))

    (define (bytevector-u16-native-set! bv k n)
      (bytevector-u16-set! bv k n (native-endianness)))

    (define (bytevector-s16-native-set! bv k n)
      (bytevector-s16-set! bv k n (native-endianness)))


    (define (bytevector-u32-ref bv k endianness)
      (case endianness
        ((big)
         (bitwise-ior
          (arithmetic-shift (bytevector-u16-ref bv k 'big) 16)
          (bytevector-u16-ref bv (+ k 2) 'big)))
        ((little)
         (bitwise-ior
          (bytevector-u16-ref bv k 'little)
          (arithmetic-shift (bytevector-u16-ref bv (+ k 2) 'little) 16)))))

    (define (bytevector-s32-ref bv k endianness)
      (let ((n (bytevector-u32-ref bv k endianness)))
        (if (= (bitwise-and n #x80000000) 0)
            n
            (- (+ (bitwise-and (bitwise-not n) #xffffffff) 1)))))


    (define (bytevector-u32-native-ref bv k)
      (bytevector-u32-ref bv k (native-endianness)))

    (define (bytevector-s32-native-ref bv k)
      (bytevector-s32-ref bv k (native-endianness)))

    (define (bytevector-u32-set! bv k n endianness)
      (case endianness
        ((big)
         (bytevector-u16-set!
          bv k (bitwise-and (arithmetic-shift n -16) #xffff) 'big)
         (bytevector-u16-set! bv (+ k 2) (bitwise-and n #xffff) 'big))
        ((little)
         (bytevector-u16-set! bv k (bitwise-and n #xffff) 'little)
         (bytevector-u16-set!
          bv (+ k 2) (bitwise-and (arithmetic-shift n -16) #xffff) 'little)))
      )

    (define (bytevector-s32-set! bv k n endianness)
      (bytevector-u32-set!
       bv k (if (>= n 0)
                n
                (+ (bitwise-and (bitwise-not (- n)) #xffffffff) 1))
       endianness))


    (define (bytevector-u32-native-set! bv k n)
      (bytevector-u32-set! bv k n (native-endianness)))

    (define (bytevector-s32-native-set! bv k n)
      (bytevector-s32-set! bv k n (native-endianness)))



    (define (bytevector-u64-ref bv k endianness)
      (case endianness
        ((big)
         (bitwise-ior
          (arithmetic-shift (bytevector-u32-ref bv k 'big) 32)
          (bytevector-u32-ref bv (+ k 4) 'big)))
        ((little)
         (bitwise-ior
          (bytevector-u32-ref bv k 'little)
          (arithmetic-shift (bytevector-u32-ref bv (+ k 4) 'little) 32)))))

    (define (bytevector-s64-ref bv k endianness)
      (let ((n (bytevector-u64-ref bv k endianness)))
        (if (= (bitwise-and n #x8000000000000000) 0)
            n
            (- (+ (bitwise-and (bitwise-not n) #xffffffffffffffff) 1)))))

    (define (bytevector-u64-native-ref bv k)
      (bytevector-u64-ref bv k (native-endianness)))

    (define (bytevector-s64-native-ref bv k)
      (bytevector-s64-ref bv k (native-endianness)))

    (define (bytevector-u64-set! bv k n endianness)
      (case endianness
        ((big)
         (bytevector-u32-set!
          bv k (bitwise-and (arithmetic-shift n -32) #xffffffff) 'big)
         (bytevector-u32-set! bv (+ k 4) (bitwise-and n #xffffffff) 'big))
        ((little)
         (bytevector-u32-set! bv k (bitwise-and n #xffffffff) 'little)
         (bytevector-u32-set!
          bv (+ k 4) (bitwise-and (arithmetic-shift n -32)
                                  #xffffffff) 'little))))

    (define (bytevector-s64-set! bv k n endianness)
      (bytevector-u64-set!
       bv k (if (>= n 0)
                n
                (+ (bitwise-and (bitwise-not (- n)) #xffffffffffffffff) 1))
       endianness))

    (define (bytevector-u64-native-set! bv k n)
      (bytevector-u64-set! bv k n (native-endianness)))

    (define (bytevector-s64-native-set! bv k n)
      (bytevector-s64-set! bv k n (native-endianness)))


    ;; from sagittarius
    (define (bytevector-uint-ref bv index endien size)
      (cond ((eq? endien 'big)
             (let ((end (+ index size)))
               (let loop ((i index) (acc 0))
                 (if (>= i end)
                     acc
                     (loop (+ i 1)
                           (+ (* 256 acc) (bytevector-u8-ref bv i)))))))
            ((eq? endien 'little)
             (let loop ((i (+ index size -1)) (acc 0))
               (if (< i index)
                   acc
                   (loop (- i 1) (+ (* 256 acc) (bytevector-u8-ref bv i))))))
            (else
             (error 'bytevector-uint-ref "expected endianness" endien))))


    ;; from sagittarius
    (define (bytevector-sint-ref bv index endien size)
      (cond ((eq? endien 'big)
             (if (> (bytevector-u8-ref bv index) 127)
                 (- (bytevector-uint-ref bv index endien size) (expt 256 size))
                 (bytevector-uint-ref bv index endien size)))
            ((eq? endien 'little)
             (if (> (bytevector-u8-ref bv (+ index size -1)) 127)
                 (- (bytevector-uint-ref bv index endien size) (expt 256 size))
                 (bytevector-uint-ref bv index endien size)))
            (else
             (error 'bytevector-uint-ref "expected endianness" endien))))


    ;; from sagittarius
    (define (bytevector-uint-set! bv index val endien size)
      (cond ((= val 0)
             (let ((end (+ index size)))
               (let loop ((i index))
                 (cond ((>= i end) #t)
                       (else
                        (bytevector-u8-set! bv i 0)
                        (loop (+ i 1)))))))
            ((< 0 val (expt 256 size))
             (cond ((eq? endien 'big)
                    (let ((start (- (+ index size) 1)))
                      (let loop ((i start) (acc val))
                        (cond ((< i index) #t)
                              (else
                               ;; mod256 -> bitwise-and
                               (bytevector-u8-set! bv i (bitwise-and acc 255))
                               ;; div256 -> bitwise-arithmetic-shift
                               (loop (- i 1) (arithmetic-shift acc -8)))))))
                   ((eq? endien 'little)
                    (let ((end (+ index size)))
                      (let loop ((i index) (acc val))
                        (cond ((>= i end) #t)
                              (else
                               ;; mod256 -> bitwise-and
                               (bytevector-u8-set! bv i (bitwise-and acc 255))
                               ;; div256 -> bitwise-arithmetic-shift
                               (loop (+ i 1) (arithmetic-shift acc -8)))))))))
            (else
             (error 'bytevector-uint-set! "value out of range" val))))


    ;; from sagittarius
    (define (bytevector-sint-set! bv index val endien size)
      (let* ((p-bound (expt 2 (- (* size 8) 1)))
             (n-bound (- (+ p-bound 1))))
        (if (< n-bound val p-bound)
            (if (> val 0)
                (bytevector-uint-set! bv index val endien size)
                (bytevector-uint-set! bv index (+ val (expt 256 size))
                                      endien size))
            (error 'bytevector-sint-set! "value out of range" val))))



    (define (call-with-bytevector-output-port func)
      (let ((out-bv (open-output-bytevector)))
        (func out-bv)
        (get-output-bytevector out-bv)))


    (define (call-with-string-output-port func)
      (let ((out-bv (open-output-string)))
        (func out-bv)
        (get-output-string out-bv)))


    (define (bitwise-reverse-bit-field v start end)
      (do ((i start (+ i 1))
           (ret 0 (if (bitwise-bit-set? v i)
                      (bitwise-ior
                       ret (arithmetic-shift 1 (- end i 1)))
                      ret)))
          ((= i end)
           (bitwise-ior (arithmetic-shift ret start)
                        (copy-bit-field v 0 start end)))))


    (define (bitwise-rotate-bit-field n start end count)
      (let ((width (- end start)))
        (if (positive? width)
            (let* ((count (modulo count width))
                   (field0 (bitwise-bit-field n start end))
                   (field1 (arithmetic-shift field0 count))
                   (field2 (arithmetic-shift field0 (- width count)))
                   (field (bitwise-ior field1 field2)))
              (copy-bit-field n start end field))
            n)))


    (define (bitwise-bit-count ei)
      (cond ((>= ei 0)
             (let loop ((ei ei)
                        (result 0))
               (if (= ei 0) result
                   (loop (arithmetic-shift ei -1)
                         (if (> (bitwise-and ei 1) 0)
                             (+ result 1)
                             result)))))
            (else
             (bitwise-not (bitwise-bit-count (bitwise-not ei))))))

    (define (bitwise-bit-field x1 x2 x3)
      (let ((mask (bitwise-not (arithmetic-shift -1 x3))))
        (arithmetic-shift (bitwise-and x1 mask) (- x2))))


    (define (fxarithmetic-shift-left v n)
      (cond ((>= n 32) 0)
            ((< n -32) 0)
            (else
             (bitwise-and #xffffffffffffffff
                          (arithmetic-shift (bitwise-and #xffffffffffffffff v) n)))))


    (define (fxarithmetic-shift-right v n)
      (fxarithmetic-shift-left v (- n)))


    (define (bitwise-arithmetic-shift-left v n)
      (arithmetic-shift v n))


    (define (bitwise-arithmetic-shift-right v n)
      (arithmetic-shift v (- n)))

    (define (fx+ a b)
      (bitwise-and #xffffffffffffffff (+ a b)))

    (define (fx- a b)
      (bitwise-and #xffffffffffffffff (- a b)))

    (define (fx* a b)
      (bitwise-and #xffffffffffffffff (* a b)))

    (define (fx=? a b)
      (= a b))

    (define (fx>=? a b)
      (>= a b))

    (define (fxbit-count v)
      (bitwise-bit-count v))

    (define (fxbit-field v a b)
      (bitwise-bit-field (bitwise-and #xffffffffffffffff v) a b))

    (define (fxior a b)
      (bitwise-ior
       (bitwise-and #xffffffffffffffff a)
       (bitwise-and #xffffffffffffffff b)))

    (define (fxand a b)
      (bitwise-and
       (bitwise-and #xffffffffffffffff a)
       (bitwise-and #xffffffffffffffff b)))

    ))
