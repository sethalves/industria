
(define (main-program)

  (and

   (= (bitwise-length 9) 4)
   (= (bytevector-uint-ref (bytevector 1 2 3 4 5) 1 'big 2) 515)
   (= (bytevector-uint-ref (bytevector 1 2 3 4 5) 1 'big 3) 131844)
   (= (bytevector-uint-ref (bytevector 1 2 3 4 5) 1 'little 3) 262914)

   (for-all = '(1 2 3) '(1 2 3))
   (for-all equal? '(1 2 3) '(1 2 3))

   (exists = '(2 1 3) '(4 1 6))

   (bitwise-bit-set? 400 4)
   (not (bitwise-bit-set? 400 3))

   (= (bitwise-bit-field 255 3 6) 7)

   (let ((bv (make-bytevector 5 0)))
     (bytevector-uint-set! bv 2 5334 'big 2)
     (write (bytevector-u16-native-ref bv 2))
     (newline)
     (or (= (bytevector-u16-native-ref bv 2) 5334)
         (= (bytevector-u16-native-ref bv 2) 54804)))


   (= (bitwise-reverse-bit-field 3456 0 64) 121597189939003392)


   #t))
