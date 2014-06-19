
(define (main-program)

  (and

   (= (bytevectors-length (list (bytevector 1 2 3) (bytevector 1 2 3))) 6)

   (= 453543 (bytevector->uint (uint->bytevector 453543)))

   #t))
