;;; Copyright 2022 Wolfgang Corcoran-Mathe
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

(import (chicken condition)
        (srfi 141)
        test)

(define-syntax raises-type-exception
  (syntax-rules ()
    ((raises-type-exception e)
     (condition-case e
       ((exn type) #t)
       (junk () #f)))))

(define-syntax raises-arithmetic-exception
  (syntax-rules ()
    ((raises-arithmetic-exception e)
     (condition-case e
       ((exn arithmetic) #t)
       (junk () #f)))))

(test-group "Exceptions"
  (test #t (raises-type-exception (ceiling/ 1.1 1)))
  (test #t (raises-type-exception (ceiling/ 1 1.1)))
  (test #t (raises-arithmetic-exception (ceiling/ 1 0)))

  (test #t (raises-type-exception (ceiling-quotient 1.1 1)))
  (test #t (raises-type-exception (ceiling-quotient 1 1.1)))
  (test #t (raises-arithmetic-exception (ceiling-quotient 1 0)))

  (test #t (raises-type-exception (ceiling-remainder 1.1 1)))
  (test #t (raises-type-exception (ceiling-remainder 1 1.1)))
  (test #t (raises-arithmetic-exception (ceiling-remainder 1 0)))

  (test #t (raises-type-exception (floor/ 1.1 1)))
  (test #t (raises-type-exception (floor/ 1 1.1)))
  (test #t (raises-arithmetic-exception (floor/ 1 0)))

  (test #t (raises-type-exception (floor-quotient 1.1 1)))
  (test #t (raises-type-exception (floor-quotient 1 1.1)))
  (test #t (raises-arithmetic-exception (floor-quotient 1 0)))

  (test #t (raises-type-exception (floor-remainder 1.1 1)))
  (test #t (raises-type-exception (floor-remainder 1 1.1)))
  (test #t (raises-arithmetic-exception (floor-remainder 1 0)))

  (test #t (raises-type-exception (truncate/ 1.1 1)))
  (test #t (raises-type-exception (truncate/ 1 1.1)))
  (test #t (raises-arithmetic-exception (truncate/ 1 0)))

  (test #t (raises-type-exception (truncate-quotient 1.1 1)))
  (test #t (raises-type-exception (truncate-quotient 1 1.1)))
  (test #t (raises-arithmetic-exception (truncate-quotient 1 0)))

  (test #t (raises-type-exception (truncate-remainder 1.1 1)))
  (test #t (raises-type-exception (truncate-remainder 1 1.1)))
  (test #t (raises-arithmetic-exception (truncate-remainder 1 0)))

  (test #t (raises-type-exception (round/ 1.1 1)))
  (test #t (raises-type-exception (round/ 1 1.1)))
  (test #t (raises-arithmetic-exception (round/ 1 0)))

  (test #t (raises-type-exception (round-quotient 1.1 1)))
  (test #t (raises-type-exception (round-quotient 1 1.1)))
  (test #t (raises-arithmetic-exception (round-quotient 1 0)))

  (test #t (raises-type-exception (round-remainder 1.1 1)))
  (test #t (raises-type-exception (round-remainder 1 1.1)))
  (test #t (raises-arithmetic-exception (round-remainder 1 0)))

  (test #t (raises-type-exception (euclidean/ 1.1 1)))
  (test #t (raises-type-exception (euclidean/ 1 1.1)))
  (test #t (raises-arithmetic-exception (euclidean/ 1 0)))

  (test #t (raises-type-exception (euclidean-quotient 1.1 1)))
  (test #t (raises-type-exception (euclidean-quotient 1 1.1)))
  (test #t (raises-arithmetic-exception (euclidean-quotient 1 0)))

  (test #t (raises-type-exception (euclidean-remainder 1.1 1)))
  (test #t (raises-type-exception (euclidean-remainder 1 1.1)))
  (test #t (raises-arithmetic-exception (euclidean-remainder 1 0)))

  (test #t (raises-type-exception (balanced/ 1.1 1)))
  (test #t (raises-type-exception (balanced/ 1 1.1)))
  (test #t (raises-arithmetic-exception (balanced/ 1 0)))

  (test #t (raises-type-exception (balanced-quotient 1.1 1)))
  (test #t (raises-type-exception (balanced-quotient 1 1.1)))
  (test #t (raises-arithmetic-exception (balanced-quotient 1 0)))

  (test #t (raises-type-exception (balanced-remainder 1.1 1)))
  (test #t (raises-type-exception (balanced-remainder 1 1.1)))
  (test #t (raises-arithmetic-exception (balanced-remainder 1 0)))
  )

(test-exit)
