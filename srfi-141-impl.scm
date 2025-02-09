
;;; -*- Mode: Scheme -*-

;;;; Integer Division Operators

;;; Given a QUOTIENT and REMAINDER defined for nonnegative numerators
;;; and positive denominators implementing the truncated, floored, or
;;; Euclidean integer division, this implements a number of other
;;; integer division operators.

;;; Copyright (c) 2010--2011 Taylor R. Campbell
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

;;; CHICKEN Scheme port edited by Wolfgang Corcoran-Mathe 2022.

(define (%check-arguments loc n d)
  (assert-type loc (integer? n))
  (assert-type loc (integer? d))
  (if (zero? d)
      (arithmetic-exception loc "division by zero" n d)))

;;;; Integer Division

;;;; Ceiling

(: ceiling/ (integer integer -> integer integer))
(define (ceiling/ n d)
  (%check-arguments 'ceiling/ n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (ceiling-/- n d))
            ((negative? n)
             (let ((n (- 0 n)))
               (values (- 0 (quotient n d)) (- 0 (remainder n d)))))
            ((negative? d)
             (let ((d (- 0 d)))
               (values (- 0 (quotient n d)) (remainder n d))))
            (else
             (ceiling+/+ n d)))
      (let ((q (ceiling (/ n d))))
        (values q (- n (* d q))))))

(: ceiling-/- (integer integer -> integer integer))
(define (ceiling-/- n d)
  (let ((n (- 0 n)) (d (- 0 d)))
    (let ((q (quotient n d)) (r (remainder n d)))
      (if (zero? r)
          (values q r)
          (values (+ q 1) (- d r))))))

(: ceiling+/+ (integer integer -> integer integer))
(define (ceiling+/+ n d)
  (let ((q (quotient n d)) (r (remainder n d)))
    (if (zero? r)
        (values q r)
        (values (+ q 1) (- r d)))))

(: ceiling-quotient (integer integer -> integer))
(define (ceiling-quotient n d)
  (%check-arguments 'ceiling-quotient n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (receive (q r) (ceiling-/- n d) r q))
            ((negative? n) (- 0 (quotient (- 0 n) d)))
            ((negative? d) (- 0 (quotient n (- 0 d))))
            (else (receive (q r) (ceiling+/+ n d) r q)))
      (ceiling (/ n d))))

(: ceiling-remainder (integer integer -> integer))
(define (ceiling-remainder n d)
  (%check-arguments 'ceiling-remainder n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (receive (q r) (ceiling-/- n d) q r))
            ((negative? n) (- 0 (remainder (- 0 n) d)))
            ((negative? d) (remainder n (- 0 d)))
            (else (receive (q r) (ceiling+/+ n d) q r)))
      (- n (* d (ceiling (/ n d))))))

;;;; Euclidean Division

;;; 0 <= r < |d|

(: euclidean/ (integer integer -> integer integer))
(define (euclidean/ n d)
  (%check-arguments 'euclidean/ n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d)) (ceiling-/- n d))
            ((negative? n) (floor-/+ n d))
            ((negative? d)
             (let ((d (- 0 d)))
               (values (- 0 (quotient n d)) (remainder n d))))
            (else (values (quotient n d) (remainder n d))))
      (let ((q (if (negative? d) (ceiling (/ n d)) (floor (/ n d)))))
        (values q (- n (* d q))))))

(: euclidean-quotient (integer integer -> integer))
(define (euclidean-quotient n d)
  (%check-arguments 'euclidean-quotient n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (receive (q r) (ceiling-/- n d) r q))
            ((negative? n) (receive (q r) (floor-/+ n d) r q))
            ((negative? d) (- 0 (quotient n (- 0 d))))
            (else (quotient n d)))
      (if (negative? d) (ceiling (/ n d)) (floor (/ n d)))))

(: euclidean-remainder (integer integer -> integer))
(define (euclidean-remainder n d)
  (%check-arguments 'euclidean-remainder n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (receive (q r) (ceiling-/- n d) q r))
            ((negative? n) (receive (q r) (floor-/+ n d) q r))
            ((negative? d) (remainder n (- 0 d)))
            (else (remainder n d)))
      (- n (* d (if (negative? d) (ceiling (/ n d)) (floor (/ n d)))))))

;;;; Floor

(: floor/ (integer integer -> integer integer))
(define (floor/ n d)
  (%check-arguments 'floor/ n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (let ((n (- 0 n)) (d (- 0 d)))
               (values (quotient n d) (- 0 (remainder n d)))))
            ((negative? n) (floor-/+ n d))
            ((negative? d) (floor+/- n d))
            (else (values (quotient n d) (remainder n d))))
      (let ((q (floor (/ n d))))
        (values q (- n (* d q))))))

(: floor-/+ (integer integer -> integer integer))
(define (floor-/+ n d)
  (let ((n (- 0 n)))
    (let ((q (quotient n d)) (r (remainder n d)))
      (if (zero? r)
          (values (- 0 q) r)
          (values (- (- 0 q) 1) (- d r))))))

(: floor+/- (integer integer -> integer integer))
(define (floor+/- n d)
  (let ((d (- 0 d)))
    (let ((q (quotient n d)) (r (remainder n d)))
      (if (zero? r)
          (values (- 0 q) r)
          (values (- (- 0 q) 1) (- r d))))))

(: floor-quotient (integer integer -> integer))
(define (floor-quotient n d)
  (%check-arguments 'floor-quotient n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d)) (quotient (- 0 n) (- 0 d)))
            ((negative? n) (receive (q r) (floor-/+ n d) r q))
            ((negative? d) (receive (q r) (floor+/- n d) r q))
            (else (quotient n d)))
      (floor (/ n d))))

(: floor-remainder (integer integer -> integer))
(define (floor-remainder n d)
  (%check-arguments 'floor-remainder n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (- 0 (remainder (- 0 n) (- 0 d))))
            ((negative? n) (receive (q r) (floor-/+ n d) q r))
            ((negative? d) (receive (q r) (floor+/- n d) q r))
            (else (remainder n d)))
      (- n (* d (floor (/ n d))))))

;;;; Round Ties to Even

(: round/ (integer integer -> integer integer))
(define (round/ n d)
  (%check-arguments 'round/ n d)
  (define (divide n d adjust leave)
    (let ((q (quotient n d)) (r (remainder n d)))
      (if (and (not (zero? r))
               (or (and (odd? q) (even? d) (divisible? n (quotient d 2)))
                   (< d (* 2 r))))
          (adjust (+ q 1) (- r d))
          (leave q r))))
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (divide (- 0 n) (- 0 d)
               (lambda (q r) (values q (- 0 r)))
               (lambda (q r) (values q (- 0 r)))))
            ((negative? n)
             (divide (- 0 n) d
               (lambda (q r) (values (- 0 q) (- 0 r)))
               (lambda (q r) (values (- 0 q) (- 0 r)))))
            ((negative? d)
             (divide n (- 0 d)
               (lambda (q r) (values (- 0 q) r))
               (lambda (q r) (values (- 0 q) r))))
            (else
             (let ((return (lambda (q r) (values q r))))
               (divide n d return return))))
      (let ((q (round (/ n d))))
        (values q (- n (* d q))))))

(: divisible? (integer integer -> boolean))
(define (divisible? n d)
  ;; This operation admits a faster implementation than the one given
  ;; here.
  (zero? (remainder n d)))

(: round-quotient (integer integer -> integer))
(define (round-quotient n d)
  (%check-arguments 'round-quotient n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (receive (q r) (round/ n d)
        r                               ;ignore
        q)
      (round (/ n d))))

(: round-remainder (integer integer -> integer))
(define (round-remainder n d)
  (%check-arguments 'round-remainder n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (receive (q r) (round/ n d)
        q                               ;ignore
        r)
      (- n (* d (round (/ n d))))))

;;;; Truncate

(: truncate/ (integer integer -> integer integer))
(define (truncate/ n d)
  (%check-arguments 'truncate/ n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (let ((n (- 0 n)) (d (- 0 d)))
               (values (quotient n d) (- 0 (remainder n d)))))
            ((negative? n)
             (let ((n (- 0 n)))
               (values (- 0 (quotient n d)) (- 0 (remainder n d)))))
            ((negative? d)
             (let ((d (- 0 d)))
               (values (- 0 (quotient n d)) (remainder n d))))
            (else
             (values (quotient n d) (remainder n d))))
      (let ((q (truncate (/ n d))))
        (values q (- n (* d q))))))

(: truncate-quotient (integer integer -> integer))
(define (truncate-quotient n d)
  (%check-arguments 'truncate-quotient n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d)) (quotient (- 0 n) (- 0 d)))
            ((negative? n) (- 0 (quotient (- 0 n) d)))
            ((negative? d) (- 0 (quotient n (- 0 d))))
            (else (quotient n d)))
      (truncate (/ n d))))

(: truncate-remainder (integer integer -> integer))
(define (truncate-remainder n d)
  (%check-arguments 'truncate-remainder n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (- 0 (remainder (- 0 n) (- 0 d))))
            ((negative? n) (- 0 (remainder (- 0 n) d)))
            ((negative? d) (remainder n (- 0 d)))
            (else (remainder n d)))
      (- n (* d (truncate (/ n d))))))

;;; Copyright 2015 William D Clinger.
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright and permission notice in full.
;;;
;;; I also request that you send me a copy of any improvements that you
;;; make to this software so that they may be incorporated within it to
;;; the benefit of the Scheme community.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: balanced/ (integer integer -> integer integer))
(define (balanced/ x y)
  (%check-arguments 'balanced/ x y)
  (call-with-values
   (lambda () (euclidean/ x y))
   (lambda (q r)
     (cond ((< r (abs (/ y 2)))
            (values q r))
           ((> y 0)
            (values (+ q 1) (- x (* (+ q 1) y))))
           (else
            (values (- q 1) (- x (* (- q 1) y))))))))

(: balanced-quotient (integer integer -> integer))
(define (balanced-quotient x y)
  (%check-arguments 'balanced-quotient x y)
  (call-with-values
   (lambda () (balanced/ x y))
   (lambda (q r) q)))

(: balanced-remainder (integer integer -> integer))
(define (balanced-remainder x y)
  (%check-arguments 'balanced-remainder x y)
  (call-with-values
   (lambda () (balanced/ x y))
   (lambda (q r) r)))
