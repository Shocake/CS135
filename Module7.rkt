;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Module7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;****************************
;; Justin Xing
;; CS 135 Fall 2022
;; Module 7 Exercises
;;****************************
;;

;; Exercise 1
;; (sum-to n) produces the sum of all Nat between 0 and n
;; Examples:
(check-expect (sum-to 3) 6)

;; sum-to: Nat -> Nat
(define (sum-to n)
  (cond
    [(zero? n) 0]
    [else (+ n (sum-to (sub1 n)))]
    )
  )

;; Exercise 2
;; (sum-between n b) returns the sum of all Nat between b and n
;; Examples:
(check-expect (sum-between 5 3) 12)

;; sum-between: Nat Nat -> Nat
;; Requires: n >= b
(define (sum-between n b)
  (cond
    [(= n b) n]
    [else (+ n (sum-between (sub1 n) b))]
    )
  )

;; Exercise 3
;; (countdown-by top step) returns a list that counts down by step from top until the next
;; element would be zero or less
;; Examples:
(check-expect (countdown-by 5 2) (list 5 3 1))

;; countdown-by: Nat Nat -> (listof Nat)
(define (countdown-by top step)
  (cond
    [(<= top 0) empty]
    [else (cons top (countdown-by (- top step) step))]
    )
  )

;; Exercise 4
;; (n-th-item lst n) produces the nth item in lst, where (first lst) is the 0th
;; Example:
(check-expect (n-th-item (cons 3 (cons 7 (cons 31 (cons 63 empty)))) 0) 3)
(check-expect (n-th-item (cons 3 (cons 7 (cons 31 (cons 63 empty)))) 3) 63)

;; n-th-item: (listof Any) Nat → Any
;; requires: n < (length lst)
(define (n-th-item lst n)
  (cond
    [(= n 0) (first lst)]
    [else (n-th-item (rest lst) (- n 1))]
    )
  )