;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Module9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;****************************
;; Justin Xing
;; CS 135 Fall 2022
;; Module 9 Exercises
;;****************************
;;


;; Exercise 1
;; (extend-fib n lst) returns a list containing n more Fibonacci values than lst.
;; Examples:
(check-expect (extend-fib 2 (list 4 3 1)) (list 11 7 4 3 1))

;; extend-fib: Nat (listof Nat) -> (listof Nat)  
(define (extend-fib n lst)
  (cond
    [(= n 0) lst]
    [else (extend-fib (sub1 n) (cons (+ (first lst) (first (rest lst))) lst))]
    )
  )

;; Exercise 2
;; (fiba n) is a wrapper for extend-fib, and produces the nth Fibonacci number. (0,1,1,2,3...)
;; Examples:
(check-expect (fiba 2) 1)
(check-expect (fiba 5) 3)

;; fiba: Nat -> Nat
(define (fiba n)
  (first (extend-fib (- n 2) (list 1 0)))
  )

;; Exercise 3
;; (mean lst) produces the mean of the list
;; Examples:
(check-expect (mean (list 1 2 3)) 2)

;; mean: (listof Num) -> Num
(define (mean lst)
  (mean/acc lst 0 0)
  )

;; mean/acc: (listof Num) Num -> Num
(define (mean/acc lst acc elements)
  (cond
    [(empty? lst) (/ acc elements)]
    [else (mean/acc (rest lst) (+ acc (first lst)) (add1 elements))]
    )
  )