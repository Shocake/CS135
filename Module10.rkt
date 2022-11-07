;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Module10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;****************************
;; Justin Xing
;; CS 135 Fall 2022
;; Module 10 Exercises
;;****************************
;;

;; Exercise 1
(define somewhere (make-posn 7 2)
  )

;; Exercise 2
(check-expect (posn-x somewhere) 7)
(check-expect (posn-y somewhere) 2)

;; Exercise 3
(check-expect (posn? somewhere) true)
(check-expect (posn? (make-posn 2 9)) true)
(check-expect (posn? 1) false)
(check-expect (posn? 'five) false)

;; Exercise 4
;; (distance p) computes distance of p
;; Examples:
(check-expect (distance (make-posn 3 4)) 5)

;; distance: Posn -> Num
(define (distance p)
  (sqrt (+ (expt (posn-x p) 2) (expt (posn-y p) 2)))
  )

;; Exercise 5
;; (vector2D+ v1 v2) produce the vector sum of v1 and v2.
;; Example:
(check-expect (vector2D+ (make-posn 2 3) (make-posn 5 8)) (make-posn 7 11))

;; vector2D+: Posn Posn -> Posn
(define (vector2D+ p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2)) (+ (posn-y p1) (posn-y p2)))
  )

;; Exercise 6,7

(define-struct book (title author year))
;; a Book is a (make-book Str Str Nat)

(define mybook (make-book "Tree" "Frost" 2004))

(define (title b)
  (book-title b)
  )

(define (author b)
  (book-author b)
  )

(define (year b)
  (book-year b)
  )

;; Exercise 8

(define-struct inventory (desc price available))
;; an Inventory is a (make-inventory Str Num Nat)
;; Requires: price >= 0

;; (total-value item) produces the cost of all our items.
;; Example:
(check-expect (total-value (make-inventory "rice" 5.50 6)) 33.00)

;; total-value: Inventory → Num
(define (total-value i)
  (* (inventory-price i) (inventory-available i))
  )

;; Exercise 9
;; (raise-price dollars item) produce item with price increased by dollars.
;; Example:
(check-expect (raise-price 0.49 (make-inventory "rice" 5.50 6))
              (make-inventory "rice" 5.99 6))

;; raise-price: Num Inventory → Inventory
(define (raise-price raise i)
  (make-inventory
   (inventory-desc i)
   (+ (inventory-price i) raise)
   (inventory-available i)
   )
  )

;; Exercise 10
;; Convert to quote notation

(check-expect (cons 4 (cons "Donkey" (cons 'ice-cream empty))) '(4 "Donkey" ice-cream))

(check-expect (list 'paper 'pen "eraser" (list 32 'pencil (list "calculator")))
              '(paper pen "eraser" (32 pencil ("calculator"))))

