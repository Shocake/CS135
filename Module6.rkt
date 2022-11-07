;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Module6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;****************************
;; Justin Xing
;; CS 135 Fall 2022
;; Module 6 Exercises
;;****************************
;;

;; Exercise 1

(define slst (cons "milk"
                   (cons "eggs"
                         (cons "bread"
                               (cons "PB" empty)))))

;; using only first, rest, slst:

;; produce the string "bread"
(define (bread x)
  (first (rest (rest x)))
  )
;; Test:
(check-expect (bread slst) "bread")

;; produce the list (cons "bread" (cons "PB" empty))
(define (lst x)
  (rest (rest x))
  )
;; Test:
(check-expect (lst slst) (cons "bread" (cons "PB" empty)))

;; produce the empty list (starting with slst)
(define (emp x)
  (rest (rest (rest (rest slst))))
  )
;; Test:
(check-expect (emp slst) empty)

;; Exercise 2

;; (same-consec-cond? loc) determines if next two concerts are the same using cond
;; Examples:
(check-expect (same-consec-cond? empty) false)
(check-expect (same-consec-cond? (cons "a" empty)) false)
(check-expect (same-consec-cond? (cons "a" (cons "a" empty))) true)
(check-expect (same-consec-cond? (cons "a" (cons "b" empty))) false)

;; same-consec-cond?: (listof Str) -> Bool
(define (same-consec-cond? loc)
  (cond
    [(empty? loc) false]
    [(empty? (rest loc)) false]
    [(string=? (first loc) (first (rest loc))) true]
    [else false]
    )
  )

;; Exercise 3

;; (remove-second lst) produces the list with the second item removed
;; Examples:
(check-expect (remove-second (list 1 3 'five)) (list 1 'five))

;; remove-second: (listof Any) -> (listof Any)
;; Requires: (length lst) >= 2
(define (remove-second lst)
  (cons (first lst) (rest (rest lst)))
  )

;; Exercise 4

;; (sum lst) produces the sum of all the values in the list
;; Examples:
(check-expect (sum (list 1 4 5)) 10)

;; sum: (listof Int) -> Int
(define (sum lst)
  (cond
    [(empty? lst) 0]
    [else (+ (first lst) (sum (rest lst)))]
    )
  )

;; Exercise 5

;; (keep-evens lst) returns the list with only even values
;; Examples:
(check-expect (keep-evens (list 2 5 8)) (list 2 8))

;; keep-evens: (listof Int) -> (listof Int)
(define (keep-evens lst)
  (cond
    [(empty? lst) empty]
    [(even? (first lst))
     (cons (first lst) (keep-evens (rest lst)))]
    [else
     (keep-evens (rest lst))]
    )
  )

;; Exercise 6

;; (longest-word lst) produces the length of the longest word in the list of words.
;; Examples:
(check-expect (longest-word (cons "and" empty)) 3)
(check-expect (longest-word (cons "and" (cons "then" empty))) 4)

;; longest-word: (ne-listof Str) -> Nat
(define (longest-word lst)
  (longest-word/acc lst (first lst))
  )

;; (longest-word/acc lst word) produces the length of the longest word in the list of words
;; using accumulative recursion
;; longest-word/acc: (listof Str) Str -> Nat
(define (longest-word/acc lst word)
  (cond
    [(empty? lst) (string-length word)]
    [(> (string-length (first lst)) (string-length word))
     (longest-word/acc (rest lst) (first lst))]
    [else
     (longest-word/acc (rest lst) word)]
    )
  )

;; Exercise 7

;; (e->* s) replaces each e in s with *
;; Examples:
(check-expect (e->* "hello") "h*llo")

;; e->*: Str -> Str
(define (e->* s)
  (list->string (e->*/list (string->list s)))
)

;; (e->*/list lst) replaces each #\e in lst with #\*
;; e->*: (listof Char) -> (listof Char)
(define (e->*/list lst)
  (cond
    [(empty? lst) empty]
    [(char=? (first lst) #\e)
     (cons #\* (e->*/list (rest lst)))]
    [else (cons (first lst) (e->*/list (rest lst)))]
    )
  )

;; Exercise 8
;; (add-first lst) adds the first number to all the numbers in the list
;; Examples:
(check-expect (add-first (list 1 3 4)) (list 2 4 5))

;; add-first: (ne-listof Num) -> (ne-listof Num)
(define (add-first lst)
  (add-first/h lst (first lst))
  )

;; add-first/h: (ne-listof Num) (Num) -> (ne-listof Num)
(define (add-first/h lst f)
  (cond
    [(empty? lst) empty]
    [else (cons (+ (first lst) f) (add-first/h (rest lst) f))]
    )
  )

;; Exercise 9
;; (drop-first lst) produces lst with all copies of the first item removed.
;; Examples:
(check-expect (drop-first (cons 'A (cons 'V (cons 'B (cons 'A (cons 'D empty))))))
              (cons 'V (cons 'B (cons 'D empty))))

;; drop-first: (ne-listof Sym) -> (listof Sym)
(define (drop-first lst)
  (drop-first/h lst (first lst))
  )

;; drop-first/h: (listof Sym) Sym -> (listof Sym)
(define (drop-first/h lst s)
  (cond
    [(empty? lst) empty]
    [(symbol=? (first lst) s) (drop-first/h (rest lst) s)]
    [else (cons (first lst) (drop-first/h (rest lst) s))]
    )
  )