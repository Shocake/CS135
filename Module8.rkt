;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Module8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;****************************
;; Justin Xing
;; CS 135 Fall 2022
;; Module 8 Exercises
;;****************************
;;

;; Exercise 1

;; (sort lon) sorts the elements of lon in non-increasing order
(check-expect (sort (list 3 4 8 2)) (list 8 4 3 2))
;; sort: (listof Num) → (listof Num)
(define (sort lon)
  (cond [(empty? lon) empty]
        [else (insert (first lon)
                      (sort (rest lon)))]))

;; insert: Num (listof Num) → (listof Num)
;; requires: slon is sorted in non-increasing order

(define (insert n slon)
  (cond [(empty? slon) (cons n empty)]
        [(>= n (first slon)) (cons n slon)]
        [else (cons (first slon) (insert n (rest slon)))]))

;; Exercise 2
;; You want to add one more element to the list lst. Do you use (cons elem lst) or
;; (list elem lst)? What’s the difference between them?
;;   You use cons, as (list elem lst) would make a list containing elem and a nested lst
;;   rather than adding the element to lst.

;; Why is (list 1 2) legal but (cons 1 2) is not?
;;   cons requires the second element to be a list, while list has a hidden empty already.

;; What’s the difference between (cons 1 empty) and (list 1 empty)?
;;   (cons 1 empty) is a list with 1 in it while (list 1 empty) has a nested list (empty)
;;   within the list.

;; Exercise 3
;; What is the length of gear?
(define gear (list (list "hat" "boots") "coat"
                   (list 32.3 (list "mitts")) empty "scarf"))

(check-expect (length gear) 5)

;; Exercise 4
;; Write an expression, et, using cons but not list so that (check-expect? gear e) passes.
(define et (cons (cons "hat" (cons "boots" empty))
                 (cons "coat"
                       (cons (cons 32.3
                                   (cons (cons "mitts" empty)
                                         empty))
                             (cons empty
                                   (cons "scarf"
                                         empty))))
                 )
  )

(check-expect gear et)

;; Exercise 5

;; An association list (AL) is one of:
;; ⋆ empty
;; ⋆ (cons (list Nat Str) AL)
;; Requires: each key (Nat) is unique

;; (add-al assoc alst) adds assoc to alst. If alst already contains assoc's
;; key, replace the value. Otherwise, add assoc at the end of alst.
;; Examples:
(check-expect (add-al (list 8 "Asha") empty) (list (list 8 "Asha")))
(check-expect ; alst does not contain this key.
 (add-al (list 7 "Bo")
         (list (list 8 "Asha") (list 2 "Joseph") (list 5 "Sami")))
 (list (list 8 "Asha") (list 2 "Joseph") (list 5 "Sami") (list 7 "Bo")))
(check-expect ; alst contains this key.
 (add-al (list 7 "Bo")
         (list (list 7 "Asha") (list 2 "Joseph") (list 5 "Sami")))
 (list (list 7 "Bo") (list 2 "Joseph") (list 5 "Sami") ))

;; add-al: (list Nat Str) AL -> AL
(define (add-al assoc alist)
  (cond
    [(empty? alist) (cons assoc empty)]
    [(= (first (first alist)) (first assoc)) (cons assoc (rest alist))]
    [else (cons (first alist) (add-al assoc (rest alist)))]
    )
  )

;; Exercise 6
;; (remove-al key alst) removes the association of key from alst.
;; Examples:
(check-expect (remove-al 8 (list (list 8 "Asha"))) empty)
(check-expect (remove-al 8 (list (list 8 "Asha") (list 7 "Bob"))) (list (list 7 "Bob")))

;; remove-al: Nat AL -> AL
(define (remove-al key alst)
  (cond
    [(empty? alst) empty]
    [(= key (first (first alst))) (remove-al key (rest alst))]
    [else (cons (first alst) (remove-al key (rest alst)))]
    )
  )

;; Exercise 7
;; How would you modify lookup-al to avoid searching the whole list most of the time?
;;   Use BSTs
;; How would you modify insert-al? Does it do more “work” or less “work” than
;; inserting into an unordered list?
;;   ? Less

;; Exercise 8
;; (expand-each l1 l2) makes a list of: For each item in the first list,
;; make a list that contains that item, followed by all the items in the second list.
;; Examples:

;; expand-each: (listof Any) (listof Any) -> (listof (listof Any))

;; Exercise 9
;; (vector-add lst1 lst2) adds the lst1 and lst2 as vectors.
;; Examples:
(check-expect (vector-add (list 3 5) (list 7 11)) (list 10 16))

;; vector-add: (listof Num) (listof Num) -> (listof Num)
(define (vector-add lst1 lst2)
  (cond [(empty? lst1) empty]
        [else
         (cons (+ (first lst1) (first lst2))
               (vector-add (rest lst1) (rest lst2)))]
        )
  )

;; Exercise 10

;; (join-names g s) Make a list of full names from g (given names) and
;; s (surnames).
;; Example:
(define gnames (list "Joseph" "Burt" "Douglas" "James" "David"))
(define snames (list "Hagey" "Matthews" "Wright" "Downey" "Johnston"))

(check-expect (join-names gnames snames)
              (list "Joseph Hagey" "Burt Matthews" "Douglas Wright"
                    "James Downey" "David Johnston"))
;; join-names: (listof Str) (listof Str) -> (listof Str)
(define (join-names f l)
  (cond
    [(empty? f) empty]
    [else
     (cons (string-append (first f) " " (first l))
           (join-names (rest f) (rest l)))]
    )
  )
           
;; Exercise 11

;; (merge lon1 lon2) merges lon1 and lon2
;; merge: (listof Num) (listof Num) → (listof Num)
;; Requires: lon1 and lon2 are already in ascending order.
(define (merge lon1 lon2)
  (cond [(empty? lon1) lon2]
        [(empty? lon2) lon1]
        [else
         (cond [(< (first lon1) (first lon2))
                (cons (first lon1) (merge (rest lon1) lon2))]
               [else (cons (first lon2) (merge lon1 (rest lon2)))])]))

;; Exercise 12
;; (list=? lst1 lst2) determines if lst1 and lst2 are equal
;; Examples:
(check-expect (list=? (list 1 3 5) (list 1 3)) false)
(check-expect (list=? (list 1 3 5) (list 1 4 5)) false)
(check-expect (list=? (list 1 3) (list 1 3 5)) false)
(check-expect (list=? (list 1 3 5) (list 1 3 5)) true)
;; list=?: (listof Num) (listof Num) → Boolean
(define (list=? lst1 lst2)
  (cond
    [(and (empty? lst1) (empty? lst2)) true]
    [(or (and (empty? lst1) (cons? lst2)) (and (cons? lst1) (empty? lst2))) false]
    [else
     (and (= (first lst1) (first lst2))
          (list=? (rest lst1) (rest lst2)))]))
