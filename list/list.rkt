#lang class/1
;; A [List X] implements
;; - length : -> Natural
;; - reverse : -> [List X]
;; - append-last : X -> [List X]
;; - foldr : [Fun2 X Y] Y -> Y
;; - foldl : [Fun2 X Y] Y -> Y

;; A NumberToNumber implements
;; - call : Number -> Number

;; A StringToPosn implements
;; - call : String -> Posn

;; A [Fun1 X Y] implements
;; - call : X -> Y

;; A [Fun2 X Y Z] implements
;; - call : X Y -> Z

;; ☠ : [Y -> Z] [X -> Y] -> [X -> Z]
;; Compose function
(define ☠
  (λ (f g) (λ (x) (f (g x)))))

;; Compose functional object
(define-class ☠%
  ;; implements [Fun2 [Fun1 Y Z] [Fun1 X Y] [Fun1 X Z]]
  (define (call f g)
    (local [(define-class λ%
              (define (call x)
                (f . call (g . call x))))]
      (new λ%))))
  
(define-class n2s% ;; implements [Fun1 Number String]
  (define (call n)
    (number->string n)))

(define-class sq% ;; implements [Fun1 Number Number]
  (define (call x)
    (* x x)))

(define-class mult% ;; implements [Fun2 Number Number Number]
  (define (call x y)
    (* x y)))

(define-class count%
  (define (call x y)
    (add1 y)))

(define-class wrap% ;; implements [Fun2 X Y Z]
  (fields f) ;; [X Y -> Z]
  (define (call x y)
    ((this . f) x y)))

;; Function object for "cons"
(define-class newcons%
  (define (call x ls)
    (new cons% x ls)))

(define-class list%  
  (define (prod)
    (this . foldr (new mult%) 1))
  (define (sum)
    (this . foldr (new wrap% +) 0))
  (define (length)
    (this . foldl (new count%) 0))
  (define (append-last x)
    (this . foldr (new newcons%) (new cons% x (new empty%))))
  (define (reverse)
    (this . foldl (new newcons%) (new empty%))))

(define-class empty%
  (super list%)
  (define (foldr combine base) base)
  (define (foldl combine acc) acc))

(define-class cons%
  (super list%)
  (fields first rest)
  
  (define (foldr combine base)
    (combine . call
             (this . first)
             (this . rest . foldr combine base)))
  
  (define (foldl combine acc)
    (this . rest . foldl 
          combine
          (combine . call (this . first) acc))))
    
(define ls
  (new cons% 3 (new cons% 4 (new cons% 5 (new empty%)))))

(check-expect (ls . sum) 12)
(check-expect (ls . reverse . reverse) ls)
(check-expect (ls . length) 3)
(check-expect (ls . append-last 10 . sum) 22)

