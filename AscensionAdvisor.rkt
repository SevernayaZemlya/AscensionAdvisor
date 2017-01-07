;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname AscensionAdvisor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)

;; CONSTANTS ==========================


;; DATA DEFINITIONS ============================

(define-struct card (buy punch))
;; Card is (make-card Natural[0, Infinity) Natural[0, Infinity))
;; interp. a card with buy and punch equal to values

(define APPRENTICE (make-card 1 0)) ; Apprentice has 1 buy
(define MYSTIC (make-card 2 0)) ; Mystic has 2 buy
(define MILITIA (make-card 0 1)) ; Militia has 1 punch
(define INFANTRY (make-card 0 2)) ; Infantry has 2 punch

#;
(define (fn-for-card c)
  (... (card-buy c)     ;Natural[0, Infinity)
       (card-punch c)))     ;Natural[0, Infinity)
       
  
;; Template rules used:
;;  - compound: 2 fields


;; ListOfCard is one of:
;; - empty
;; - (cons Card ListOfCard)
;; interp. a list of Cards
(define LOC1 empty)
(define LOC2 (cons APPRENTICE empty))
(define LOC3 (cons APPRENTICE (cons MILITIA empty)))
(define STARTDECK (cons APPRENTICE
                        (cons APPRENTICE
                              (cons APPRENTICE
                                    (cons APPRENTICE
                                          (cons APPRENTICE
                                                (cons APPRENTICE
                                                      (cons APPRENTICE
                                                            (cons APPRENTICE
                                                                  (cons MILITIA
                                                                        (cons MILITIA empty))))))))))) ; Starting Deck

#;
(define (fn-for-loc loc)
  (cond [(empty? loc)(...)]
        [else
         (... (fn-for-card (first loc))
              (fn-for-loc (rest loc)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: 2 fields
;; - reference: (first loc) is Card
;; - self-reference: (rest loc) is ListOfCard


;; FUNCTIONS ====================================

;; Card ListOfCards -> ListOfCard
;; remove Card from List

(check-expect (remove-card empty empty) empty) ; remove nothing from nothing
(check-expect (remove-card empty LOC3) LOC3) ; remove nothing from list
(check-expect (remove-card (make-card 0 1) LOC3) (cons APPRENTICE empty)) ; remove one Militia from list of Apprentice, Militia
(check-expect (remove-card APPRENTICE STARTDECK)
                        (cons APPRENTICE
                              (cons APPRENTICE
                                    (cons APPRENTICE
                                          (cons APPRENTICE
                                                (cons APPRENTICE
                                                      (cons APPRENTICE
                                                            (cons APPRENTICE
                                                                  (cons MILITIA
                                                                        (cons MILITIA empty)))))))))) ; removes one Apprentice from list

; (define (remove-card c loc) loc) ;stub
; Template from ListOfCard

(define (remove-card c loc)
  (cond [(empty? loc)empty]
        [else
         (if (same-card? c (first loc)) ; If you find card
             (rest loc)        ; Remove card
             (cons (first loc) ; Otherwise keep card
                   (remove-card c (rest loc))))])) ; and check rest of list

;; Card Card -> Boolean
;; Returns true if two cards match

(check-expect (same-card? APPRENTICE APPRENTICE) true)
(check-expect (same-card? APPRENTICE MILITIA) false)

;(define (same-card? c1 c2) false) ; stub
; Template from Card

(define (same-card? c1 c2)
  (if (or (empty? c1) (empty? c2))
      false
      (and
       (= (card-buy c1) (card-buy c2))
       (= (card-punch c1) (card-punch c2)))))
       
;; Card ListOfCard -> ListOfCard
;; add Card to List

(check-expect (add-card empty empty) empty) ; add nothing to nothing
(check-expect (add-card MILITIA empty) (cons MILITIA empty)) ; add Militia to nothing
(check-expect (add-card empty LOC3) LOC3) ; add nothing to list
(check-expect (add-card (make-card 0 1) LOC3) (cons MILITIA LOC3)) ; add one Militia to list of Apprentice, Militia
(check-expect (add-card MYSTIC STARTDECK)(cons MYSTIC (cons APPRENTICE
                        (cons APPRENTICE
                              (cons APPRENTICE
                                    (cons APPRENTICE
                                          (cons APPRENTICE
                                                (cons APPRENTICE
                                                      (cons APPRENTICE
                                                            (cons APPRENTICE
                                                                  (cons MILITIA
                                                                        (cons MILITIA empty)))))))))))) ; Adds one Mystic to list
; (define (add-card c loc) loc) ;stub
; Template from ListOfCard
(define (add-card c loc)
  (cond [(empty? c) loc]
        [else
         (cons c loc)]))

;; ListOfCard -> Number
;; Counts number of cards in list

(check-expect (count-list empty) 0)
(check-expect (count-list LOC2) 1)
(check-expect (count-list LOC3) 2)

;(define (count-list loc) 0) ; stub
;Template from ListofCard

(define (count-list loc)
  (cond [(empty? loc)0]
        [else
         (+ 1 (count-list (rest loc)))]))

;; ListOf Card-> Number
;; Totals buy of all Cards in list

(check-expect (buy-list empty) 0)
(check-expect (buy-list LOC2) 1)
(check-expect (buy-list LOC3) 1)
(check-expect (buy-list STARTDECK) 8)

;(define (buy-list loc) 0) ; stub
;Template from ListofCard

(define (buy-list loc)
  (cond [(empty? loc)0]
        [else         
         (+ (card-buy (first loc))
                      (buy-list (rest loc)))]))


;; ListofCard -> Number
;; Totals punch of all Cards in list

(check-expect (punch-list empty) 0)
(check-expect (punch-list LOC2) 0)
(check-expect (punch-list LOC3) 1)
(check-expect (punch-list STARTDECK) 2)

;(define (punch-list loc) 0) ; stub
;Template from ListofCard

(define (punch-list loc)
  (cond [(empty? loc)0]
        [else         
         (+ (card-punch (first loc))
                      (punch-list (rest loc)))]))

;; ListOfCard -> ListofNumber
;; Displays Average buy and punch per hand of List

(check-expect (average-hand empty) (cons 0 (cons 0 empty)))
(check-expect (average-hand LOC2) (cons 5 (cons 0 empty)))
(check-expect (average-hand LOC3) (cons 2.5 (cons 2.5 empty)))
(check-expect (average-hand STARTDECK)
              (cons (* 5 (/(buy-list STARTDECK)(count-list STARTDECK)))
              (cons (* 5(/(punch-list STARTDECK)(count-list STARTDECK))) empty)))

;(define (average-hand loc) empty) ; stub
;Template from ListOfCard

(define (average-hand loc)
  (cond [(empty? loc)(cons 0 (cons 0 empty))]
        [else
         (cons (* 5 (/ (buy-list loc) (count-list loc)))
               (cons (* 5 (/ (punch-list loc) (count-list loc))) empty))]))

(average-hand STARTDECK)
(average-hand (add-card MYSTIC (add-card MYSTIC STARTDECK)))
(average-hand (add-card MYSTIC (add-card MYSTIC (add-card MYSTIC STARTDECK))))
(average-hand (add-card (make-card 3 0) STARTDECK))

