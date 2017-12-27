;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rectangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRENDAN ZHANG (20720995)
;; CS135 Fall 2017
;; Assignment 09, Question 02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "rectanglelib.rkt")

(define-struct cell (num used?))
;; A Cell is a (make-cell Nat Bool)

;; A Grid is a (listof (listof Cell))
;; requires: the grid contains a non-empty list of non-empty lists,
;;  all the same length.

(define-struct rect (x y w h))
;; A Rect is a (make-rect Nat Nat Nat Nat)

(define-struct state (grid rects))
;; A State is a (make-state Grid (listof Rect))


;; Here are a couple of constants that can be used to define
;; the puzzle in the assignment, and a random larger puzzle.

(define puzz '((0 0 0 0 0 5 0)
               (0 0 0 0 0 2 2)
               (0 3 0 6 3 2 0)
               (4 0 0 0 0 0 0)
               (0 0 0 4 0 4 0)
               (2 0 6 0 2 4 0)
               (0 0 0 0 0 0 0)))

(define big-puzz '((4 0 7 0 0 0 0 0 0 0 0 21 0)
                   (0 3 2 0 0 0 0 0 0 0 0 0 2)
                   (0 0 0 0 0 0 0 2 3 0 0 0 0)
                   (0 0 0 20 0 0 0 0 0 0 0 0 5)
                   (0 2 0 0 0 0 0 4 0 0 0 0 0)
                   (0 0 3 0 0 0 0 0 0 0 0 0 0)
                   (3 0 0 0 0 5 2 4 0 0 0 0 0)
                   (0 0 0 0 0 2 0 6 0 0 0 0 0)
                   (0 0 0 20 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 24 0)
                   (0 0 0 0 4 0 4 0 0 0 4 0 0)
                   (0 0 3 0 0 0 0 0 0 0 8 0 2)))

(define state-puzz (make-state
                    (list
                     (list
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 5 false)
                      (make-cell 0 false))
                     (list
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 2 false)
                      (make-cell 2 false))
                     (list
                      (make-cell 0 false)
                      (make-cell 3 false)
                      (make-cell 0 false)
                      (make-cell 6 false)
                      (make-cell 3 false)
                      (make-cell 2 false)
                      (make-cell 0 false))
                     (list
                      (make-cell 4 false)
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 0 false))
                     (list
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 4 false)
                      (make-cell 0 false)
                      (make-cell 4 false)
                      (make-cell 0 false))
                     (list
                      (make-cell 2 false)
                      (make-cell 0 false)
                      (make-cell 6 false)
                      (make-cell 0 false)
                      (make-cell 2 false)
                      (make-cell 4 false)
                      (make-cell 0 false))
                     (list
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 0 false)
                      (make-cell 0 false)))
                    empty))

(define gridA (list (list (make-cell 1 true) (make-cell 1 true))
                    (list (make-cell 1 false) (make-cell 1 false))))

(define gridB (list (list (make-cell 4 true) (make-cell 0 true))
                    (list (make-cell 0 true) (make-cell 0 true))))

(define gridC (list (list (make-cell 1 true) (make-cell 1 false))
                    (list (make-cell 2 true) (make-cell 0 true))))


;;Part A

;;(map2d f lolov) consumes a function (f) and a list of lists of values (lolov) and
;; produces a new list of lists in which f has been applied to every element of lolov
;;map2d: (X -> Y) (listof (listof X)) -> (listof (listof Y))
;;Examples:
(check-expect (map2d add1 (list (list 3 4 5) (list 10 9 8)))
              (list (list 4 5 6) (list 11 10 9)))
(check-expect (map2d sqr (list (list 1))) (list (list 1)))

(define (map2d f lolov)
  (map (lambda (x) (map f x)) lolov))

;;Tests:
(check-expect (map2d floor (list (list 2.4) (list 4.5))) (list (list 2) (list 4)))
(check-expect (map2d sub1 (list (list -1 -2) (list 0 2)))
              (list (list -2 -3) (list -1 1)))


;;Part B

;;(construct-puzzle lolon) consumes a list of lists of natural numbers (lolon) and
;; produces a State representing the initial state of the puzzle
;;construct-puzle: (listof (listof Nat)) -> State
;;Examples:
(check-expect (construct-puzzle puzz) state-puzz)
(check-expect (construct-puzzle empty) (make-state empty empty))

(define (construct-puzzle lolon)
  (make-state (map2d (lambda (x) (make-cell x false)) lolon) empty))

;;Tests:
(check-expect (construct-puzzle (list (list 1 2) (list 5 6)))
              (make-state
               (list
                (list (make-cell 1 false) (make-cell 2 false))
                (list (make-cell 5 false) (make-cell 6 false)))
               empty))


;;Part C

;;(solved? state) consumes a State (state) and produces true if the puzzle described by
;; the state is fully solved and false if it is not fully solved
;;solved?: State -> Bool
;;Examples:
(check-expect (solved? state-puzz) false)
(check-expect (solved? (make-state empty empty)) true)

(define (solved? state)
  (local
    [(define grid (state-grid state))
    (define (row-solved? loc)
       (cond
         [(empty? loc) true]
         [(cell-used? (first loc)) (row-solved? (rest loc))]
         [else false]))
    (define (grid-solved? grid)
              (cond
                [(empty? grid) true]
                [(row-solved? (first grid)) (grid-solved? (rest grid))]
                [else false]))]
      (grid-solved? grid)))

;;Tests:
(check-expect (solved? (make-state gridB (list (make-rect 0 0 2 2)))) true)


;;Part D

;;(get-first-unused grid) consumed a Grid (grid) and produces a list containing the
;; x and y coordinates of the topmost, leftmost cell in grid which is not marked as
;; used
;;get-first-unused: Grid -> (list Nat Nat)
;;requires: grid contains at least one unused cell
;;Examples:
(check-expect (get-first-unused gridA) (list 1 0))

(define (get-first-unused grid)
  (local
    [(define (check-row i loc)
       (cond
         [(empty? loc) empty]
         [(not (cell-used? (first loc))) (list i)]
         [else (check-row (add1 i) (rest loc))]))
     (define (check-grid i grid)
       (cond
         [(empty? (check-row 0 (first grid))) (check-grid (add1 i) (rest grid))]
         [else (cons i (check-row 0 (first grid)))]))]
    (check-grid 0 grid)))

;;Tests:
(check-expect (get-first-unused gridC) (list 0 1))


;;Part E

;;(subgrid->list lolst bounds) produces a list of the elements in lolst
;; that lie within a given bounds
;;subgrid->list: (listof (listof X)) Rect -> (listof X)

(define (subgrid->list lolst bounds)
  (local
    [(define (sublist lst start w)
       (cond
         [(> start 0) (sublist (rest lst) (sub1 start) w)]
         [(> w 0) (cons (first lst) (sublist (rest lst) 0 (sub1 w)))]
         [else empty]))]
    (foldr append empty
           (map (lambda (row) (sublist row (rect-x bounds) (rect-w bounds)))
                (sublist lolst (rect-y bounds) (rect-h bounds))))))


;;(map-subgrid f lolst bounds) applies f to each element of lolst that lie within
;; a given bounds
;;map-subgrid (X -> Y) (listof (listof X)) -> (listof (listof (anyof X Y)))

(define (map-subgrid f lolst bounds)
  (local [(define (map-sublist f lst start w)
            (cond
              [(> start 0) (cons (first lst) (map-sublist f (rest lst) (sub1 start) w))]
              [(> w 0) (cons (f (first lst)) (map-sublist f (rest lst) 0 (sub1 w)))]
              [else lst]))]
    (map-sublist
     (lambda (row) (map-sublist f row (rect-x bounds) (rect-w bounds)))
     lolst (rect-y bounds) (rect-h bounds))))


;;(neighbours state) consumes a State (state) and produces a list of new states that
;; might legitimately follow from state after adding a single new rectangle and if
;; no legal rectangles can be added it produces the empty list
;;neighbours: State -> (listof State)

(define (neighbours state)
  (local
    [(define grid (state-grid state))
     (define unused (get-first-unused grid))
     (define width (length (first grid)))
     (define height (length grid))
     (define max-width (- width (first unused)))
     (define max-height (- height (second unused)))
     (define all-rects
       (foldr append empty
                      (build-list max-height
                                  (lambda (h)(build-list max-width
                                                         (lambda (w)
                                                           (make-rect
                                                            (first unused) (second unused)
                                                            (add1 w) (add1 h))))))))
     (define (valid-rect? rec)
       (local
         [(define cells (subgrid->list grid rec))
          (define pos (filter positive? (map cell-num cells)))]
         (and
          (not (member? true (map cell-used? cells)))
          (= (length pos) 1)
          (= (first pos) (* (rect-w rec) (rect-h rec))))))]
    (map (lambda (rec) (make-state
                        (map-subgrid (lambda (c) (make-cell (cell-num c) true)) grid rec)
                        (cons rec (state-rects state))))
         (filter valid-rect? all-rects))))

;;Tests:
(check-expect (neighbours (make-state (list (list (make-cell 0 false))) empty)) empty)


;;Part F

;;(solve-rectangle-puzzle lolon) consumes a list of lists of natural numbers describing
;; an initial puzzle and produces a list of recthangles that describe a solution
;; if one exists or false if no solution can be found
;;solve-rectangle-puzzle: (listof (listof Nat)) -> (anyof (listof Rect) false)

(define (solve-rectangle-puzzle puzz)
  (local
    [(define sol (search solved? neighbours (construct-puzzle puzz)))]
    (cond
      [(false? sol) false]
      [else (state-rects sol)])))

;;Tests:
(check-expect (solve-rectangle-puzzle puzz) (list
                                             (make-rect 5 5 2 2)
                                             (make-rect 4 5 1 2)
                                             (make-rect 1 5 3 2)
                                             (make-rect 0 5 1 2)
                                             (make-rect 0 4 4 1)
                                             (make-rect 5 3 2 2)
                                             (make-rect 4 2 1 3)
                                             (make-rect 4 1 2 1)
                                             (make-rect 2 1 2 3)
                                             (make-rect 1 1 1 3)
                                             (make-rect 6 0 1 2)
                                             (make-rect 1 0 5 1)
                                             (make-rect 0 0 1 4)))


