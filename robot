;; ***************************************************
;; Gareth Sharpe (20337731)
;; CS 135 Winter 2016
;; Assignment 03, Question 2
;; ***************************************************

;; (move-west initial-position distance-to-move) moves the
;; robot in the compass west direction, independent of its orientation.
;; move-west : Posn Num -> Posn

;; Example:
(check-expect (move-west (make-posn 0 0) 10) (make-posn -10 0))

(define (move-west initial-position distance-to-move)
  (make-posn (- (posn-x initial-position) distance-to-move)
             (posn-y initial-position)))

;; Test:
(check-expect (move-west (make-posn 5 5) -8) (make-posn 13 5))

;; (move-east initial-position distance-to-move) moves the
;; robot in the compass east direction, independent of its orientation.
;; move-east : Posn Num -> Posn

;; Example:
(check-expect (move-east (make-posn 0 0) 10) (make-posn 10 0))

(define (move-east initial-position distance-to-move)
  (make-posn (+ (posn-x initial-position) distance-to-move)
             (posn-y initial-position)))

;; Test: 
(check-expect (move-east (make-posn 1 3) 5) (make-posn 6 3))

;; (move-north initial-position distance-to-move)) moves the
;; robot in the compass north direction, independent of its orientation.
;; move-north : Posn Num -> Posn

;; Example:
(check-expect (move-north (make-posn 0 0) 10) (make-posn 0 10))

(define (move-north initial-position distance-to-move)
  (make-posn (posn-x initial-position)
             (+ (posn-y initial-position) distance-to-move)))

;; Test: 
(check-expect (move-north (make-posn -3 -5) 5) (make-posn -3 0))

;; (move-south initial-position distance-to-move) moves the
;; robot in the compass south direction, independent of its orientation.
;; move-south : Posn Num -> Posn

;; Example:
(check-expect (move-south (make-posn 0 0) 10) (make-posn 0 -10))

(define (move-south initial-position distance-to-move)
  (make-posn (posn-x initial-position)
             (- (posn-y initial-position) distance-to-move)))

;; Test:
(check-expect (move-south (make-posn -4 -3) -5) (make-posn -4 2))

;; (robot initial-position initial-direction turn-action distance-to-move) produces a new
;;    posn for a robot after it consumes your instructions.

;; robot : Posn Sym Sym Num -> Posn

;; requires: initial-direction must be one of {'north, 'south, 'east, 'west}
;;           turn-action must be one of {'right, 'left, 'no-turn}

;; Examples
(check-expect (robot (make-posn 0 0) 'south 'right 1) (make-posn -1 0))
(check-expect (robot (make-posn 5 5) 'north 'left 3) (make-posn 2 5))
(check-expect (robot (make-posn -3 8) 'east 'no-turn -10) (make-posn -13 8))
(check-expect (robot (make-posn -1 -5) 'west 'right 7) (make-posn -1 2))


(define (robot initial-position initial-direction turn-action distance-to-move)
  (cond
    [(equal? initial-direction 'north) (cond
                                         [(equal? turn-action 'left)
                                          (move-west initial-position distance-to-move)]
                                         [(equal? turn-action 'right)
                                          (move-east initial-position distance-to-move)]
                                         [else
                                          (move-north initial-position distance-to-move)])]
    [(equal? initial-direction 'east) (cond
                                        [(equal? turn-action 'left)
                                         (move-north initial-position distance-to-move)]
                                        [(equal? turn-action 'right)
                                         (move-south initial-position distance-to-move)]
                                        [else
                                         (move-east initial-position distance-to-move)])]
    [(equal? initial-direction 'south) (cond
                                         [(equal? turn-action 'left)
                                          (move-east initial-position distance-to-move)]
                                         [(equal? turn-action 'right)
                                          (move-west initial-position distance-to-move)]
                                         [else
                                          (move-south initial-position distance-to-move)])]
    [(equal? initial-direction 'west) (cond
                                        [(equal? turn-action 'left)
                                         (move-south initial-position distance-to-move)]
                                        [(equal? turn-action 'right)
                                         (move-north initial-position distance-to-move)]
                                        [else
                                         (move-west initial-position distance-to-move)])]))

;; Tests:
(check-expect (robot (make-posn 0 0) 'north 'right 5) (make-posn 5 0))
(check-expect (robot (make-posn 0 0) 'north 'left 5) (make-posn -5 0))
(check-expect (robot (make-posn 0 0) 'north 'no-turn 5) (make-posn 0 5))

(check-expect (robot (make-posn 0 0) 'east 'right 5) (make-posn 0 -5))
(check-expect (robot (make-posn 0 0) 'east 'left 5) (make-posn 0 5))
(check-expect (robot (make-posn 0 0) 'east 'no-turn 5) (make-posn 5 0))

(check-expect (robot (make-posn 0 0) 'south 'right 5) (make-posn -5 0))
(check-expect (robot (make-posn 0 0) 'south 'left 5) (make-posn 5 0))
(check-expect (robot (make-posn 0 0) 'south 'no-turn 5) (make-posn 0 -5))

(check-expect (robot (make-posn 0 0) 'west 'right 5) (make-posn 0 5))
(check-expect (robot (make-posn 0 0) 'west 'left 5) (make-posn 0 -5))
(check-expect (robot (make-posn 0 0) 'west 'no-turn 5) (make-posn -5 0))
