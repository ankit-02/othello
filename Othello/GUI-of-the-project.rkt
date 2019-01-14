#lang racket/gui
(provide (all-defined-out))
(require "AI-of-the-project.rkt")
(require racket/draw)

;function for getting mouse click
(define pos #f)
(define game-canvas%
  (class canvas%
    (inherit refresh)
    
    (define position #f)
    
    (define/override (on-event click)
      (case (send click get-event-type)
        [(left-down)
         (begin
           (set! position (list (send click get-x) (send click get-y)))
           (set! pos position))
         (refresh)]
        [else (void)]))
    (super-new)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax for
  (syntax-rules (:)
    [(for unit : condition : step : statements)
     (begin
       (define (iter)
         (cond [condition (begin statements step (iter))]))
       (iter))]))

(struct coin (col pos) #:transparent)
(struct vec (x y) #:transparent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define n 10)
(define starter 0)

(define frame-size (+ 50  (* n 50)))
(define initbox (- (/ frame-size 2) (* n 25)))
(define board-frame (new frame% [label "Playing"]
                         [width frame-size]
                         [height (+ 210 frame-size)]))
(define canvas
  (new game-canvas%
       [parent board-frame]
       [paint-callback
        (lambda (canvas dc) (paint dc))]))
(define (paint dc) (send dc draw-bitmap face-bitmap 0 0))
(define face-bitmap (make-object bitmap% frame-size frame-size ))
(define bm-dc (make-object bitmap-dc% face-bitmap))
(send bm-dc clear)
(define black-pen (make-object pen% "black" 2 'solid))
(define yellow-brush (make-object brush% "yellow" 'solid))
(define white-brush (make-object brush% "white" 'solid))
(define green-brush (make-object brush% "green" 'solid))
(define black-brush (make-object brush% "Black"  'solid))
(define blue-brush (make-object brush% "Blue"  'solid))
(define red-pen (make-object pen% "RED" 2 'solid))
(define white-pen (make-object pen% "white" 1 'solid))
(define scale-radius 2)

(define (draw-coins l)
  (define i 0)
  (define j 0)
  (begin 
    (send bm-dc clear)
    (send bm-dc set-pen black-pen)
    (send bm-dc set-brush green-brush)
    (for i : (< i n) : (set! i (+ 1 i)) : (begin (set! j 0)
                                                 (for j : (< j n) : (set! j (+ 1 j))
                                                   :(send bm-dc draw-rectangle (+ initbox (* i 50))
                                                          (+ initbox (* j 50)) 50 50))))
    (send bm-dc set-background "yellow")
    (map (lambda (p)
           (begin (cond  [(= (coin-col p) 800)
                          (begin 
                            (send bm-dc set-brush blue-brush)
                            (send bm-dc set-pen red-pen))]  
                         
                         [(< (coin-col p) 0)
                          (begin
                            (send bm-dc set-brush black-brush)
                            (send bm-dc set-pen black-pen))]  
                         [else
                          (begin
                            (send bm-dc set-brush white-brush)
                            (send bm-dc set-pen white-pen))])  
                  (let*
                      ([posn (coin-pos p)]
                       [diameter (* 2.5  scale-radius (expt (abs (coin-col p)) .3333))]
                       [x (- (vec-x (coin-pos p)) (/ diameter 2))]
                       [y (- (vec-y (coin-pos p)) (/ diameter 2))])
                    (if (= (coin-col p) 800)   (send bm-dc draw-rectangle (+ x 3) (+ y 3)  40 40) 
                        (send bm-dc draw-ellipse x y diameter diameter) )))) l)
    (send board-frame refresh)
    (send board-frame show #t)))

(define dist 50)
(define (conv lis )
  (define n (length lis))
  (define (f l n i)
    (define (join l1 l2)
      (if (null? l1) '()
          (cons (list (car l1) (car l2)) (join (cdr l1) (cdr l2)))))
    (define (fun ll n r)
      (let* ((newlist (build-list n (lambda(t) t)))
             (pp (map (lambda(t) (coin (* 400 t) (vec 0  r))) ll))
             (pre-final (join newlist pp))
             (final (map (lambda(t) (coin (coin-col (cadr t)) (vec (* dist (+ (car t) disbyr)) (* (+ r disbyr) dist)))) pre-final)))
        final))
    (if (null? l) '()
        (append (fun (car l) n i) (f (cdr l) n (+ i 1)))))
  (f lis n 0))
(define disbyr
  (/ (+ initbox 25) dist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define exit 0)
(define level 0)
(define square-side 50)
(define (initialbed n)
  (let* ((a n)
         (ab (- (/ n 2) 1))
         (b (build-list n (lambda(x) 0)))
         (abb (build-list ab (lambda(x) b)))
         (mid-list (build-list ab (lambda(x) 0)))
         (midrow (list (append mid-list (list 1 -1) mid-list)))
         (mid1row (list (append mid-list (list -1 1) mid-list)))
         (final (append abb midrow mid1row abb)))
    final))
(define initial-board
  (initialbed n))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define reversi (new frame%
                     [label "Reversi"]
                     [min-width 500]
                     [min-height 400]))

(define player-turn (new text-field%
                         [label "Whose turn : "]
                         [parent board-frame]
                         [init-value "No-one"]))

(define winner (new frame%
                    [label "Winner"]
                    [min-width 200]
                    [min-height 50]))

(define play-again (new button%
                        [label "Play Again"]
                        [parent winner]
                        [min-width 100]
                        [min-height 50]
                        [vert-margin 50]
                        [horiz-margin 100]
                        [callback (lambda (button event)
                                    (begin
                                      (set! exit 0)
                                      (set! initial-board (initialbed n))
                                      (send winner show #f)
                                      (send reversi show #t)))]))
(define exit1 (new button%
                   [label "Exit"]
                   [parent board-frame]
                   [min-width 100]
                   [min-height 50]
                   [vert-margin 50]
                   [horiz-margin 100]
                   [callback (lambda (button event)
                               (set! exit 1))]))
(define exit2 (new button%
                   [label "Exit"]
                   [parent winner]
                   [min-width 100]
                   [min-height 50]
                   [vert-margin 50]
                   [horiz-margin 100]
                   [callback (lambda (button event)
                               (begin
                                 (send winner show #f)))]))

(define winnermsg (new text-field%
                       [label ""]
                       [parent winner]
                       [min-width 200]
                       [min-height 50]
                       [style '(multiple)]
                       [vert-margin 10]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define background (read-bitmap "reversiEight.png"))

(define rowpanel (new horizontal-panel%
                      [parent reversi]
                      [alignment '(center top)]
                      [vert-margin 10]
                      [spacing 20]))
(define rowpanel2 (new horizontal-panel%
                       [parent reversi]
                       [alignment '(center bottom)]
                       [vert-margin 10]
                       [spacing 20]))
(define col1 (new vertical-panel% 
                  [parent rowpanel]
                  [alignment '(left top)]))
(define col2 (new vertical-panel% 
                  [parent rowpanel]
                  [alignment '(center top)]))
(define col3 (new vertical-panel%
                  [parent rowpanel]
                  [alignment '(right top)]))

(define mode-selector (new radio-box%
                           [label "Game Mode : "]
                           [choices (list "Single Player" "Double Player")]
                           [parent col1]
                           [min-width 100]
                           [min-height 50]
                           [vert-margin 10]
                           [horiz-margin 20]))

(define size-selector (new radio-box%
                           [label "Board Size :  "]
                           [parent col2]
                           [choices (list "6" "8" "10")]
                           [min-width 100]
                           [min-height 50]
                           [horiz-margin 20]))

(define coin-selector (new radio-box%
                           [label "Starting Player : "]
                           [choices (list "Black" "White")]
                           [parent col3]
                           [min-width 100]
                           [min-height 50]
                           [vert-margin 10]
                           [horiz-margin 20]))

(define new-game-button (new button%
                             [label "New Game"]
                             [parent rowpanel2]
                             [min-width 100]
                             [min-height 50]
                             [vert-margin 30]
                             [callback (lambda (button event)
                                         (begin
                                           (let* [(k (send size-selector get-selection))]
                                             (begin
                                               (set! n (+ 6 (* 2 k)))
                                               (send board-frame resize (+ 50  (* n 50)) (+ 220  (* n 50))) 
                                               (set! initial-board
                                                     (initialbed (+ 6 (* 2 k))))))
                                           (send reversi show #f)
                                           (let* [(l (send mode-selector get-selection))
                                                  (m (send coin-selector get-selection))
                                                  (n (cond [(= m 0) (set! starter -1)]
                                                           [(= m 1) (set! starter 1)]))]
                                             (cond [(= l 0) (send level-selector show #t)]
                                                   [(= l 1) (play 'double starter)]))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define level-selector (new frame%
                            [label "Select Level"]
                            [width 300]
                            [height 400]))
(define middle (new vertical-panel% 
                    [parent level-selector]
                    [alignment '(center center)]))
(define level-top (new horizontal-panel%
                       [parent middle]
                       [alignment '(center top)]))
(define level-bottom (new horizontal-panel%
                          [parent middle]
                          [alignment '(center bottom)]))

(define select-level (new radio-box%
                          [label "Difficulty level : "]
                          [choices (list "Easy" "Medium" "Expert")]
                          [parent level-top]
                          [min-width 100]
                          [min-height 50]
                          [vert-margin 50]))
(define level-button (new button%
                          [label "Start Game"]
                          [parent level-bottom]
                          [min-width 100]
                          [min-height 50]
                          [vert-margin 80]
                          [callback (lambda (button event)
                                      (send level-selector show #f)
                                      (let* [(a (send select-level get-selection))]
                                        (begin
                                          (set! level (+ (* a 2) 1))
                                          (play 'single starter))))]))

(define (update-board-by-input player)
  (lambda (x y)
    (let* ((r-x (+ 1 (quotient (- x 10) square-side)))
           (r-y (+ 1 (quotient (- y 10) square-side)))
           (posn (list r-x r-y)))
      (updateBoard initial-board player posn))))

(define (clicked-posn pos)
  (let* ((n (length (car initial-board))))
    (list (+ 1 (quotient (- (cadr pos) 10) square-side))
          (+ 1 (quotient (- (car pos) 10) square-side)))))

(define (score board)
  (let* ((flattened (append* board))
         (sum (foldr + 0 flattened)))
    (cond [(= sum 0) "Well played! It's a Draw"]
          [(> sum 0) "The Winner is White"]
          [else "The Winner is Black"])))


(define (play singleordouble start-player)
  (cond [(or (= exit 1) (no-zero (append* initial-board)))
         (begin (send board-frame show #f)
                (send winner show #t)
                (send winnermsg set-value (score initial-board)))]
        [else (let* ((posns (getValidMoves initial-board start-player))
                     (posns1 (getValidMoves initial-board (* -1 start-player))))
                (if (and (null? posns1) (null? posns))
                    (begin (send board-frame show #f)
                           (send winnermsg set-value (string-append "NO VALID MOVES FOR EITHER PLAYER : "
                                                                    (score initial-board)))
                           (send winner show #t))
                    (begin (if (= start-player 1)
                               (send player-turn set-value "White")
                               (send player-turn set-value "Black"))
                           (if (null? posns)
                               (if (equal? singleordouble 'double)
                                   (play singleordouble (* -1 start-player))
                                   (computer-player (* -1 start-player)))
                               (begin (draw-coins (conv (show-2 initial-board posns)))
                                      (if (equal? #f pos)
                                          (begin
                                            (sleep/yield 1)
                                            (play singleordouble start-player))
                                          (let* ((posn (clicked-posn pos)))
                                            (if (posnPresent? posn posns)
                                                (let* ((next-step1 (updateBoard initial-board start-player posn))
                                                       (next-step2 (remove-2 next-step1)))
                                                  (begin (set! initial-board next-step2)
                                                         (draw-coins (conv next-step1))
                                                         (if (equal? singleordouble 'double)
                                                             (play singleordouble (* -1 start-player))
                                                             (computer-player (* -1 start-player)))))
                                                (begin
                                                  (sleep/yield 1)
                                                  (play singleordouble start-player))))))))))]))

(define (computer-player player)
  (begin
    (let* ((posns (getValidMoves initial-board player)))
      (if (null? posns) (play 'single (* -1 player))
          (let* ((best-move (bestMove player initial-board level)))
            (begin (set! initial-board (updateBoard initial-board player best-move))
                   (draw-coins (conv initial-board))))))
    (play 'single (* -1 player))))
