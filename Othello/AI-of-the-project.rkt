#lang racket
(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;helperFunctions

(define (give r l)
  (if (> r (length l)) "error"
      (list-ref l (- r 1)))) ;give the value at rth position in list l

(define (sum l)
  (foldr + 0 l))                                             ;give first r elements of list l

(define (divide l m)
  (if (null? l) '()
      (append (list (take l m)) (divide (list-tail l m) m ))))   ;divide list l into m parts

(define (posnPresent? posn listofPosns)
  (cond [(null? listofPosns) #f]
        [(equal? posn (car listofPosns)) #t]
        [else (posnPresent? posn (cdr listofPosns))]))    ;checks availability of posn in list os posns

(define (refine listofPosns)
  (if (null? listofPosns) '()
      (if (posnPresent? (car listofPosns) (cdr listofPosns)) (refine (cdr listofPosns))
          (cons (car listofPosns) (refine (cdr listofPosns))))))       ;removes same posn in the list of posns

(define (update-list n x l)
  (list-set l (- n 1) x))                                                     ;update nth position of list l with x

(define (min-len l)
  (if (= (length l) 1) (car l)
      (if (> (length (car l)) (length (min-len (cdr l))))
          (min-len (cdr l))
          (car l))))                                                     ;gives minimum length element in list l

(define (max-len l)
  (if (= (length l) 1) (car l)
      (if (< (length (car l)) (length (max-len (cdr l))))
          (max-len (cdr l))
          (car l))))                                                     ;gives maximum length element in list l


(define (mergeTwoBoard l1 l2 player)
  (cond [(= (length l1) 0) '()]
        [(= (car l1) player) (cons (car l1) (mergeTwoBoard (cdr l1) (cdr l2) player))]
        [(= (car l2) player) (cons (car l2) (mergeTwoBoard (cdr l1) (cdr l2) player))]
        [(= (or (car l1) (car l2)) (* -1 player)) (cons (* -1 player) (mergeTwoBoard (cdr l1) (cdr l2) player))]
        [(= (car l2) 0) (cons (car l1) (mergeTwoBoard (cdr l1) (cdr l2) player))]))                                  ;merge two boards in favour of player

(define (merge lis player)
  (cond [(= (length lis) 1) (car lis)]
        [else (let* ((nex (merge (cdr lis) player))
                     (pre (car lis)))
                (mergeTwoBoard pre nex player))]))
(define (place-to-posn check-posn len)
  (if (= (remainder check-posn len) 0)
      (list (quotient check-posn len) len)
      (list (+ 1 (quotient check-posn len))
            (remainder check-posn len))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;testing function in each direction
;higher-order function used

(define (test board r step terminate player)
  (define len (length (car board)))
  (define flatten (append* board))
  (define (helper-test check-posn posn-betwn)
    (cond [(terminate check-posn) (list 0 check-posn posn-betwn)]
          [(and (= 0 (give check-posn flatten))
                (= 0 (length posn-betwn))) (list 0 check-posn posn-betwn)]
          [(= 0 (give check-posn flatten)) (list 1 check-posn posn-betwn)]
          [(= player (give check-posn flatten)) (list 0 check-posn posn-betwn)]
          [(= (* -1 player) (give check-posn flatten))
           (let* ((position (place-to-posn check-posn len)))
             (helper-test (+ check-posn step)
                          (cons position
                                posn-betwn)))]))
  (helper-test (+ r step) '()))

(define (uptest board r player)
  (let* ((n (length (car board))))
    (test board r (* -1 n) (lambda(x) (if (< x 1) #t #f)) player)))

(define (downtest board r player)
  (let* ((n (length (car board))))
    (test board r n (lambda(x) (if (> x (expt n 2)) #t #f)) player)))

(define (righttest board r player)
  (let* ((n (length (car board))))
    (test board r 1 (lambda(x) (if (= (modulo x n) 1) #t #f)) player)))

(define (lefttest board r player)
  (let* ((n (length (car board))))
    (test board r -1 (lambda(x) (if (= (modulo x n) 0) #t #f)) player)))

(define (uprighttest board r player)
  (let* ((n (length (car board))))
    (test board r (* -1 (- n 1)) (lambda(x) (if (or (= (modulo x n) 1)
                                                    (< x 1)) #t #f)) player)))

(define (uplefttest board r player)
  (let* ((n (length (car board))))
    (test board r (* -1 (+ 1 n)) (lambda(x) (if (or (= (modulo x n) 0)
                                                    (< x 1)) #t #f)) player)))

(define (downrighttest board r player)
  (let* ((n (length (car board))))
    (test board r (+ 1 n) (lambda(x) (if (or (= (modulo x n) 1)
                                             (> x (expt n 2))) #t #f)) player)))

(define (downlefttest board r player)
  (let* ((n (length (car board))))
    (test board r (- n 1) (lambda(x) (if (or (= (modulo x n) 0)
                                             (> x (expt n 2))) #t #f)) player)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;list comprehension
(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...)
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...)
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;gives list of lists of posn of valid moves corresponding to the player
;list comprehension used

(define (getValidMoves board player)
  (define n (length (car board)))
  (define flatten (append* board))
  (define (helper-move r l1)
    (let* ((term (give r flatten))
           (posn (place-to-posn r n)))
      (cond [(> r (expt n 2)) (refine l1)]
            [(= term player)
             (let* ((upt (uptest board r player))
                    (dwt (downtest board r player))
                    (rtt (righttest board r player))
                    (ltt (lefttest board r player))
                    (urt (uprighttest board r player))
                    (ult (uplefttest board r player))
                    (drt (downrighttest board r player))
                    (dlt (downlefttest board r player)))
               (begin (lc (set! l1 (cons (place-to-posn (cadr y) n) l1))
                          : y <- (list upt dwt rtt ltt urt ult drt dlt) @ (= 1 (car y)))
                      (helper-move (+ r 1) l1)))]
            [else (helper-move (+ r 1) l1)])))
  (helper-move 1 '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;gives a list of lists of posn which will be occupied when
;the player will put its tile in the posn given as argument
;list comprehension used

(define (isValidMove board player posn)
  (define n (length (car board)))
  (define flatten (append* board))
  (define box (+ (cadr posn) (* (- (car posn) 1) n)))
  (define (checkBox r l1)
    (let* ((term (give r flatten)))
      (cond [(> r (expt n 2)) (refine l1)]
            [(= term player)
             (let* ((upt (uptest board r player)) 
                    (dwt (downtest board r player)) 
                    (rtt (righttest board r player)) 
                    (ltt (lefttest board r player)) 
                    (urt (uprighttest board r player)) 
                    (ult (uplefttest board r player)) 
                    (drt (downrighttest board r player)) 
                    (dlt (downlefttest board r player)))
               (begin (lc (set! l1 (append (caddr y) l1)) : y <- (list upt dwt rtt ltt urt ult drt dlt)
                          @ (and (= (car y) 1) (= box (cadr y))))
                      (checkBox (+ r 1) l1)))]
            [else (checkBox (+ r 1) l1)])))
  (let* ((moves (getValidMoves board player)))
    (if (not (posnPresent? posn moves))
        "Error: Not a valid move"
        (checkBox 1 '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;updates board when the player will put its tile in the posn given as argument

(define (updateBoard board player posn)
  (define flatten (append* board))
  (define (appendedPosn pos)
    (+ (cadr pos) (* n (- (car pos) 1))))
  (define n (length (car board)))
  (let* ((tiles (isValidMove board player posn))
         (updatedList (map (lambda(pos) (update-list (appendedPosn pos) player flatten)) tiles))
         (updatedBoard (mergeTwoBoard (update-list (appendedPosn posn) player flatten) (merge updatedList player) player)))
    (divide updatedBoard n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;divides the posns on the basis of stability

(define (stability lis n)
  (define (hihi ll n a b c)
    (if (null? ll) (list a b c)
        (let* ((cr (car ll)))
          (cond [(or (equal? cr (list 1 1))
                     (equal? cr (list 1 n))
                     (equal? cr (list n n))
                     (equal? cr (list n 1)))
                 (hihi (cdr ll) n (cons cr a) b c)]
                [(and (or (= (car cr) n)
                          (= (car cr) 1))
                      (not (or (= 2 (cadr cr))(= (- n 1) (cadr cr)))))
                 (hihi (cdr ll) n a (cons cr b) c)]
                [(and (or (= (cadr cr) n)
                          (= (cadr cr) 1))
                      (not (or (= 2 (car cr))(= (- n 1) (car cr)))))
                 (hihi (cdr ll) n a (cons cr b) c)]
                [else (hihi (cdr ll) n a b (cons cr c))]))))
  (hihi lis n '() '() '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;most stable posns list

(define (stable lis)
  (cond [(not (null? (car lis))) (car lis)]
        [(not (null? (cadr lis))) (cadr lis)]
        [else (caddr lis)]))

(define (process board player)
  (define n (length (car board)))
  (let* ((movestocheck (stable (stability (getValidMoves board (* -1 player)) n))) 
         (movesVs (map (lambda(posn) (cons posn (isValidMove board (* -1 player) posn)))
                       movestocheck)))
    (if (null? movesVs)
        (set! board board)
        (set! board (updateBoard board (* -1 player) (car (max-len movesVs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;gives the list of scores and posn after n steps (where n is level)
;considering the opposition plays optimum
(define (miniMaxofPosn board posn player level)
  (define n (length (car board)))
  (define (scoreOfPlayer player board) (* player (sum (append* board))))
  (define initialBoard board)
  (define dBoard
    (begin (set! board (updateBoard board player posn))
           (process board player)
           board))
  (define (helper step)
    (cond [(= step 1) dBoard]
          [else (begin (process board (* -1 player))
                       (process board player)
                       (helper (- step 1)))]))
  (cons posn (list (scoreOfPlayer player (helper level)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;gives best move corresponding to player checking n level of depth (n is level)

(define (bestMove player board level)
  (define (scoreOfPlayer player board)
    (* player (sum (append* board))))
  (define (maxScore lis)
    (if (= (length lis) 1) (car lis)
        (if (> (cadar lis) (cadr (maxScore (cdr lis))))
            (car lis) (maxScore (cdr lis)))))
  (let* ((possblPosn (getValidMoves board player))
         (scoresMoves (map (lambda(posn) (miniMaxofPosn board posn player level)) possblPosn)))
    (car (maxScore scoresMoves))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;shows and removes valid moves 
(define (show-2 board posns)
  (define n (length (car board)))
  (define flattened (append* board))
  (define (helper posn)
    (if (null? posn) (divide flattened n)
        (let* ((pos (car posn)))
          (begin (set! flattened (update-list
                                  (+ (* (- (car pos) 1) n) (cadr pos)) 2 flattened))
                 (helper (cdr posn))))))
  (helper posns))
  

(define (remove-2 board)
  (let* ((n (length (car board)))
         (flattened (append* board))
         (updated (map (lambda (x) (if (= x 2) 0 x)) flattened)))
    (divide updated n)))

(define (no-zero flattened)
  (if (null? flattened) #t
      (if (= (car flattened) 0) #f (no-zero (cdr flattened)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

         
                       
                              
                       
                       
          
          
        
    





