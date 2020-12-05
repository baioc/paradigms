(import (scheme base) (scheme write))
(import (scheme read) (scheme char))
(import (srfi 1)) ; lists
(import (srfi 27)) ; randomness


;;; We represent an Othello 8x8 board as a linear bytevector:
;;;   00   01 02 03 04 05 06 07 08   09
;;;   .. /-------------------------\ ..
;;;   10 | 11 12 13 14 15 16 17 18 | 19
;;;   20 | 21 22 23 24 25 26 27 28 | 29
;;;   30 | 31 32 33 34 35 36 37 38 | 39
;;;   40 | 41 42 43 44 45 46 47 48 | 49
;;;   50 | 51 52 53 54 55 56 57 58 | 59
;;;   60 | 61 62 63 64 65 66 67 68 | 69
;;;   70 | 71 72 73 74 75 76 77 78 | 79
;;;   80 | 81 82 83 84 85 86 87 88 | 89
;;;   .. \-------------------------/ ..
;;;   90   91 92 93 94 95 96 97 98   99

(define (make-board)
  (let ((board (make-bytevector 100 empty))) ; initially all empty
    ;; fill borders
    (do ((i 0 (+ i 1)))
        ((> i 9))
      (bytevector-u8-set! board i outer)               ; top
      (bytevector-u8-set! board (+ 90 i) outer)        ; bottom
      (bytevector-u8-set! board (* 10 i) outer)        ; left
      (bytevector-u8-set! board (+ (* 10 i) 9) outer)) ; right
    ;; set initial pieces
    (bytevector-u8-set! board 44 white)
    (bytevector-u8-set! board 45 black)
    (bytevector-u8-set! board 55 white)
    (bytevector-u8-set! board 54 black)
    ;; and then return the new board
    board))

(define (board-position row column)
  (+ (* row 10) column))

(define (board-ref board position)
  (bytevector-u8-ref board position))

(define (board-set! board position piece)
  (bytevector-u8-set! board position piece))

(define (board-copy board)
  (bytevector-copy board))

(define (display-board board)
  ;; column labels
  (do ((j 0 (+ j 1)))
      ((> j 8))
    (display (string-ref " abcdefgh" j))
    (display " "))
  ;; the rest of the board
  (do ((i 1 (+ i 1)))
      ((> i 8))
    (newline)
    (display i) ; line labels
    (display " ")
    (do ((j 1 (+ j 1)))
        ((> j 8))
      (display-piece (board-ref board (board-position i j)))
      (display " ")))
  (newline))

;;; Directed steps (up, left, right, down) are equivalent to (-10, -1, +1, +10).

(define directions
  '(-11 -10 -9
    -1      +1
    +9  +10 +11))

(define (move-in-direction position direction)
  (+ position direction))

;;; And borders can be checked by seeing if any of the digits are 0 or 9.

(define valid-squares
  '(11 12 13 14 15 16 17 18
    21 22 23 24 25 26 27 28
    31 32 33 34 35 36 37 38
    41 42 43 44 45 46 47 48
    51 52 53 54 55 56 57 58
    61 62 63 64 65 66 67 68
    71 72 73 74 75 76 77 78
    81 82 83 84 85 86 87 88))

(define (valid? move)
  (and (integer? move)
       (<= 11 move 88)
       (<= 1 (modulo move 10) 8)))

;;; Each valid (inner) square can be empty or have a player-owned piece.

(define black 0)
(define white 1)
(define empty 2)
(define outer 3)

(define (display-piece piece)
  (display (string-ref "O@.?" piece)))

(define (opponent player) ; black <-> white
  (- 1 player))

;;; A move is legal iff its at an empty square and would flip opponent pieces.

(define (legal? move player board)
  (and (valid? move)
       (eqv? (board-ref board move) empty)
       (any (lambda (dir) (would-flip? move player board dir))
            directions)))

(define (legal-moves color board)
  (filter (lambda (move) (legal? move color board))
          valid-squares))

(define (next-play strategy player board)
  (let ((move (strategy player (board-copy board))))
    (cond ((legal? move player board) move)
          (else
            (display "Illegal move!\n")
            (next-play strategy player board)))))

;;; Pieces are flipped when a move causes them to be bracketted between player pieces.

;; Semi-predicate, returns some bracketing position when it is found.
(define (would-flip? move player board direction)
  (let ((step (move-in-direction move direction)))
    (and (valid? step)
         (eqv? (board-ref board step) (opponent player))
         (let find-bracketing ((position (move-in-direction step direction)))
           (and (valid? position)
                (let ((piece (board-ref board position)))
                  (cond ((eqv? piece player) position)
                        ((eqv? piece (opponent player))
                         (find-bracketing (move-in-direction position direction)))
                        (else #f))))))))

;;; A player gets a turn iff he has legal moves, game is over when both don't.

(define (has-move? player board)
  (any (lambda (move) (legal? move player board))
        valid-squares))

(define (next-turn current board)
  (let ((next (opponent current)))
    (cond ((has-move? next board) next)
          ((has-move? current board) current)
          (else #f))))

(define (othello-match black-play white-play)
  (let ((board (make-board)))
    (display-board board)
    (do ((player black (next-turn player board))) ; black goes first
        ((not player)) ; game over when next-turn returns false
      (let* ((strategy (if (eqv? player black) black-play white-play))
             (move (next-play strategy player board)))
        (execute-move! move player board)
        (display-board board)))
    (let ((b (board-count black board)) (w (board-count white board)))
      (display "    Game Over\n")
      (display-board board)
      (display "Final score: [")
      (display-piece black) (display " = ") (display b)
      (display "] [")
      (display-piece white) (display " = ") (display w)
      (display "] => ")
      (cond ((> b w) (display-piece black) (display " wins!\n"))
            ((> w b) (display-piece white) (display " wins!\n"))
            (else (display "It's a tie!\n"))))))

(define (execute-move! move player board) ; supposes given move is legal
  (define (flip-bracketed! direction)
    (let ((bracketer (would-flip? move player board direction)))
      (when bracketer
        (do ((pos move (move-in-direction pos direction)))
            ((eqv? pos bracketer))
          (board-set! board pos player)))))

  (board-set! board move player)
  (for-each flip-bracketed! directions))

(define (board-count player board)
  (count (lambda (pos) (eqv? (board-ref board pos) player))
         valid-squares))


;;; Strategies

;; Human playing through the terminal.
(define (human color board)
  (display "Playing as ")
  (display-piece color)
  (display ". Next move: ")
  (let ((input (read)))
    (and (symbol? input)
         (let ((str (symbol->string input)))
           (and (= (string-length str) 2)
                (let* ((c (string-ref str 0))
                       (r (string-ref str 1))
                       (col (+ (- (char->integer c) (char->integer #\a)) 1))
                       (row (digit-value r)))
                  (board-position row col)))))))

;; Random strategy.
(define (random color board)
  (let ((moves (legal-moves color board)))
    (list-ref moves (random-integer (length moves)))))

(define (maximizer score-fn)
  (lambda (color board)
    (let* ((moves (legal-moves color board))
           (scores (map (lambda (move)
                          (let ((temp-board (board-copy board)))
                            (execute-move! move color temp-board)
                            (score-fn color temp-board)))
                        moves))
           (best (fold (lambda (score move best)
                         (let-values (((high-score best-move) best))
                           (if (> score high-score)
                               (values score move)
                               best)))
                       (values (car scores) (car moves))
                       (cdr scores)
                       (cdr moves))))
      (let-values (((score move) best)) move))))

(define (score-difference player board)
  (- (board-count player board)
     (board-count (opponent player) board)))

;; Greedy strategy that tries to take as many pieces as possible on each turn.
(define greedy (maximizer score-difference))

(define (weighted-score player board)
  (define weights
   #(0   0   0  0  0  0  0   0   0 0
     0 120 -20 20  5  5 20 -20 120 0
     0 -20 -40 -5 -5 -5 -5 -40 -20 0
     0  20  -5 15  3  3 15  -5  20 0
     0   5  -5  3  3  3  3  -5   5 0
     0   5  -5  3  3  3  3  -5   5 0
     0  20  -5 15  3  3 15  -5  20 0
     0 -20 -40 -5 -5 -5 -5 -40 -20 0
     0 120 -20 20  5  5 20 -20 120 0
     0   0   0  0  0  0  0   0   0 0))

  (let ((opponent (opponent player)))
    (fold
     +
     0
     (map (lambda (pos)
            (let ((piece (board-ref board pos)))
              (cond ((eqv? piece player) (vector-ref weights pos))
                    ((eqv? piece opponent) (- (vector-ref weights pos)))
                    (else 0))))
          valid-squares))))

;; Prioritizes the control of strategic positions on the board.
(define priority (maximizer weighted-score))

(define (minimax player board ply-depth score-fn)
  (define (maximin player board ply-depth score-fn)
    (let-values (((score move) (minimax player board ply-depth score-fn)))
      (values (- score) move)))

  (define (final-score player board)
    (let ((s (score-difference player board)))
      (* s s s))) ; cube it to maintain sign

  (define (high-score-move move best)
    (let ((temp-board (board-copy board)))
      (execute-move! move player temp-board)
      (let-values (((high-score best-move) best)
                   ((score opp-move) (maximin (opponent player)
                                              temp-board
                                              (- ply-depth 1)
                                              score-fn)))
        (if (or (not high-score) (> score high-score))
            (values score move)
            best))))

  (if (<= ply-depth 0)
      (values (score-fn player board) #f)
      (let ((moves (legal-moves player board)))
        (cond ((not (null? moves))
               (fold high-score-move (values #f #f) moves))
              ((has-move? (opponent player) board)
               (maximin (opponent player) board (- ply-depth 1) score-fn))
              (else
               (values (final-score player board) #f))))))

;; Returns the optimal strategy for a given score function and maximum ply.
(define (optimal ply-depth score-fn)
  (lambda (color board)
    (let-values (((score move) (minimax color board ply-depth score-fn)))
      move)))


;;; Playtesting

; (othello-match human random)                       ; random
; (othello-match human greedy)                       ; easy
; (othello-match human (optimal 2 score-difference)) ; normal
; (othello-match human priority)                     ; medium
; (othello-match human (optimal 2 weighted-score))   ; hard
; (othello-match human (optimal 3 weighted-score))   ; pro
