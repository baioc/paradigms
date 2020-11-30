(import (scheme base) (scheme write))
(import (scheme read))
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

;;; A player gets a turn iff he has valid moves, game is over when both don't.

(define (next-turn current board)
  (define (has-move? player)
    (any (lambda (move) (legal? move player board))
         valid-squares))

  (let ((next (opponent current)))
    (cond ((has-move? next) next)
          ((has-move? current) current)
          (else #f))))

(define (othello-match black-play white-play)
  (let ((board (make-board)))
    (do ((player black (next-turn player board))) ; black goes first
        ((not player)) ; game over when next-turn returns false
      (let* ((strategy (if (eqv? player black) black-play white-play))
             (move (next-play strategy player board)))
        (execute-move! move player board)))
    (display "Game Over\n")
    (display-board board)
    (let ((b (board-count black board)) (w (board-count white board)))
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

(define (human color board)
  (display-board board)
  (display "Playing as ")
  (display-piece color)
  (display ". Next move (column line): ")
  (let ((input (read)))
    (and (list? input)
         (= (length input) 2)
         (let ((row (second input))
               (ch (string-ref (symbol->string (first input)) 0)))
           (let ((col (+ (- (char->integer ch) (char->integer #\a)) 1)))
             (board-position row col))))))

(define (random color board)
  (define (legal-moves color board)
    (filter (lambda (move) (legal? move color board))
            valid-squares))

  (let ((possibilities (legal-moves color board)))
    (list-ref possibilities (random-integer (length possibilities)))))


;; testing
(othello-match human random)
