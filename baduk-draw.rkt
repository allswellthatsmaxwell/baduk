#lang racket/gui
; the main program to run in order to play Baduk. Draws a Baduk board and
; allows black and white to place move alternately. Handles the drawing
; and erasing necessary for placing and capturing stones.

(require racket/gui/base)
(require errortrace)
(require "baduk-base.rkt")
(require "baduk-sgf-ops.rkt")

(define b board)

(define square-size 42)
(define padding (/ square-size 2)) ; leave some space around the board
(define panel-size (+ (* board-size square-size)
                      (* padding 2)))
(define frame-size (+ panel-size square-size))
(define far-edge-px (- (- panel-size padding) square-size))

(define frame (new frame%
                   [label "board"]
                   [width frame-size]
                   [height frame-size]))

(define panel (new panel%
                   [parent frame]
                   [style (list 'border)]
                   [horiz-margin square-size]
                   [vert-margin square-size]))

(define current-player-color "black")

; a canvas that acts on the board and the
; on-screen board when a stone is placed.
(define go-canvas%
  (class canvas%
    (define/override (on-event ev)
      (when (send ev button-down? 'left)
        (let* ([x (send ev get-x)]
               [y (send ev get-y)]
               [gridpt-coords (snap-to-gridpt x y)]
               [gridpt-x (car gridpt-coords)]
               [gridpt-y (cdr gridpt-coords)]
               [center-x (- gridpt-x square-size)]
               [center-y (- gridpt-y square-size)]
               [row-col (coords-screen>board x y)]
               [row (car row-col)]
               [col (cdr row-col)]
               [gridpt (get-gridpt-by-coords b row col)])
          (place-stone b row col current-player-color)
          (send dc set-brush current-player-color 'solid)
          (set! current-player-color (opposite-color current-player-color))
          (send dc draw-ellipse center-x center-y square-size square-size)
          (let ([dead-stones (remove-surrounding-stones b row col)])
            (for-each erase-stone dead-stones)))))
  (super-new)))

; draws the initial window and game board
(define canvas (new go-canvas%
                    [parent panel]
                    ; draws board-size x board-size grid 
                    ; with margins around edges
                    [paint-callback 
                     (lambda (canvas dc)
                       (letrec
                           ([draw-grid
                             (lambda (x y)
                               (cond
                                 [(< y padding) '()]
                                 [else
                                  (send dc draw-line padding y far-edge-px y)
                                  (send dc draw-line x padding x far-edge-px)
                                  (draw-grid (- x square-size)
                                             (- y square-size))]))])
                         (draw-grid far-edge-px far-edge-px)))]))

; draws the board.
(define (draw-board board)
  (for-each
   (lambda (row-list)
     (for-each
      (lambda (gridpt)
        (let* ([row (gridpt-row gridpt)]
               [col (gridpt-col gridpt)]
               [coords (coords-board>screen row col)]
               [x (car coords)]
               [y (cdr coords)])
          (erase-stone gridpt)
          (cond [(gridpt-has-stone? gridpt)
                 (send dc set-brush (gridpt-color gridpt) 'solid)
                 (send dc set-pen "black" 1 'solid)
                 (send dc draw-ellipse x y square-size square-size)])))
      row-list))
   board))

; reads a game file (in sgf format) from sgf-path and draws that game on the board.
(define (read-sgf-from-file sgf-path)
  (let* ([gridpts (sgf-file->gridpts sgf-path)]
         [new-board (populate-board gridpts)])
    (set! b new-board)
    (for-each erase-stone gridpts)
    (draw-board new-board)))

(define pixel-diagonal (sequence->list
                        (in-range 0
                                  (+ panel-size square-size)
                                  square-size)))

; translates board row, col coordinates to
; pixel x, y coordinates to draw a stone at.
(define (coords-board>screen row col)
  (cons (list-ref pixel-diagonal row) (list-ref pixel-diagonal col)))

; translates screen x, y coordinates to a board coordinate
; between 0 (inclusive) and board-size (exclusive).
(define (coords-screen>board x y)
  (letrec ([find-gridpt
            (lambda (pixel-diagonal px i)
              (cond
                [(null? pixel-diagonal) i]
                [(< px (car pixel-diagonal)) i]
                [else  (find-gridpt (cdr pixel-diagonal) px (+ i 1))]))])
    (cons (find-gridpt pixel-diagonal x 0) (find-gridpt pixel-diagonal y 0))))

; snaps x, y to a new x, y that is centered on the
; nearest board intersection to the passed coordinates.
(define (snap-to-gridpt x y)
  (let* ([click-coords (coords-screen>board x y)]
         [x-board (car click-coords)]
         [y-board (cdr click-coords)]
         [gridpt-coords (coords-board>screen x-board y-board)]
         [gridpt-x (car gridpt-coords)]
         [gridpt-y (cdr gridpt-coords)])
        (cons gridpt-x gridpt-y)))

; erases the stone at the intersection that gridpt represents.
(define (erase-stone gridpt)
  (cond [(null? gridpt) (raise "gridpoint does not exist")]
        [else (let*
                  ([coords (coords-board>screen (- (gridpt-row gridpt) 1)
                                                (- (gridpt-col gridpt) 1))]
                   [x (car coords)]
                   [y (cdr coords)]
                   [half-square (/ square-size 2)])
                (send dc set-pen "white" 1 'solid)
                (send dc set-brush "white" 'solid)
                (send dc draw-rectangle x y square-size square-size)
                (send dc set-brush "black" 'solid)
                (send dc set-pen "black" 1 'solid)
                (send dc draw-line (+ x half-square)
                      (+ y square-size) (+ x half-square) y)
                (send dc draw-line (+ x square-size)
                      (+ y half-square) x (+ y half-square)))]))

; undoes the previous move
(define (pop-move)
  (let ([stone (pop-gridpt)])
    (erase-stone stone)
    (set! current-player-color (opposite-color current-player-color))
    stone))

(define dc (send canvas get-dc))

(send dc set-pen "black" 1 'solid)

(send frame show #t)

