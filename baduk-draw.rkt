#lang racket/gui
(require racket/gui/base)
(require "baduk-base.rkt")

(define square-size 42)
(define padding (/ square-size 2)) ; around board
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
               [col (cdr row-col)])
          (print row) (print " ") (print col) (print " ") (displayln "")
          (place-stone board (- row 1) (- col 1) current-player-color)
          (send dc set-brush current-player-color 'solid)
          (set! current-player-color (switch-turn-color current-player-color))
          (send dc draw-ellipse center-x center-y square-size square-size)
          (let ([captures (collect-single-captures board (- row 1) (- col 1))]) ; minus 1'd while solving neighbor problem
            (remove-capture-list captures)
            (for-each erase-stone captures))))) ; erases 1 too far in SE direction
    (super-new)))

(define canvas (new go-canvas%
                    [parent panel]
                    [paint-callback ; draws board-size x board-size grid with margins around edges
                     (lambda (canvas dc)
                       (letrec ([draw-grid (lambda (x y)
                                             (cond
                                               [(< y padding) '()]
                                               [else
                                                (send dc draw-line padding y far-edge-px y)
                                                (send dc draw-line x padding x far-edge-px)
                                                (draw-grid (- x square-size) (- y square-size))]))])
                         (draw-grid far-edge-px far-edge-px)))]))

(define (draw-board board)
  (for-each
   (lambda (row-list)
     (for-each
      (lambda (gridpt)
        (let* ([row (+ (gridpt-row gridpt) 1)]
               [col (+ (gridpt-col gridpt) 1)]
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


(define pixel-diagonal (sequence->list (in-range 0 (+ panel-size square-size) square-size)))

(define (coords-board>screen row col)
  (cons (list-ref pixel-diagonal row) (list-ref pixel-diagonal col)))

(define (coords-screen>board x y)
  (letrec ([find-gridpt (lambda (pixel-diagonal px i)
                          (cond
                            [(null? pixel-diagonal) i]
                            [(< px (car pixel-diagonal)) i]
                            [else  (find-gridpt (cdr pixel-diagonal) px (+ i 1))]))])
    (cons (find-gridpt pixel-diagonal x 0) (find-gridpt pixel-diagonal y 0))))

(define (snap-to-gridpt x y)
  (let* ([click-coords (coords-screen>board x y)]
         [x-board (car click-coords)]
         [y-board (cdr click-coords)]
         [gridpt-coords (coords-board>screen x-board y-board)]
         [gridpt-x (car gridpt-coords)]
         [gridpt-y (cdr gridpt-coords)])
        (cons gridpt-x gridpt-y)))

(define (erase-stone gridpt)
  (cond [(null? gridpt) (raise "gridpoint does not exist")]
        [else (let* ([coords (coords-board>screen (- (gridpt-row gridpt) 1) (- (gridpt-col gridpt) 1))]
                     [x (car coords)]
                     [y (cdr coords)]
                     [half-square (/ square-size 2)])
                (send dc set-pen "white" 1 'solid)
                (send dc set-brush "white" 'solid)
                (send dc draw-rectangle x y square-size square-size)
                (send dc set-brush "black" 'solid)
                (send dc set-pen "black" 1 'solid)
                (send dc draw-line (+ x half-square) (+ y square-size) (+ x half-square) y)
                (send dc draw-line (+ x square-size) (+ y half-square) x (+ y half-square)))]))

(define dc (send canvas get-dc))

(send frame show #t)
