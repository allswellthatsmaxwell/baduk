
#lang racket/gui
(require racket/gui/base)
(require "./Documents/cse/baduk/baduk-base.rkt")

(define frame-size 1000)
(define margin-size 101)
(define panel-size (- frame-size (* margin-size 2)))
(define padding board-size) ; margin around edges of go board
(define square-size (/ (- panel-size padding) board-size))

(define frame (new frame%
                   [label "board"]
                   [width frame-size]
                   [height frame-size]))

(define panel (new panel%
                   [parent frame]
                   [style (list 'border)]
                   [horiz-margin (- margin-size padding)]
                   [vert-margin (- margin-size padding)]))

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
          (place-stone board (- row 2) (- col 2) current-player-color)
          (send dc set-brush current-player-color 'solid)
          (set! current-player-color (switch-turn-color current-player-color))
          (send dc draw-ellipse center-x center-y square-size square-size))))
    (super-new)))

(define canvas (new go-canvas%
                    [parent panel]
                    [paint-callback ; draws board-size x board-size grid with margins around edges
                     (lambda (canvas dc)
                       (letrec ([draw-grid (lambda (x y)
                                             (cond
                                               [(< y 0) '()]
                                               [else
                                                (send dc draw-line padding y panel-size y)
                                                (send dc draw-line x padding x panel-size)
                                                (draw-grid (- x square-size) (- y square-size))]))])
                         (draw-grid panel-size panel-size)))]))

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

(define dc (send canvas get-dc))

(send frame show #t)
