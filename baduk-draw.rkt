#lang racket/gui
(require racket/gui/base)

(define board-size 19)
(define frame-size 1000)
(define margin-size 101)
(define panel-size (- frame-size (* margin-size 2)))
(define square-size (/ panel-size board-size))

(define frame (new frame%
                   [label "board"]
                   [width frame-size]
                   [height frame-size]))

(define panel (new panel%
                   [parent frame]
                   [style (list 'border)]
                   [horiz-margin-size 101]
                   [vert-margin-size 101]))

(new canvas%
     [parent panel]
     [paint-callback
      (lambda (canvas dc) ; draw 19 lines both ways.
        (letrec ([draw-lines-horiz (lambda (y)
                                     (cond
                                       [(<= y 0) '()]
                                       [else (send dc draw-line 0 y frame-size y)
                                             (draw-lines-horiz (- square-size y))]))])
          (draw-lines-horiz board-size)))])


(send frame show #t)
