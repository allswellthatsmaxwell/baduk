#lang racket/gui
(require racket/gui/base)

(define board-size 19)
(define frame (new frame%
                   [label "board"]
                   [width 1000]
                   [height 1000]))

(define panel (new panel%
                   [parent frame]
                   [style (list 'border)]
                   [horiz-margin 101]
                   [vert-margin 101]))

(new canvas%
     [parent panel]
     [paint-callback
      (lambda (canvas dc)
       ; draw 19 lines both ways. ( 


(send dc draw-line 0 0 1000 1000)

(send frame show #t)
