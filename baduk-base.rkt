(define board-size 19)
(define liberty-directions ('N 'S 'E 'W))
(define all-directions (list liberty-directions ('NE 'NW 'SE 'SW)))
(define compass-dir-to-shifts (hash 'N (lambda (row col) (list (+ row 1) col))
                                    'S (lambda (row col) (list (- row 1) col))
                                    'E (lambda (row col) (list row (- col 1)))
                                    'W (lambda (row col) (list row (+ col 1)))
                                    'NE (lambda (row col) (list (+ row 1) (- col 1)))
                                    'NW (lambda (row col) (list (+ row 1) (+ col 1)))
                                    'SE (lambda (row col) (list (- row 1) (- col 1)))
                                    'SW (lambda (row col) (list (- row 1) (+ col 1)))

; one point on the board. gridpt-is-empty? <=> (null? gridpt-color) is true
(struct gridpt (row
                col
                [is-empty? #:mutable]
                [color #:mutable]))

; constructs an empty board gridpts, size board-size x board-size. each
; component gridpt is initially empty and has color null.
(define board (build-list board-size
                          (lambda (row) (build-list board-size
                                               (lambda (col) (gridpt row
                                                                col
                                                                #t
                                                                null))))))

(define (opposite-color color)
  (cond
    [(equal? color 'white) 'black]
    [(equal? color 'black) 'white]
    [(null? color) null]
    [else (error "color is not black or white")]))

(define (get-gridpt-by-coords row col board)
  (list-ref (list-ref board row)
            col))

(define (fully-surrounded gridpt)
  

; returns true if gridpt-color gridpt is equal to color, false otherwise
(define (bordered-above-by-color gridpt color board)
  (let ([gridpt-above (get-gridpt-by-coords (gridpt-row gridpt)
                                            (gridpt-col gridpt)
                                            board)])
    (equal? (gridpt-color gridpt-above)
            color)))
