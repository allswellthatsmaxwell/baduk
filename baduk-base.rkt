#lang racket
(define board-size 19)
(define liberty-directions (list 'N 'S 'E 'W))
(define all-directions (append liberty-directions (list 'NE 'NW 'SE 'SW)))
(define compass-dir-to-shifts (hash 'N (lambda (row col) (list (+ row 1) col))
                                    'S (lambda (row col) (list (- row 1) col))
                                    'E (lambda (row col) (list row (- col 1)))
                                    'W (lambda (row col) (list row (+ col 1)))
                                    'NE (lambda (row col) (list (+ row 1) (- col 1)))
                                    'NW (lambda (row col) (list (+ row 1) (+ col 1)))
                                    'SE (lambda (row col) (list (- row 1) (- col 1)))
                                    'SW (lambda (row col) (list (- row 1) (+ col 1)))))

(define (count-liberties gridpt)
  (length (filter (lambda (dir) (not (bordered-in-dir-by-color gridpt
                                                          dir
                                                          (opposite-color (gridpt-color gridpt)))))
                  liberty-directions)))

(define (fully-surrounded gridpt)
  (= (count-liberties gridpt) 0))

; one point on the board. gridpt-is-empty? <=> (null? gridpt-color) is true
(struct gridpt (row
                col
                [is-empty? #:mutable]
                [color #:mutable])
  #:transparent)

; constructs an empty board gridpts, size board-size x board-size. each
; component gridpt is initially empty and has color null.
(define board (build-list board-size
                          (lambda (row) (build-list board-size
                                               (lambda (col) (gridpt row col #t null))))))

(define (opposite-color color)
  (cond
    [(equal? color 'white) 'black]
    [(equal? color 'black) 'white]
    [(null? color) null]
    [else (error "color is not black or white")]))

(define (get-gridpt-by-coords row col)
  (list-ref (list-ref board row) col))

; returns true if the gridpt one space in direction dir from
; passed gridpt is equal to color, false otherwise
(define (bordered-in-dir-by-color gridpt dir color)
  (let* ([shift-function (hash-ref compass-dir-to-shifts dir)]
         [neighbor-coords (shift-function (gridpt-row gridpt) (gridpt-col gridpt))]
         [row (car neighbor-coords)]
         [col (car (cdr neighbor-coords))]
         [gridpt-neighbor (get-gridpt-by-coords row col)])
    (and (not (gridpt-is-empty? gridpt)) (equal? color (gridpt-color gridpt-neighbor)))))

(define (place-stone row col color)
  (let ([targetpt (get-gridpt-by-coords row col)])
    (cond
      [(not (gridpt-is-empty? targetpt)) (error (string-append "there is already a stone at " (~a targetpt)))]
      [else (set-gridpt-color! targetpt color)
            (set-gridpt-is-empty?! targetpt #f)])))

(define (remove-stone row col)
  (let ([targetpt (get-gridpt-by-coords row col)])
    (set-gridpt-color! targetpt null)
    (set-gridpt-is-empty?! targetpt #t)))
