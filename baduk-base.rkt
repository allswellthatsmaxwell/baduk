#lang racket
(provide (except-out (all-defined-out)
                     compass-dir-to-shifts
                     liberty-directions))

(define board-size 19)
(define liberty-directions (list 'N 'S 'E 'W))
(define compass-dir-to-shifts (hash 'N  (lambda (row col) (list (+ row 1) col))
                                    'S  (lambda (row col) (list (- row 1) col))
                                    'E  (lambda (row col) (list row       (- col 1)))
                                    'W  (lambda (row col) (list row       (+ col 1)))
                                    'NE (lambda (row col) (list (+ row 1) (- col 1)))
                                    'NW (lambda (row col) (list (+ row 1) (+ col 1)))
                                    'SE (lambda (row col) (list (- row 1) (- col 1)))
                                    'SW (lambda (row col) (list (- row 1) (+ col 1)))))

(define move-count 0)

; returns the number of liberties gridpt has.
(define (count-liberties gridpt)
  (length (filter (lambda (dir) (not (bordered-in-dir-by-color gridpt
                                                          dir
                                                          (opposite-color (gridpt-color gridpt)))))
                  liberty-directions)))

; returns true if gridpt has zero liberties, false otherwise.
(define (fully-surrounded? gridpt)
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

; given white or black, returns the other color. Raises error if
; color is not any of black or white or null.
(define (opposite-color color)
  (cond
    [(equal? color "white") "black"]
    [(equal? color "black") "white"]
    [(null? color) null]
    [else (error "color is not black or white")]))

; returns the gridpt with coordinates row, col
(define (get-gridpt-by-coords board row col)
  (list-ref (list-ref board row) col))

; returns true if the gridpt one space in direction dir from
; passed gridpt is equal to color, false otherwise
(define (bordered-in-dir-by-color gridpt dir color)
  (let* ([shift-function (hash-ref compass-dir-to-shifts dir)]
         [neighbor-coords (shift-function (gridpt-row gridpt) (gridpt-col gridpt))]
         [row (car neighbor-coords)]
         [col (car (cdr neighbor-coords))]
         [gridpt-neighbor (get-gridpt-by-coords board row col)])
    (and (not (gridpt-is-empty? gridpt)) (equal? color (gridpt-color gridpt-neighbor)))))

; place stone of color color at row, col. If a stone is already at location,
; raises error.
(define (place-stone board row col color)
  (let ([targetpt (get-gridpt-by-coords board row col)])
    (cond
      [(not (gridpt-is-empty? targetpt)) (error (string-append "there is already a stone at " (~a targetpt)))]
      [else (set-gridpt-color! targetpt color)
            (set-gridpt-is-empty?! targetpt #f)
            (set! move-count (+ 1 move-count))])))

; take the stone at row, col off of board. If no stone is at location, does nothing.
(define (remove-stone gridpt)
    (set-gridpt-color! gridpt null)
    (set-gridpt-is-empty?! gridpt #t))

(define (shift-dir board row col dir)
  (let ([shift-function (hash-ref compass-dir-to-shifts dir)])
    (shift-function row col)))

(define (get-neighbor board row col dir)
  (let* ([neighbor-row-col (shift-dir board row col (car liberty-directions))]
         [neighbor-row (car neighbor-row-col)]
         [neighbor-col (cdr neighbor-row-col)])
    (get-gridpt-by-coords board neighbor-row neighbor-col)))

(define (check-for-captures board row col liberty-directions)
  (cond [(null? liberty-directions) '()]
        [else (let ([neighbor (get-neighbor board row col (car liberty-directions))])
                (cond
                  [(fully-surrounded? neighbor)
                   (remove-stone neighbor)]
                  (check-for-captures board row col (cdr liberty-directions))))]))

(define (switch-turn-color color)
  (opposite-color color))
