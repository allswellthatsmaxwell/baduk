#lang racket
(provide (except-out (all-defined-out)
                     liberty-directions))

(define board-size 19)
(define liberty-directions (list "N" "S" "E" "W"))

(define move-count 0)

(define (get-adjacent-coords row col dir)
  (cond [(equal? dir "N") (cons (+ row 1) col)]
        [(equal? dir "S") (cons (- row 1) col)]
        [(equal? dir "E") (cons row (+ col 1))]
        [(equal? dir "W") (cons row (- col 1))]))

; returns the number of liberties gridpt has.
(define (count-libos gridpt)
  (letrec ([count-libies (lambda (liberty-dirs)
                         (cond [(null? liberty-dirs) 0]
                               [else
                                (+ (count-libies (cdr liberty-dirs))
                                   (if (bordered-in-dir-by-color? gridpt
                                                                  (car liberty-dirs)
                                                                  (opposite-color (gridpt-color gridpt)))
                                       0
                                       1))]))])
    (count-libies liberty-directions)))

(define (count-liberties gridpt)
  (length (filter (lambda (dir) (not (bordered-in-dir-by-color? gridpt
                                                          dir
                                                          (opposite-color (gridpt-color gridpt)))))
                  liberty-directions)))

; returns true if gridpt has zero liberties, false otherwise.
(define (fully-surrounded? gridpt)
  (= (count-liberties gridpt) 0))

; one point on the board. gridpt-has-stone? <=> (null? gridpt-color) is false
(struct gridpt (row
                col
                [has-stone? #:mutable]
                [color #:mutable])
  #:transparent)

; constructs an empty board gridpts, size board-size x board-size. each
; component gridpt is initially empty and has color null.
(define (make-board)
  (build-list board-size
              (lambda (row) (build-list board-size
                                   (lambda (col) (gridpt (+ row 1) (+ col 1) #f null))))))

(define board (make-board))

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
  (with-handlers ([exn:fail? (lambda (exn) '())])
    (list-ref (list-ref board row) col)))

; returns true if the gridpt one space in direction dir from
; passed gridpt is equal to color, false otherwise
(define (bordered-in-dir-by-color? gridpt dir color)
  (cond [(null? gridpt) #f]
        [else
         (let* ([neighbor-coords (get-adjacent-coords (- (gridpt-row gridpt) 1) (- (gridpt-col gridpt) 1) dir)] ; changed to -1 for neighbors problem
                [row (car neighbor-coords)]
                [col (cdr neighbor-coords)]
                [neighbor (get-gridpt-by-coords board row col)])
           (and (not (null? neighbor))
                (and (gridpt-has-stone? neighbor) ; changed from gridpt to neighbor
                     (equal? color (gridpt-color neighbor)))))]))

; place stone of color color at row, col. If a stone is already at location,
; raises error.
(define (place-stone board row col color)
  (let ([targetpt (get-gridpt-by-coords board row col)])
    (cond
      [(gridpt-has-stone? targetpt) (error (string-append "there is already a stone at " (~a targetpt)))]
      [else (set-gridpt-color! targetpt color)
            (set-gridpt-has-stone?! targetpt #t)
            (set! move-count (+ 1 move-count))])))

; take gridpt off of whatever board it's on. If no stone exists, does nothing.
(define (remove-stone gridpt)
  (print "removing ")
  (displayln gridpt)
  (set-gridpt-color! gridpt null)
  (set-gridpt-has-stone?! gridpt #f))

(define (get-neighbor board row col dir)
  (let* ([neighbor-row-col (get-adjacent-coords row col dir)]
         [neighbor-row (car neighbor-row-col)]
         [neighbor-col (cdr neighbor-row-col)])
    (get-gridpt-by-coords board neighbor-row neighbor-col)))

; east and north work, west and south do not - they increment both row and col.
(define (collect-single-captures board row col)
  (letrec ([collect-captures
            (lambda (board row col neighbors)
              (cond [(null? neighbors) '()]
                    [else (let ([neighbor (car neighbors)])
                            (cond [(null? neighbor) '()]
                                  [(fully-surrounded? neighbor)
                                   (cons neighbor (collect-captures board row col (cdr neighbors)))]
                                  [else
                                   (display neighbor) (display " not surrounded with ") (display (count-liberties neighbor)) (displayln " libs")
                                   (collect-captures board row col (cdr neighbors))]))]))])
    (collect-captures board row col (get-neighbors board row col liberty-directions))))

; row and col are wrong SOMEWHERE. In get-gridpt-by-coords?!
(define (get-neighbors board row col liberty-directions)
  (cond [(null? liberty-directions) '()]
        [else (cons (get-neighbor board row col (car liberty-directions))
                    (get-neighbors board row col (cdr liberty-directions)))]))

(define (remove-capture-list capture-list)
  (for-each remove-stone capture-list))

(define (switch-turn-color color)
  (opposite-color color))

; ok. dunno if row is actually row or if it's col.
(define (write-row row-list row)
  (for-each displayln (filter gridpt-has-stone? (list-ref row-list row))))

; working on this one now
(define (get-placed-stones board)
  (letrec ([get-stones (lambda (row)
                          (cond [(>= row board-size) '()]
                                [(let ([row-list (list-ref board row)])
                                   (cons (filter (lambda (gridpt) (gridpt-has-stone? gridpt)) row-list)
                                         (get-stones (+ row 1))))]))])
    (filter (lambda (pt) (not (null? pt)))
            (get-stones 0))))

(define (populate-board stones)
  (let ([new-board (make-board)])
    (for-each (lambda (gridpt) (let ([row (gridpt-row gridpt)]
                                [col (gridpt-col gridpt)]
                                [color (gridpt-color gridpt)])
                            place-stone new-board row col color)) stones)
    board))

(define (write-sgf stones)
  (define out (open-output-file "board.sgf" #:exists 'replace))
  (for-each displayln stones)
  (for-each (lambda (stone) write out stone) stones)
  (close-output-port out))
