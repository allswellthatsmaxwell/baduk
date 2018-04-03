#lang racket
; Defines the underlying rules for the game of Baduk. 

(require racket/set)
(provide (except-out (all-defined-out)
                     liberty-directions
                     move-stack))
(provide (struct-out gridpt))

(define board-size 19)
(define liberty-directions (list "N" "S" "E" "W"))

(define move-count 0)
(define move-stack '())

; gridpt represents one point on the board.
(struct gridpt (row
                col
                [has-stone? #:mutable] ; gridpt-has-stone? <=> (null? gridpt-color) is false
                [color #:mutable])
  #:transparent)

; returns the coordinate adjacent to the coordinate at row, col, in direction dir.
(define (get-adjacent-coords row col dir)
  (cond [(equal? dir "N") (cons (+ row 1) col)]
        [(equal? dir "S") (cons (- row 1) col)]
        [(equal? dir "E") (cons row (+ col 1))]
        [(equal? dir "W") (cons row (- col 1))]))

; returns the number of empty adjacent spaces to the coordinate at gridpt.
(define (count-liberties gridpt)
  (length
   (filter
    (lambda (dir) (empty-in-dir? gridpt dir))
    liberty-directions)))

; returns true if gridpt has zero liberties, false otherwise.
(define (fully-surrounded? gridpt)
  (= (count-liberties gridpt) 0))

; constructs an empty board gridpts, size board-size x board-size. each
; component gridpt is initially empty and has color null.
(define (make-board)
  (build-list (+ board-size 1)
              (lambda (row) (build-list (+ board-size 1)
                                   (lambda (col) (gridpt row col #f null))))))

(define board (make-board))

; given white or black, returns the other color. Raises error if
; color is not any of black or white or null.
(define (opposite-color color)
  (cond
    [(equal? color "white") "black"]
    [(equal? color "black") "white"]
    [else (error "color is not black or white")]))

; returns the gridpt with coordinates row, col. If no such point, returns null.
(define (get-gridpt-by-coords board row col)
  (with-handlers ([exn:fail? (lambda (exn) '())])
    (list-ref (list-ref board row) col)))

; returns true if the gridpt one space in direction dir from
; passed gridpt is equal to color, false otherwise
(define (bordered-in-dir-by-color? gridpt dir color)
  (let ([neighbor (get-neighbor-of-gridpt gridpt dir)])
    ((and (not (null? neighbor))
          (and (gridpt-has-stone? neighbor)
               (equal? color (gridpt-color neighbor)))))))

; returns neighbor of gridpt in direction dir.
(define (get-neighbor-of-gridpt gridpt dir)
  (let* ([neighbor-coords (get-adjacent-coords (gridpt-row gridpt)
                                               (gridpt-col gridpt)
                                               dir)] ; changed to -1s for neighbors problem
         [row (car neighbor-coords)]
         [col (cdr neighbor-coords)])
    (displayln "IN GET-NEIGHBOR-OF-GRIDPT")
    (get-gridpt-by-coords board row col)))

; returns white if the stone adjacent to gridpt in direction dir is white,
;         black,                                                    black,
; or      null                                                      null.
(define (friend-in-dir? gridpt dir)
  (let ([neighbor (get-neighbor-of-gridpt gridpt dir)])
    (and (not (null? neighbor))
         (equal? (gridpt-color neighbor) (gridpt-color gridpt)))))

; returns true iff the point one step in direction dir is occupied by
; a stone with color opposite of gridpt.
(define (opponent-in-dir? gridpt dir)
  (let ([neighbor (get-neighbor-of-gridpt gridpt dir)])
    (and (not (null? neighbor))
         (and (gridpt-has-stone? neighbor)
              (equal? (gridpt-color neighbor) (gridpt-color gridpt))))))

; returns true iff the point one step in direction dir is unoccupied.
(define (empty-in-dir? gridpt dir)
  (let ([neighbor (get-neighbor-of-gridpt gridpt dir)])
    (and (not (null? neighbor))
         (not (gridpt-has-stone? neighbor)))))

; returns true iff gridpt-1 and gridpt-2 are occupied and of opposite color.
(define (opponents? gridpt-1 gridpt-2)
  (and (not (null? gridpt-1))
       (and (not (null? gridpt-2))
            (and (gridpt-has-stone? gridpt-1)
                 (and (gridpt-has-stone? gridpt-2)
                      (and (not (equal? (gridpt-color gridpt-1)
                                        (gridpt-color gridpt-2)))))))))

; place stone of color color at row, col. If a stone is already at location,
; raises error.
(define (place-stone board row col color)
  (let ([targetpt (get-gridpt-by-coords board row col)])
    (displayln targetpt) ; debug
    (cond
      [(gridpt-has-stone? targetpt)
       (error (string-append "there is already a stone at " (~a targetpt)))]
      [else (set-gridpt-color! targetpt color)
            (set-gridpt-has-stone?! targetpt #t)
            (set! move-count (+ 1 move-count))
            (set! move-stack (cons targetpt move-stack))])))

; place gridpt on the board
(define (place-stone-by-gridpt board gridpt)
  (place-stone board gridpt-row gridpt-col gridpt-color))

; take gridpt off of whatever board it's on. If no stone exists, does nothing.
(define (remove-stone gridpt)
  (set-gridpt-color! gridpt null)
  (set-gridpt-has-stone?! gridpt #f))

; undoes the previous move.
(define (pop-gridpt)
  (let ([stone (car move-stack)])
    (remove-stone (car move-stack))
    (set! move-stack (cdr move-stack))
    (set! move-count (- move-count 1))
    stone))

; returns the gridpt placed at turn number "turn".
(define (get-move-by-turn-placed turn)
  (list-ref move-stack turn))

; returns the gridpt one step in direction dir from row, col.
(define (get-neighbor-by-coords board row col dir)
  (let* ([neighbor-row-col (get-adjacent-coords row col dir)]
         [neighbor-row (car neighbor-row-col)]
         [neighbor-col (cdr neighbor-row-col)])
    (get-gridpt-by-coords board neighbor-row neighbor-col)))

; returns all neighbors of the gridpt at row, col. Neighboring points
; need not be occupied to be included in returned list.
(define (get-neighbors board row col)
  (letrec ([help-get-neighbors
            (lambda (liberty-directions)
              (cond [(null? liberty-directions) '()]
                    [else (cons (get-neighbor-by-coords board row col
                                                        (car liberty-directions))
                                (help-get-neighbors (cdr liberty-directions)))]))])
    (help-get-neighbors liberty-directions)))

; returns a mutable set of stones in the same group as gridpt. This includes gridpt.
; (A "group" is any collection of adjacent stones of the same color).
(define (collect-group board gridpt)
  (letrec
      ([group (mutable-set)]
       [collect-stones
        (lambda (gridpt stones)
          (cond [(and (not (null? gridpt))
                      (and (gridpt-has-stone? gridpt)
                           (not (set-member? stones gridpt))))
                 (set-add! stones gridpt)
                 (let* ([neighbors (get-neighbors board
                                                  (gridpt-row gridpt)
                                                  (gridpt-col gridpt))]
                        [same-color-neighbors (filter (lambda (stone) (equal? (gridpt-color stone)
                                                                         (gridpt-color gridpt)))
                                                      neighbors)])
                   (for-each (lambda (friend) (collect-stones friend stones))
                             same-color-neighbors)
                   (for-each (lambda (friend) (set-add! stones friend))
                             same-color-neighbors))]))])
    (collect-stones gridpt group)
    group))

; group: a set
; returns true iff group has zero liberties.
(define (group-dead? group)
  (null? (filter (lambda (stone) (not (fully-surrounded? stone)))
                 (set->list group))))

; board: a list of list
; row, col: integers, 0 <= each < board-size
; removes every dead group that neighbors the gridpt at row, col on board.
; returns a list of the removed stones (null if no stones removed).
(define (remove-surrounding-stones board row col)
  (letrec ([gridpt (get-gridpt-by-coords board row col)]
           [opposite-color-neighbors (filter
                                      (lambda (neighbor) (opponents? gridpt neighbor))
                                      (get-neighbors board row col))]
           ; checks each adjacent, opposite-color position to gridpt.
           ; If any adjacent groups are dead, removes them.
           ; builds up list of removed stones to return.
           [remove-helper
            (lambda (neighbors)
              (cond [(null? neighbors) '()]
                    [else
                     (let* ([neighbor (car neighbors)]
                            [neighbor-group (set->list (collect-group board neighbor))]
                            [dead-stones (cond [(group-dead? neighbor-group)
                                                (for-each (lambda (stone) (remove-stone stone))
                                                          neighbor-group)
                                                neighbor-group]
                                               [else '()])])
                       (append dead-stones (remove-helper (cdr neighbors))))]))])
    (remove-helper opposite-color-neighbors)))

; ok. dunno if row is actually row or if it's col.
(define (write-row row-list row)
  (for-each displayln (filter gridpt-has-stone? (list-ref row-list row))))

; returns a list of all the occupied gridpts on board.
(define (get-placed-stones board)
  (letrec ([get-stones (lambda (row)
                          (cond [(>= row board-size) '()]
                                [(let ([row-list (list-ref board row)])
                                   (cons (filter (lambda (gridpt) (gridpt-has-stone? gridpt)) row-list)
                                         (get-stones (+ row 1))))]))])
    (flatten
     (filter (lambda (pt) (not (null? pt)))
             (get-stones 0)))))

; returns a new board (list of lists) with each stone from stones placed on it.
(define (populate-board stones)
  (let ([new-board (make-board)])
    (for-each (lambda (gridpt) (let ([row (gridpt-row gridpt)]
                                     [col (gridpt-col gridpt)]
                                     [color (gridpt-color gridpt)])
                                 place-stone new-board row col color))
              stones)
    new-board))

; writes each stone in stones to a file.
(define (write-sgf stones)
  (define out (open-output-file "board.sgf" #:exists 'replace))
  (for-each displayln stones)
  (for-each (lambda (stone) write out stone) stones)
  (close-output-port out))
