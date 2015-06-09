#lang racket
; Defines a number of file operations on sgf files. An SGF file is a common
; format to store games of Baduk with.
; SGF format: First come a number of lines of settings and game metadata.
; Then comes a single line that contains all of the move information.
; Example of an SGF for an 8-move game:
; 
; (;GM[1]
; KM[6.50]
; RU[Japanese]
; CA[UTF-8]
; ;B[qd];W[dc];B[pp];W[dp];B[de];W[ce];B[cf];W[cd];)
;
; (normally games are hundreds of moves, however).

(require "baduk-base.rkt")
(provide sgf-file->gridpts)

; various constants to aid with the conversion from SGF to gridpoints.
(define ascii-a-val 97)
(define sgf-color-pos 0)
(define sgf-row-pos 2)
(define sgf-col-pos 3)

; parses one move from an SGF file, converting letters
; to coordinate positions. For example, passing sgf-entry as
; B[dd]
; would return the list
; (4 4 B).
(define (parse-sgf-entry sgf-entry)
  (let* ([row (- (char->integer (string-ref sgf-entry sgf-row-pos))
                 ascii-a-val)]
         [col (- (char->integer (string-ref sgf-entry sgf-col-pos))
                 ascii-a-val)]
         [color-identifier (string-ref sgf-entry sgf-color-pos)]
         [color (cond [(char-ci=? color-identifier #\B) "black"]
                      [(char-ci=? color-identifier #\W) "white"])])
    (cons row (cons col color))))

; returns the string of moves from the file at sgf-path
(define (get-moves-as-strings sgf-path)
  (cddr
   (string-split
    (file->string sgf-path)
    ";")))

; returns the moves in the SGF file at sgf-path as a list of gridpts.
(define (sgf-file->gridpts sgf-path)
  (let ([triplets (map parse-sgf-entry 
                       (get-moves-as-strings sgf-path))])
    (for/list ([i (length triplets)])
      (let ([make-gridpt (λ (row-col-color)
                           (displayln row-col-color)
                           (gridpt (+ (car row-col-color) 1)
                                   (+ (car (cdr row-col-color)) 1)
                                   #f
                                   (cddr row-col-color)))])
        (make-gridpt (list-ref triplets i))))))

;(define sgf "kgs-19-2015-05-new/2015-05-27-8.sgf")
;(sgf-file->gridpts sgf)
;(displayln (get-moves-as-strings sgf))
;(displayln "done")
