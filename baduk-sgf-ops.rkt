#lang racket
; Defines a number of file operations on sgf files. An SGF file is a common
; format to store games of Baduk with.

(require "baduk-base.rkt")
(provide sgf-file->pairs)
(provide sgf-file->gridpts)

(define ascii-a-val 97)
(define sgf-color-pos 0)
(define sgf-row-pos 2)
(define sgf-col-pos 3)

(define (parse-sgf-entry sgf-entry)
  (let* ([row (- (char->integer (string-ref sgf-entry sgf-row-pos))
                 ascii-a-val)]
         [col (- (char->integer (string-ref sgf-entry sgf-col-pos))
                 ascii-a-val)]
         [color (string-ref sgf-entry sgf-color-pos)])
    (cons row (cons col color))))

(define (get-moves-as-strings sgf-path)
  (cddr
   (string-split
    (file->string sgf-path)
    ";")))

(define (sgf-file->pairs sgf-path)
  (map
   (λ (entry)
     (let* ([row-col-color (parse-sgf-entry entry)]
            [row (car row-col-color)]
            [col (car (cdr row-col-color))])
       (cons row col)))
   (get-moves-as-strings sgf-path)))

(define (sgf-file->gridpts sgf-path)
  (let ([triplets (map parse-sgf-entry 
                       (get-moves-as-strings sgf-path))])
    (for/list ([i (length triplets)])
      (let ([make-gridpt (λ (row-col-color)
                           (displayln row-col-color)
                           (gridpt (+ (car row-col-color) 1)
                                   (+ (car (cdr row-col-color) ) 1)
                                   #f
                                   (cddr row-col-color)))])
        (make-gridpt (list-ref triplets i))))))
