#lang racket
(provide reshape)

(define m '((1 2 3)
(4 5 6)))

;Reshape Matrix
(define (reshape m r c)
  (if (<= (* c r) (length (iter m)))
  (split-list (/ (length (iter m)) r) (iter m))
  (error "message")))

(define (iter lst [acc '()])
  (if [null? lst]
       acc
      (iter (cdr lst) (append acc (car lst)))))

(define (split-list n lst)
  (define (iter l k segment)
    (cond
      ([null? l] (list segment))
      ([zero? k] (cons segment (iter l n '())))
      (else (iter (cdr l) (- k 1) (append segment (list (car l)))))))
  (iter lst n '()))