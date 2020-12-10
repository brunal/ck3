#lang racket
; Functions for querying a gamestate.
(provide get-element get-player-id get-character)

(define (assq-cdr v lst)
  (cond
    ((assq v lst) => cdr)
    (else #f)))

; Returns sub-gamestate at `path`. `path` can be an atom or a list of atoms.
; Returns #f if the path is not found.
(define (get-element gamestate path)
  (cond
    ((false? gamestate) #f)
    ((null? path) gamestate)
    ((pair? path) (get-element (assq-cdr (car path) gamestate) (cdr path)))
    (else (assq-cdr path gamestate))))

(define (get-player-id gamestate)
  (get-element gamestate '(meta_data meta_main_portrait id)))


;TODO:  handle dynasties: so we try to stain in main and not lesser houses
; TODO: handle the dead
(define (get-character gamestate id)
  (get-element gamestate (list 'living id)))
