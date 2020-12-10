#lang racket

(require "load-gamestate.rkt" "manage-gamestate.rkt" "descendants.rkt")

(define gs (load-gamestate "gamestate.sexp"))

(define pid (get-player-id gs))
(define p (get-character gs pid))