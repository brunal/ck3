#lang racket

;(require brag/support)

;(token 'LEFT-BRACKET)
;(token 'RIGHT-BRACKET)
;(token 'EQUAL)

(require br-parser-tools/lex brag/support)

(define (tokenize ip)
  (port-count-lines! ip)
  (define ck3-lexer
    (lexer-src-pos
      (numeric (token 'NUMBER (string->number lexeme)))
      ((concatenation numeric "." numeric "." numeric) (token 'DATE lexeme))
      ((union alphabetic "." "\"" "_") (token 'STRING lexeme))
      ("=" (token 'EQUAL))
      ("{" (token 'LEFT-BRACKET))
      ("}" (token 'RIGHT-BRACKET))
      (whitespace (token 'WHITESPACE #:skip? #t))
      ((eof) (void))
    ))
  (define (next-token) (ck3-lexer ip))
  next-token)

; Parsing this gives:
; Encountered unexpected token of type 'NUMBER (value 5) while parsing 'unknown [line=#f, column=#f, offset=#f]
(define small-input-port (open-input-string "meta_data=5
save_game_version=3
version=\"1.2.1\"
"))

(define sample-input-port (open-input-string "meta_data={
        save_game_version=3
        version=\"1.2.1\"
        portraits_version=3
        meta_date=1170.3.22
        meta_player_name=\"Samrajni Amro la Dame du Gange\"
        meta_title_name=\"Empire Pallava\"
        meta_coat_of_arms={
                pattern=\"pattern_vertical_split_01.dds\"
                color1=yellow
                colored_emblem={
                        color1=orange
                        color2=orange
                        texture=\"ce_tiger_rampant.dds\"
                        instance={
                                scale={ 0.900000 0.900000 }
                        }
                }
        }
}
ironman_manager={
        date=867.1.1
        save_game=\"\"
        save_storage=local
}
date=1170.3.22
"))
(define token-thunk (tokenize small-input-port))

(require "ck3-file-grammar.rkt")

(define stx (parse token-thunk))