#lang racket
; A CK3 save, once unzipped, is a mapping
; key1=value1
; key2=value3
; ..
; where key can be an unquoted string or an integer
; where value can be:
; a structure, one of
; * a list: { list-val1 list-val2 ... } where each is a structure or a quoted string or a number
; * a mapping: { list-key1=list-val1 list-key2=list-val2 ... } like the top-level
; an atom, one of
; * an unquoted string
; * a date (YYYY.MM?.DD?)
; * a quoted string
; * a number
; rgb color: "rgb { a b c }"
; We parse that into a s-expression, where
; * each mapping is a list of dotted pairs (key . value)
; * each list is a list
; * base values are atoms  # note: we should produce either symbols, number, strings.

(provide open-and-read-ck3-file read-ck3-file read-short-gamestate read-gamestate)

; opens and reads the save at the given path into a sexp.
(define (open-and-read-ck3-file path)
  (read-ck3-file (open-input-file path)))

; reads the given port into a sexp and closes it.
(define (read-ck3-file port)
  (define original-port (current-input-port))
  (current-input-port port)
  (let ((result (read-structure)))
    (close-input-port port)
    (current-input-port original-port)
    result))

(define (skip-whitespace)
  (when (member (peek-char) '(#\tab #\  #\newline) eq?)
    (read-char)
    (skip-whitespace)))

; returns whether we're at the end of a structure ('}'), and consumes it.
; Also accepts eof for closing a structure, which makes it work for the top-level mapping.
(define (consume-structure-end?)
  (let ((ch (peek-char)))
    (cond
      ((eof-object? ch) #t)
      ((eq? ch #\}) (eat-next-char  #\}) #t)
      (else #f))))

; reads the next word and swallows next-char.
(define (read-word-and-eat-next-char next-char)
  (let ((word (read-next-word)))
    (eat-next-char next-char)
    word))

; eats the next character, checking it has the expected value.
(define (eat-next-char  char)
  (let ((ch (read-char)))
    (unless (eq? ch char)
      (error (format "Next char must be ~@c, got ~@c, ~s" char ch (error-context))))))

; reads and parses the next word, skipping whitespace and until the next = or \n or ' ' (unless in quotes).
; Note: quoted strings get unquoted. Upate that when parsing supports generating symbols.
(define (read-next-word)
  (skip-whitespace)
  (define (read-as-list-until sentinels consume? (acc '()))
    (if (member (peek-char) sentinels)
        (if consume? (cons (read-char) acc) acc)
        (read-as-list-until sentinels consume? (cons (read-char) acc))))
  (maybe-parse (list->string (reverse
                (cond
                  ((eq? (peek-char) #\") (read-as-list-until '(#\") #t (list (read-char))))
                  (else (read-as-list-until '(#\= #\newline #\ ) #f)))))))

(define (maybe-parse s)
  (or (string->number s) s))

; tries parsing the list of chars as a string, a number, a date or a symbol.
(define (parse s)
    (cond
     ;((null? lst) (error (format "Gotta parse ~a/~a! ~a" lst s (error-context))))
      ;((null? s) "NULL")
      ((eq? (car (string->list s)) #\") (substring s 1 (sub1 (string-length substring))))
      ((string->number s) => values)
      ((regexp-match
        #px"^([\\d]{3,4})\\.([\\d]{1,2})\\.([\\d]{1,2})$" s) => (lambda (matches) (cons 'date (map string->number (cdr matches)))))
      (else (string->symbol s))))


; reads the value as an atom or a {....} structure or a rgb spec.
; {...} is either a mapping or a list of integers or strings.
; if we have an atom, we return it (as a number or a string).
; if we have rgb { n n n }, return it as ('rgb n n n)
; if we have a mapping, we return a (key . value) list.
; if we have a list, we return it.
(define (read-value)
  (if (eq? (peek-char) #\{)
      (begin (read-char) (read-structure))
      (let ((word (read-next-word)))
        (if (equal? word "rgb")
            (begin (eat-next-char #\ )
                   (eat-next-char #\{)
                   (read-list '(rgb)))
            word))))

; reads the upcoming {...} structure into a mapping or a list depending on
; whether a = can be found after the next word. { already eaten.
(define (read-structure (acc '()))
  (skip-whitespace)
  (cond
    ((consume-structure-end?) (reverse acc))
    ; nested list
    ; ((eq? (peek-char) #\{) (read-char) (cons (read-structure) acc))
    ((eq? (peek-char) #\{) (read-list))
    (else
     (let ((word (read-next-word))
           (next-char (read-char)))
       (read-structure
        (cons
         (cond
         ((eq? next-char #\=) (cons word (read-value)))
         ((eq? next-char #\ ) word)
         (else (error (string-append "char is not equal or space but '" (string next-char) "', " (error-context)))))
         acc))))))

; reads a list of integers, strings or structures.
(define (read-list (acc '()))
  (if (consume-structure-end?)
      (reverse acc)
      (let ((item (read-value)))
        (skip-whitespace)
        (read-list (cons item acc)))))

(define (error-context)
  (let ((position (file-position (current-input-port)))
        (this-line (read-line))
        (next-line (read-line)))
    (format "at position ~s, next bits: ~s ~s" position this-line next-line)))

(define (read-short-gamestate) (open-and-read-ck3-file "/Users/cauet/Documents/gamestate_start"))
(define (read-gamestate) (open-and-read-ck3-file "/Users/cauet/Documents/gamestate"))


; trace lines start with "> > >" or "< < <"
(define *trace* #f)
(define (enough-carets? n)
  (define (carets-count-higher-than lst goal)
    (cond
      ((zero? goal) #t)
      ((null? lst) #f)
      ((member (car lst) '(#\> #\<)) (carets-count-higher-than (cddr lst) (- goal 1)))
      (else #f)))
  (lambda (s) (carets-count-higher-than (string->list s) n)))

(define (trace-if pred)
  (lambda (s) (when (pred s) (display s) (newline))))

(require racket/trace)

(when *trace*
  (trace read-list read-value read-structure)
  (current-trace-notify (trace-if (enough-carets? 6))))
