#lang racket
; A CK3 save, once unzipped, is a mapping
; key1=value1
; key2=value3
; ..
; where key can be an atom: a string, a number or a date
; where value can be an atom, a list of values, a mapping or a mix of those 2.
; We parse that into a s-expression, where
; * a mapping a is a dotted pair
; * each list is a list
; * atoms are symbols or numbers.
; There is special handling for
; * dates: atoms but turned into ('date y m d)
; * rgb info: turned into '(color rgb r g b) instead of '((color . rgb) (r g b))

(provide open-and-read-ck3-save read-ck3-save read-ck3-save load-gamestate dump-gamestate import-gamestate)

; opens and reads the save at the given path into a sexp.
(define (open-and-read-ck3-save path)
  (read-ck3-save (open-input-file path)))

; reads the given port into a sexp and closes it.
(define (read-ck3-save port)
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

; reads the upcoming {...} structure into a mapping or a list. { already eaten.
(define (read-structure (acc '()))
  (skip-whitespace)
  (if (consume-structure-end?)
      (reverse acc)
      (read-structure
       (let ((word (parse-next-word))
             (next-char (read-char)))
         (cond
           ((eq? word #\{) (cons (read-structure) acc))
           ((eq? next-char #\=) (cons (cons word (read-value)) acc))
           (else (cons word acc)))))))

; reads and parses the next word, skipping whitespace and until the next = or \n or ' ' (unless in quotes).
(define (parse-next-word)
  (skip-whitespace)
  (cond
    ((eq? (peek-char) #\{) (read-char))
    (else
     (define (read-as-list-until sentinels consume? (acc '()))
       (if (member (peek-char) sentinels)
           (if consume? (cons (read-char) acc) acc)
           (read-as-list-until sentinels consume? (cons (read-char) acc))))
     (parse (list->string (reverse
                           (cond
                             ((eq? (peek-char) #\") (read-as-list-until '(#\") #t (list (read-char))))
                             (else (read-as-list-until '(#\= #\newline #\ ) #f)))))))))

; tries parsing the list of chars as a string, a number, a date or a symbol.
(define (parse s)
  (cond
    ((eq? (car (string->list s)) #\") (substring s 1 (sub1 (string-length s))))
    ((string->number s) => values)
    ((regexp-match
      #px"^([\\d]{3,4})\\.([\\d]{1,2})\\.([\\d]{1,2})$" s) => (lambda (matches) (cons 'date (map string->number (cdr matches)))))
    (else (string->symbol s))))

(define (read-value)
  (let ((word (parse-next-word)))
    (cond
      ((eq? word #\{) (read-structure))
      ((eq? word 'rgb) (skip-whitespace) (eat-next-char #\{) (read-structure '(rgb)))
      (else word))))

; eats the next character, checking it has the expected value.
(define (eat-next-char char)
  (let ((ch (read-char)))
    (unless (eq? ch char)
      (error (format "Next char must be ~@c, got ~@c, ~s" char ch (error-context))))))

(define (error-context)
  (let ((position (file-position (current-input-port)))
        (this-line (read-line))
        (next-line (read-line)))
    (format "at position ~s, next bits: ~s ~s" position this-line next-line)))

; returns whether we're at the end of a structure ('}'), and consumes it.
; Also accepts eof for closing a structure, which makes it work for the top-level mapping.
(define (consume-structure-end?)
  (let ((ch (peek-char)))
    (cond
      ((eof-object? ch) #t)
      ((eq? ch #\}) (read-char) #t)
      (else #f))))

(define (dump-gamestate gamestate filename)
  (let ((p (open-output-file filename #:exists 'replace)))
    (write gamestate p)
    (close-output-port p)))

(define (import-gamestate filename)
  (dump-gamestate (open-and-read-ck3-save filename) (string-append filename ".sexp")))

(define (load-gamestate filename)
  (read (open-input-file filename)))