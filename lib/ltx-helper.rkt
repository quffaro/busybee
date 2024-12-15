#lang racket

(provide generate-all-math-letters)

(define letters
  (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define (generate-math-letter-cmd cmd letter #:func [f identity])
  (format "\\providecommand{\\~a}{\\~a{~a}}"
    (f letter) cmd letter))

(define (generate-math-letter-command cmd letter #:fst [fst identity] #:snd [snd identity])
  (format "\\providecommand{\\~a~a}{\\~a{~a}}"
    (fst letter) (snd letter) cmd letter))

(define (generate-all-math-letters cmd #:func [f identity] #:transform [g identity])
  (define commands
    (map (lambda (letter)
           (generate-math-letter-cmd cmd (g letter) #:func f))
         letters))
  (string-join commands "\n"))
