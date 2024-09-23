#lang racket/base

(require racket/function
		 racket/list
		 racket/class
		 racket/match
		 racket/string)

(require "polytag.rkt"
		 "common-helpers.rkt")
		 
(require txexpr
		 file/md5
		 pollen/decode
		 pollen/core
		 pollen/setup)

(provide (all-defined-out))

(define (pdf-root attrs elements)
  (txexpr 'body null elements))

(define (pdf-title attrs elems) (apply string-append `("Title: " ,@elems "}")))
(define (pdf-taxon attrs elems) (apply string-append `("Taxon: " ,@elems "}")))
(define (pdf-author attrs elems) (apply string-append `("Author: " ,@elems "}")))
(define (pdf-import attrs elems) (apply string-append `("Import: " ,@elems "}")))
(define (pdf-header attrs elems) 
  (define header-val (format "\\title{~a, ~a}\n\\author{~a}\n\\usepackage{~a}\n\n" 
										  (attr-val 'title attrs) (attr-val 'taxon attrs)
										  (attr-val 'author attrs) (attr-val 'import attrs)))
  `(header ,header-val))

(define (pdf-p attrs elems) `(txt "" ,@elems "}\n\n"))
(define (pdf-b attrs elems) (apply string-append `("\\textbf{" ,@elems "}")))
(define (pdf-em attrs elems) (apply string-append `("\\textit{" ,@elems "}")))
(define (pdf-i attrs elems) (apply string-append `("\\textit{" ,@elems "}")))
(define (pdf-caps attrs elems) (apply string-append `("\\textit{" ,@elems "}")))
(define (pdf-strike attrs elems) (apply string-append `("\\textbf{" ,@elems "}")))

(define (pdf-thm attrs elems) (apply string-append `("\\begin{theorem}" ,@elems "\end{theorem}")))
(define (pdf-proof attrs elems) (apply string-append `("\\begin{proof}" ,@elems "\end{proof}")))

#| (define (pdf-h1 attrs elems #:id [id 0]) (apply string-append `("\\p{" ,@elems "}"))) |#
#| (define (pdf-h2 attrs elems #:id [id 0]) (apply string-append `("\\p{" ,@elems "}"))) |#
#| (define (pdf-h3 attrs elems #:id [id 0]) (apply string-append `("\\p{" ,@elems "}"))) |#

(define (pdf-$ attrs elems) (apply string-append `("$" ,@elems "}")))
(define (pdf-eq attrs elems) (apply string-append `("\\begin{equation}" ,@elems "\end{equation}")))

(define (pdf-? attrs elems) (apply string-append `("{\\textbf{Question} " ,@elems "}")))

(define (pdf-qt attrs elems) (apply string-append `("\"" ,@elems "\"")))
(define (pdf-Qt attrs elems) (apply string-append `("\\begin{quote}" ,@elems "\end{quote}")))

(define (pdf-ol attrs elems) (apply string-append `("\\begin{itemize}" ,@elems "\end{itemize}")))
(define (pdf-ul attrs elems) (apply string-append `("\\begin{enumerate}" ,@elems "\end{enumerate}")))
(define (pdf-li attrs elems) (apply string-append `("\\item{" ,@elems "}")))

(define (pdf-def attrs elems) (apply string-append `("\\textbf{" ,@elems "}")))
(define (pdf-code attrs elems) (apply string-append `("\\texttt{" ,@elems "}")))
(define (pdf-pre attrs elems) (apply string-append `("\\begin{verbatim}" ,@elems "\end{verbatim}")))

(define (pdf-include attrs elems) (apply string-append `("\\include{" ,@elems "}")))
(define (pdf-link url attrs elems) (apply string-append `("[" ,@elems "](" ,@url ")")))
