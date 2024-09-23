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

(define (ellipses x)
  (string-replace x "..." "…"))

(define (ltx-root attrs elements)
  (txexpr 'body null elements))

(define (ltx-title attrs elems) (apply string-append `("Title: " ,@elems "}")))
(define (ltx-taxon attrs elems) (apply string-append `("Taxon: " ,@elems "}")))
(define (ltx-author attrs elems) (apply string-append `("Author: " ,@elems "}")))
(define (ltx-import attrs elems) (apply string-append `("Import: " ,@elems "}")))
(define (ltx-header attrs elems) 
  (define header-val (format "\\title{~a, ~a}\n\\author{~a}\n\\usepackage{~a}\n\n" 
										  (attr-val 'title attrs) (attr-val 'taxon attrs)
										  (attr-val 'author attrs) (attr-val 'import attrs)))
  `(header ,header-val))

(define (ltx-p attrs elems) `(txt "" ,@elems "}\n\n"))
(define (ltx-b attrs elems) (apply string-append `("\\textbf{" ,@elems "}")))
(define (ltx-em attrs elems) (apply string-append `("\\textit{" ,@elems "}")))
(define (ltx-i attrs elems) (apply string-append `("\\textit{" ,@elems "}")))
(define (ltx-caps attrs elems) (apply string-append `("\\textit{" ,@elems "}")))
(define (ltx-strike attrs elems) (apply string-append `("\\textbf{" ,@elems "}")))

(define (ltx-thm attrs elems) (apply string-append `("\\begin{theorem}" ,@elems "\end{theorem}")))
(define (ltx-proof attrs elems) (apply string-append `("\\begin{proof}" ,@elems "\end{proof}")))

#| (define (ltx-h1 attrs elems #:id [id 0]) (apply string-append `("\\p{" ,@elems "}"))) |#
#| (define (ltx-h2 attrs elems #:id [id 0]) (apply string-append `("\\p{" ,@elems "}"))) |#
#| (define (ltx-h3 attrs elems #:id [id 0]) (apply string-append `("\\p{" ,@elems "}"))) |#

(define (ltx-$ attrs elems) (apply string-append `("$" ,@elems "}")))
(define (ltx-eq attrs elems) (apply string-append `("\\begin{equation}" ,@elems "\end{equation}")))

(define (ltx-? attrs elems) (apply string-append `("{\\textbf{Question} " ,@elems "}")))

(define (ltx-qt attrs elems) (apply string-append `("\"" ,@elems "\"")))
(define (ltx-Qt attrs elems) (apply string-append `("\\begin{quote}" ,@elems "\end{quote}")))

(define (ltx-ol attrs elems) (apply string-append `("\\begin{itemize}" ,@elems "\end{itemize}")))
(define (ltx-ul attrs elems) (apply string-append `("\\begin{enumerate}" ,@elems "\end{enumerate}")))
(define (ltx-li attrs elems) (apply string-append `("\\item{" ,@elems "}")))

(define (ltx-def attrs elems) (apply string-append `("\\textbf{" ,@elems "}")))
(define (ltx-code attrs elems) (apply string-append `("\\texttt{" ,@elems "}")))
(define (ltx-pre attrs elems) (apply string-append `("\\begin{verbatim}" ,@elems "\end{verbatim}")))

(define (ltx-include attrs elems) (apply string-append `("\\include{" ,@elems "}")))
(define (ltx-link url attrs elems) (apply string-append `("[" ,@elems "](" ,@url ")")))
