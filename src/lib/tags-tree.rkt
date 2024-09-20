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

(define (tree-decode-paras xs)
  (define (linebreaks xs) (decode-linebreaks xs "\n"))
  (decode-paragraphs xs (λ (x) (tree-p null x))
					 #:linebreak-proc linebreaks))

(define (tree-root attrs elements)
  (define first-pass (decode-elements elements
                                      #:txexpr-elements-proc tree-decode-paras
									  #:exclude-tags '(header)))
  (define second-pass (decode-elements first-pass
                                       #:string-proc (compose1 smart-quotes smart-dashes)))
  (txexpr 'body null second-pass))

(define (tree-title attrs elems) (apply string-append `("\\title{" ,@elems "}")))
(define (tree-taxon attrs elems) (apply string-append `("\\taxon{" ,@elems "}")))
(define (tree-author attrs elems) (apply string-append `("\\author{" ,@elems "}")))
(define (tree-import attrs elems) (apply string-append `("\\import{" ,@elems "}")))
(define (tree-header attrs elems) 
  (define header-val (format "\\title{~a}\n\\taxon{~a}\n\\author{~a}\n\\import{~a}\n\n" 
										  (attr-val 'title attrs) (attr-val 'taxon attrs)
										  (attr-val 'author attrs) (attr-val 'import attrs)))
  `(header ,header-val))

(define (tree-p attrs elems) `(txt "\\p{" ,@elems "}\n\n"))
(define (tree-b attrs elems) (apply string-append `("\\strong{" ,@elems "}")))
(define (tree-em attrs elems) (apply string-append `("\\em{" ,@elems "}")))
(define (tree-i attrs elems) (apply string-append `("\\em{" ,@elems "}")))
(define (tree-caps attrs elems) (apply string-append `("\\strong{" ,@elems "}")))
(define (tree-strike attrs elems) (apply string-append `("\\strong{" ,@elems "}")))

(define (tree-thm attrs elems) (apply string-append `("\\p{" "\\strong{Theorem} " ,@elems "}")))
(define (tree-proof attrs elems) (apply string-append `("\\p{" "\\emph{Proof:} " ,@elems "}")))

#| (define (tree-h1 attrs elems #:id [id 0]) (apply string-append `("\\p{" ,@elems "}"))) |#
#| (define (tree-h2 attrs elems #:id [id 0]) (apply string-append `("\\p{" ,@elems "}"))) |#
#| (define (tree-h3 attrs elems #:id [id 0]) (apply string-append `("\\p{" ,@elems "}"))) |#

(define (tree-$ attrs elems) (apply string-append `("#{" ,@elems "}")))
(define (tree-eq attrs elems) (apply string-append `("##{" ,@elems "}")))

(define (tree-? attrs elems) (apply string-append `("{\\textbf{Question} " ,@elems "}")))

(define (tree-qt attrs elems) (apply string-append `("\"" ,@elems "\"")))
(define (tree-Qt attrs elems) (apply string-append `("\\blockquote{" ,@elems "}")))

(define (tree-ol attrs elems) (apply string-append `("\\ol{" ,@elems "}")))
(define (tree-ul attrs elems) (apply string-append `("\\ul{" ,@elems "}")))
(define (tree-li attrs elems) (apply string-append `("\\li{" ,@elems "}")))

(define (tree-def attrs elems) (apply string-append `("\\strong{" ,@elems "}")))
(define (tree-code attrs elems) (apply string-append `("\\code{" ,@elems "}")))
(define (tree-pre attrs elems) (apply string-append `("\\pre{" ,@elems "}")))

(define (tree-include attrs elems) (apply string-append `("\\transclude{" ,@elems "}")))
(define (tree-link url attrs elems) (apply string-append `("[" ,@elems "](" ,@url ")")))
