#lang racket/base

(require racket/function
         racket/list
         racket/class
         racket/match
         racket/string)

(require "polytag.rkt"
         "common-helpers.rkt")

(require txexpr
		 pollen/decode
		 pollen/core
		 pollen/setup)

(provide (all-defined-out))

(define (decode-paras-nolinebreaks xs)
	(define (no-linebreaks xs)
		(decode-linebreaks xs " "))
	(decode-paragraphs xs #:linebreak-proc no-linebreaks))

#| (define (html-root attrs elements) |#
#| 	(define first-pass (decode-elements elements |#
#|                                       #:txexpr-elements-proc decode-paras-nolinebreaks |#
#|                                       #:exclude-tags '(script style figure table pre))) |#
#| 	(define second-pass (decode-elements first-pass |#
#|                                        ; see towards end of file for detect-newthoughts |#
#|                                        #1| #:block-txexpr-proc detect-newthoughts |1# |#
#|                                        #:string-proc (compose1 smart-quotes smart-dashes) |#
#|                                        #:exclude-tags '(script style pre code))) |#
#| 	(wrap-comment-section (txexpr 'body null second-pass) identity)) |#

(define (html-root attrs elements)
	(txexpr 'root empty elements))

(define (html-title attrs elems) `(p ,@elems))
(define (html-taxon attrs elems) `(p ,@elems))
(define (html-author attrs elems) `(p ,@elems))
(define (html-import attrs elems) `(p ,@elems))
(define (html-header attrs title taxon author import) `(p ,@title))

;
(define (html-p attrs elems) `(p ,@elems))
(define (html-b attrs text) `(b ,@text))
(define (html-em attrs text) `(em ,@text))
(define (html-i attrs text) `(i ,@text))
(define (html-caps attrs elems) `(span [[class "smallcaps"]] ,@elems))
(define (html-strike attrs text) `(s ,@text))

; TODO
(define (html-thm attrs elems) `(p ,@elems))
(define (html-proof attrs elems) `(p ,@elems))

#| (define (html-h1 attrs title) `(h1 ,@title)) |#
#| (define (html-h2 attrs title) `(h2 ,@title)) |#
#| (define (html-h3 attrs title) `(h3 ,@title)) |#

; TODO no Latex support
(define (html-$ attrs elems) `(span "\\(" ,@elems "\\)"))
(define (html-eq attrs elems) `(span "\\(" ,@elems "\\"))

(define (html-? attrs elements) `(p ,@elements))

; TODO
(define (html-qt attrs elements) `(p ,@elements))
(define (html-Qt attrs elements) `(blockquote ,@elements))

(define (html-ol attrs elements) `(ol ,@elements))
(define (html-ul attrs elements) `(ul ,@elements))
(define (html-li attrs elements) `(li ,@elements))

(define (html-def attrs text) `(p ,@text))
(define (html-code attrs text) `(code ,@text))
(define (html-pre attrs text)
	(define filename (attr-val 'filename attrs))
	(define codeblock `(pre [[class "code"]] ,@text))
		(cond
			[(string>? filename "") `(@ (div [[class "listing-filename"]] 128196 " " ,filename) ,codeblock)]
			[else codeblock]))

(define (html-include attrs text) `(p ,@text))
(define (html-link url attrs elems) `(a [[href ,url]] ,@elems)) 
