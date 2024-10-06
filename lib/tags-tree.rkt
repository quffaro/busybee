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

; if there is a line break, a paragraph tag is added. this means tex headers, say, must not have a linebreak between the preceding paragraph or they'll get decoded as new paragraphs. need to fix this
(define (tree-decode-paras xs)
  (define (linebreaks xs) (decode-linebreaks xs "\n"))
  (decode-paragraphs xs (λ (x) (tree-p null x))
					 #:linebreak-proc linebreaks))

(define (tree-root attrs elements)
  (define first-pass (decode-elements elements
                                      #:txexpr-elements-proc tree-decode-paras
									  #:exclude-tags '(header tex table tr th td include)))
  (define second-pass (decode-elements first-pass
                                       #:string-proc (compose1 smart-quotes smart-dashes)
									   #:exclude-tags '(tex)))
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
(define (tree-b attrs elems) `(txt "\\strong{" ,@elems "}"))
(define (tree-em attrs elems) `(txt "\\em{" ,@elems "}"))
(define (tree-i attrs elems) `(txt "\\em{" ,@elems "}"))
(define (tree-caps attrs elems) `(txt "\\strong{" ,@elems "}"))
(define (tree-strike attrs elems) `(txt "\\strong{" ,@elems "}"))

(define (tree-thm attrs elems) `(txt "\\p{" "\\strong{Theorem} " ,@elems "}"))
(define (tree-proof attrs elems) `(txt "\\p{" "\\emph{Proof:} " ,@elems "}"))

(define (tree-h1 attrs elems #:id [id 0]) (apply string-append `("\\p{" ,@elems "}")))
(define (tree-h2 attrs elems #:id [id 0]) (apply string-append `("\\p{" ,@elems "}")))
(define (tree-h3 attrs elems #:id [id 0]) (apply string-append `("\\p{" ,@elems "}")))

(define (tree-$ attrs elems) `(tex ,(apply string-append `("#{" ,@elems "}"))))
(define (tree-eq attrs elems) `(tex ,(format "##{~a}" elems)))
(define (tree-tex attrs elems) `(tex ,(apply string-append `("\\tex{" ,(attr-val 'pkgs attrs) "}{" ,@elems "}"))))

(define (tree-? attrs elems) `(txt "{\\textbf{Question} " ,@elems "}"))

(define (tree-qt attrs elems) `(txt "\"" ,@elems "\""))
(define (tree-Qt attrs elems) `(txt "\\blockquote{" ,@elems "}"))
(define (tree-newthought attrs elems) `(txt "\\p{" ,@elems "}\n\n"))

(define (tree-ol attrs elems) `(txt "\\ol{" ,@elems "}"))
(define (tree-ul attrs elems) `(txt "\\ul{" ,@elems "}"))
(define (tree-li attrs elems) `(txt "\\li{" ,@elems "}"))

(define (tree-def attrs elems) `(txt "\\strong{" ,@elems "}"))
(define (tree-code attrs elems) `(txt "\\code{" ,@elems "}"))
(define (tree-pre attrs elems) `(txt "\\pre{" ,@elems "}"))

(define (tree-include attrs elems) `(include "\\transclude{" ,@elems "}"))

(define (tree-link url attrs tx-elem) 
  `(txt "[" ,@tx-elem "]" ,(if (non-empty-string? url) (string-append "(" url ")") "")))
(define (tree-lank attrs tx-elem) `(txt "[[" ,@tx-elem "]]"))

#| (define (tree-table . elems) (apply string-append `("\\table{" ,@elems "}"))) |#
(define (tree-td-tag . tx-els) `(txt "\\td{" ,@(esc tx-els) "}"))
(define (tree-th-tag . tx-els) `(txt "\\th{" ,@(esc tx-els) "}"))
(define (tree-tr-tag . tx-elems) `(txt "\\tr{" ,@(add-between tx-elems "  ") "}\n"))
  
; A lot of code duplicated between this function and the HTML one.
; Decided to do it this way to get complete independence between the
; HTML and tree paths.
(define (tree-table attrs elems)
  (define c-aligns (attr-val 'columns attrs))
  (cond [(not (or (equal? #f c-aligns) (column-alignments-string? c-aligns)))
         (raise-argument-error 'table "#:columns must be a string containing 'l', 'r', or 'c'" (assq 'columns attrs))])

  ; Split the arguments into rows (at "\n"), and split any string values into
  ; separate cells (at "|") and remove extra whitespace.
  (define rows-parsed (for/list ([row (in-list elems)])
                                (for/list ([cell (in-list (filter-not whitespace? (string-split row "|")))])
								  ; TODO will whitespace? fail on txexprs?
                                          (if (string? cell)
                                              (string-trim cell)
                                              cell))))
  
  ; Clean things up using the helper function above
  (define rows-of-cells (filter-not null? (map clean-cells-in-row rows-parsed)))

  ; Create lists of individual cells using the tag functions defined previously.
  ; These will be formatted according to the current target format.
  ;   LaTeX: '((txt "Cell 1") " & " (txt "Cell 2") "\\\n")
  ;   HTML:  '((td "Cell 1") (td "Cell 2"))
  (define table-rows
    (match-let ([(cons header-row other-rows) rows-of-cells])
      (cons (map tree-th-tag header-row)
            (for/list ([row (in-list other-rows)])
                      (map tree-td-tag row)))))
  
  (define col-args (if (not c-aligns) (make-string (length (first table-rows)) #\l) c-aligns))
 
	(match-let ([(cons header-row other-rows) table-rows])
		`(table "\\table{\n"
			,(apply tree-tr-tag header-row)
			,@(for/list ([row (in-list other-rows)]) (apply tree-tr-tag row))
			"}\n\n")))
