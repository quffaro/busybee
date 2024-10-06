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
(define (html-header attrs elems) 
  (define header-val (format "\\title{~a}\n\\taxon{~a}\n\\author{~a}\n\\import{~a}\n\n" 
										  (attr-val 'title attrs) (attr-val 'taxon attrs)
										  (attr-val 'author attrs) (attr-val 'import attrs)))
  `(header ,header-val))

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

(define (html-h1 attrs title) `(h1 ,@title))
(define (html-h2 attrs title) `(h2 ,@title))
(define (html-h3 attrs title) `(h3 ,@title))

; TODO no Latex support
(define (html-$ attrs elems) `(span "\\(" ,@elems "\\)"))
(define (html-eq attrs elems) `(span "\\(" ,@elems "\\"))
(define (html-tex attrs pkgs elems) `(span "\\(" ,@elems "\\"))


(define (html-? attrs elements) `(p ,@elements))

; TODO
(define (html-qt attrs elements) `(p ,@elements))
(define (html-Qt attrs elements) `(blockquote ,@elements))
(define (html-newthought attrs elems)
  `(span [[class "newthought"]] ,@elems))

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
(define (html-lank attrs elems) `(p ,@elems))

#|
detect-newthoughts: called by root above when targeting HTML.
The ◊newthought tag (defined further below) makes use of the \newthought
command in Tufte-LaTeX and the .newthought CSS style in Tufte-CSS to start a
new section with some words in small caps. In LaTeX, this command additionally
adds some vertical spacing in front of the enclosing paragraph. There is no way
to do this in HTML/CSS without adding in some Javascript: i.e., there is no
CSS selector for “p tags that contain a span of class ‘newthought’”. So we can
handle it at the Pollen processing level.
|#
(define (detect-newthoughts block-xpr)
  (define is-newthought? (λ(x) (and (txexpr? x)
                                    (eq? 'span (get-tag x))
                                    (attrs-have-key? x 'class)
                                    (string=? "newthought" (attr-ref x 'class)))))
  (if (and(eq? (get-tag block-xpr) 'p)             ; Is it a <p> tag?
          (findf-txexpr block-xpr is-newthought?)) ; Does it contain a <span class="newthought">?
      (attr-set block-xpr 'class "pause-before")   ; Add the ‘pause-before’ class
      block-xpr))   

#| otherjoel:
  ◊table : allows the creation of basic tables from a simplified notation.
  Modified from ◊quick-table in MB’s Typography for Lawyers source
  (http://docs.racket-lang.org/pollen-tfl/_pollen_rkt_.html#%28elem._%28chunk._~3cquick-table~3e~3a1%29%29)

  I’ve updated this tag so that A. it can produce both LaTeX and HTML tables,
  B. it allows you to specify the text-alignment for columns in both those
  formats, and C. it allows you to include tags inside table cells (not just strings)
|#
(define (html-td-tag . tx-els) `(td ,@tx-els))
(define (html-th-tag . tx-els) `(th ,@tx-els))
(define (html-tr-tag columns . tx-elems)
  (define column-alignments #hash(("l" . "left") ("r" . "right") ("c" . "center")))

  (cons 'tr (for/list ([cell (in-list tx-elems)]
                       [c-a columns])
                      (if (not (equal? c-a #\l))
                          (attr-set cell 'style
                                    (string-append "text-align: "
                                                   (hash-ref column-alignments (string c-a))
                                                   ";"))
                          cell))))

(define (html-table attrs elems)
  (define c-aligns (attr-val 'columns attrs))
  (cond [(not (or (equal? #f c-aligns) (column-alignments-string? c-aligns)))
         (raise-argument-error 'table "#:columns must be a string containing 'l', 'r', or 'c'" (assq 'columns attrs))])


  ;
  ; Split the arguments into rows (at "\n"), and split any string values into
  ; separate cells (at "|") and remove extra whitespace.
  (define rows-parsed (for/list ([row (in-list (split-by elems "\n"))])
                                (for/list ([cell (in-list row)])
                                          (if (string? cell)
                                              (map string-trim (filter-not whitespace? (string-split cell "|")))
                                              cell))))

  ; Clean things up (remove unnecessary levels of sublisting)
  (define rows-of-cells (map clean-cells-in-row rows-parsed))

  ; Create lists of individual cells using the tag functions defined previously.
  ; These will be formatted according to the current target format.
  ;   LaTeX: '((txt "Cell 1") " & " (txt "Cell 2") "\\\n")
  ;   HTML:  '((td "Cell 1") (td "Cell 2"))
  (define table-rows
    (match-let ([(cons header-row other-rows) rows-of-cells])
      (cons (map html-th-tag header-row)
            (for/list ([row (in-list other-rows)])
                      (map html-td-tag row)))))

  (define col-args (if (not c-aligns) (make-string (length (first table-rows)) #\l) c-aligns))
  (cons 'table (for/list ([table-row (in-list table-rows)])
                         (apply html-tr-tag col-args table-row))))
