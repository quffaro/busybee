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

(define (ltx-title attrs elems) `(txt "Title: " ,@elems "}"))
(define (ltx-taxon attrs elems) `(txt "Taxon: " ,@elems "}"))
(define (ltx-author attrs elems) `(txt "Author: " ,@elems "}"))
(define (ltx-import attrs elems) `(txt "Import: " ,@elems "}"))
(define (ltx-header attrs elems) 
  (define header-val (format "\\title{~a, ~a}\n\\author{~a}\n\\usepackage{~a}\n\n" 
										  (attr-val 'title attrs) (attr-val 'taxon attrs)
										  (attr-val 'author attrs) (attr-val 'import attrs)))
  `(header ,header-val))

(define (ltx-p attrs elems) `(txt "" ,@elems "}\n\n"))
(define (ltx-b attrs elems) `(txt "\\textbf{" ,@elems "}"))
(define (ltx-em attrs elems) `(txt "\\textit{" ,@elems "}"))
(define (ltx-i attrs elems) `(txt "\\textit{" ,@elems "}"))
(define (ltx-caps attrs elems) `(txt "\\textit{" ,@elems "}"))
(define (ltx-strike attrs elems) `(txt "\\textbf{" ,@elems "}"))

(define (ltx-thm attrs elems) `(txt "\\begin{theorem}" ,@elems "\end{theorem}"))
(define (ltx-proof attrs elems) `(txt "\\begin{proof}" ,@elems "\end{proof}"))

#| (define (ltx-h1 attrs elems #:id [id 0]) `(txt "\\p{" ,@elems "}")) |#
#| (define (ltx-h2 attrs elems #:id [id 0]) `(txt "\\p{" ,@elems "}")) |#
#| (define (ltx-h3 attrs elems #:id [id 0]) `(txt "\\p{" ,@elems "}")) |#

(define (ltx-$ attrs elems) `(tex "$" ,@elems "}"))
(define (ltx-eq attrs elems) `(tex "\\begin{equation}" ,@elems "\end{equation}"))
(define (ltx-tex attrs pkgs elems) `(tex "\\begin{equation}" ,@elems "\end{equation}"))

(define (ltx-? attrs elems) `(txt "{\\textbf{Question} " ,@elems "}"))

(define (ltx-qt attrs elems) `(txt "\"" ,@elems "\""))
(define (ltx-Qt attrs elems) `(txt "\\begin{quote}" ,@elems "\end{quote}"))

(define (ltx-ol attrs elems) `(txt "\\begin{itemize}" ,@elems "\end{itemize}"))
(define (ltx-ul attrs elems) `(txt "\\begin{enumerate}" ,@elems "\end{enumerate}"))
(define (ltx-li attrs elems) `(txt "\\item{" ,@elems "}"))

(define (ltx-def attrs elems) `(txt "\\textbf{" ,@elems "}"))
(define (ltx-code attrs elems) `(txt "\\texttt{" ,@elems "}"))
(define (ltx-pre attrs elems) `(txt "\\begin{verbatim}" ,@elems "\end{verbatim}"))

(define (ltx-include attrs elems) `(txt "\\include{" ,@elems "}"))
(define (ltx-link url attrs elems) `(txt "[" ,@elems "](" ,@url ")"))

(define (ltx-td-tag . tx-els) `(txt ,@(esc tx-els)))
(define (ltx-th-tag . tx-els) `(txt ,@(esc tx-els)))
(define (ltx-tr-tag . tx-elems) `(txt ,@(add-between tx-elems " & ") " \\\\\n"))

; A lot of code duplicated between this function and the HTML one.
; Decided to do it this way to get complete independence between the
; HTML and PDF paths.
(define (ltx-table attrs elems)
  (define c-aligns (attr-val 'columns attrs))
  (cond [(not (or (equal? #f c-aligns) (column-alignments-string? c-aligns)))
         (raise-argument-error 'table "#:columns must be a string containing 'l', 'r', or 'c'" (assq 'columns attrs))])

  ; Split the arguments into rows (at "\n"), and split any string values into
  ; separate cells (at "|") and remove extra whitespace.
  (define rows-parsed (for/list ([row (in-list (split-by elems "\n"))])
                                (for/list ([cell (in-list row)])
                                          (if (string? cell)
                                              (map string-trim (filter-not whitespace? (string-split cell "|")))
                                              cell))))

  ; Clean things up using the helper function above
  (define rows-of-cells (map clean-cells-in-row rows-parsed))

  ; Create lists of individual cells using the tag functions defined previously.
  ; These will be formatted according to the current target format.
  ;   LaTeX: '((txt "Cell 1") " & " (txt "Cell 2") "\\\n")
  ;   HTML:  '((td "Cell 1") (td "Cell 2"))
  (define table-rows
    (match-let ([(cons header-row other-rows) rows-of-cells])
      (cons (map ltx-th-tag header-row)
            (for/list ([row (in-list other-rows)])
                      (map ltx-td-tag row)))))

  (define col-args (if (not c-aligns) (make-string (length (first table-rows)) #\l) c-aligns))

  (match-let ([(cons header-row other-rows) rows-of-cells])
    `(txt "\\begin{table}[h!]\n"
          "  \\centering\n"
          "  \\begin{tabular}{" ,col-args "}\n"
          "    \\toprule\n"
          ,(apply ltx-tr-tag header-row)
          "    \\midrule\n"
          ,@(for/list ([row (in-list other-rows)]) (apply ltx-tr-tag row))
          "    \\bottomrule\n"
          "  \\end{tabular}\n"
          "\\end{table}\n")))
