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
		 pollen/setup
         pollen/pagetree)

(provide (all-defined-out))

#|
  `txt-decode` is called by root when targeting LaTeX/PDF. It simply returns all
  all elements contained inside ◊txt tag or a ◊txt-noescape tag. ◊txt is not
  intended to be used in normal markup; its sole purpose is to allow other tag
  functions to return LaTeX code as a valid tagged X-expression rather than as a
  naked string.
|#
(define (txt-decode xs)
    (if (member (get-tag xs) '(txt txt-noescape txt-comment))
        (get-elements xs)
        xs))

(define (pdf-link-decoder inline-txpr)
  (if (eq? 'zlink (get-tag inline-txpr))
      (let ([elems (get-elements inline-txpr)])
           `(txt "\\href{" ,(ltx-escape-str (first elems)) "}"
                 "{" ,@(esc (rest elems)) "}"))
      inline-txpr))

; Helper function: escape $, %, #, _ and & for LaTeX
; when not already preceeded by a backslash
(define (ltx-escape-str str)
  (identity str))
  #| (regexp-replace* #px"(?<!\\\\)([$#%&_])" str "\\\\\\1")) |#

; Helper function: escape all strings in a list
(define (esc elems)
  (for/list ([e (in-list elems)])
            (if (string? e) (ltx-escape-str e) e)))

(define (pdf-root attrs elements)
  (define first-pass (decode-elements (get-elements (wrap-comment-section (txexpr 'root null (esc elements)) esc))
                                      #:inline-txexpr-proc (compose1 txt-decode pdf-link-decoder)
                                      #:string-proc (compose1 smart-quotes smart-dashes)
                                      #:exclude-tags '(script style figure txt-noescape)))
  (txexpr 'body null (decode-elements first-pass #:inline-txexpr-proc txt-decode)))

(define (pdf-title attrs elems) `(txt "Title: " ,@elems "}"))
(define (pdf-taxon attrs elems) `(txt "Taxon: " ,@elems "}"))
(define (pdf-author attrs elems) `(txt "Author: " ,@elems "}"))
(define (pdf-import attrs elems) `(txt "Import: " ,@elems "}"))

(define (pdf-header attrs elems) 
	(define the-title (attr-val 'title attrs))
	(define the-taxon (attr-val 'taxon attrs))
	(define the-author (attr-val 'author attrs))
	(if (current-inclusion-context)
		`(txt "\\subsection*{" ,the-title "}\n\\textit{" ,the-taxon "}\n") 
		`(txt "\\begingroup
			  \\centering
			  {\\LARGE\\bf " ,the-title " }\\\\[1em]
			  \\endgroup")))



(define (pdf-p attrs elems) `(txt "" ,@elems "}\n\n"))
(define (pdf-i attrs text) `(txt "{\\itshape " ,@(esc text) "}"))
(define (pdf-em attrs elems) `(txt "\\emph{" ,@(esc elems) "}"))
(define (pdf-b attrs text) `(txt "{\\bfseries " ,@(esc text) "}"))
(define (pdf-caps attrs text) `(txt "\\textit{" ,@(esc text) "}"))
(define (pdf-strike attrs text) `(txt "\\st{" ,@(esc text) "}"))

(define (pdf-thm attrs elems) `(txt "\\begin{theorem}" ,@elems "\end{theorem}"))
(define (pdf-proof attrs elems) `(txt "\\begin{proof}" ,@elems "\end{proof}"))

(define (pdf-h1 attrs elems #:id [id 0]) `(txt "\\section*{" ,@elems "}"))
(define (pdf-h2 attrs elems #:id [id 0]) `(txt "\\subsection*{" ,@elems "}"))
(define (pdf-h3 attrs elems #:id [id 0]) `(txt "\\subsection*{" ,@elems "}"))

(define (pdf-$ attrs elems) `(txt-noescape "$" ,@elems "$")) 
(define (pdf-eq attrs elems) `(txt-noescape "\\begin{equation}" ,@elems "\\end{equation}")) 
(define (pdf-tex attrs pkgs elems) `(txt-noescape "\\begin{equation}" ,@elems "\\end{equation}"))

(define (pdf-? attrs elems) `(txt "{\\textbf{Question} " ,@elems "}"))

(define (pdf-qt attrs elems) `(txt "``" ,@elems "\""))
(define (pdf-Qt attrs elems) `(txt "\\begin{quote}" ,@elems "\\end{quote}"))
(define (pdf-newthought attrs elems) `(txt "\\newthought{" ,@(esc elems) "}"))

(define (pdf-ol attrs elems) `(txt "\\begin{itemize}" ,@elems "\\end{itemize}"))
(define (pdf-ul attrs elems) `(txt "\\begin{enumerate}" ,@elems "\\end{enumerate}"))
(define (pdf-li attrs elems) `(txt "\\item{" ,@elems "}"))

(define (pdf-def attrs elems) `(txt "\\textbf{" ,@elems "}"))

(define (pdf-code attrs text)
  `(txt "\\texttt{"
        ,@(esc (list (string-replace (apply string-append text) "\\" "\\textbackslash ")))
        "}"))
#| (define (pdf-code attrs elems) `(txt "\\texttt{" ,@elems "}")) |#
(define (pdf-pre attrs text)
  (define filename (attr-val 'filename attrs))
  (define caption
          ; Note that using title= instead of caption= prevents listings from showing up in
          ; the "List of Listings" in the table of contents
          (if (string>? filename "") (string-append "[title={" filename "}]") ""))
  `(txt-noescape "\\begin{lstlisting}" ,caption "\n" ,@text "\n\\end{lstlisting}"))
#| (define (pdf-pre attrs elems) `(txt "\\begin{verbatim}" ,@elems "\\end{verbatim}")) |#

(define current-inclusion-context (make-parameter #f))

#| (define (pdf-include attrs file) `(txt-noescape ,@file)) |#
(define (pdf-include attrs file)
  (define filepath (symb-match-substring 
	(get-pagetree (build-path (current-directory-for-user) "pdf.ptree")) (car file)))
  (displayln filepath)
  (if (attr-val 'flat attrs)
	`(txt "\\include{" ,(path->string 
						  (path-replace-extension 
							(symbol->string (car filepath)) #".tex")) "}")
	`(@ ,@(cdr (parameterize ([current-inclusion-context #t])
				 (get-doc (car filepath)))
			   ))))
; TODO need better error handling. "car" fails if there's no file. but it's better to raise an error.

(define (pdf-link url attrs elems) `(zlink ,url ,@elems))
(define (pdf-lank attrs elems) `(txt "[" ,@elems "]"))

(define (pdf-td-tag . tx-els) `(txt ,@(esc tx-els)))
(define (pdf-th-tag . tx-els) `(txt ,@(esc tx-els)))
(define (pdf-tr-tag . tx-elems) `(txt ,@(add-between tx-elems " & ") " \\\\\n"))

; A lot of code duplicated between this function and the HTML one.
; Decided to do it this way to get complete independence between the
; HTML and PDF paths.
(define (pdf-table attrs elems)
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
      (cons (map pdf-th-tag header-row)
            (for/list ([row (in-list other-rows)])
                      (map pdf-td-tag row)))))

  (define col-args (if (not c-aligns) (make-string (length (first table-rows)) #\l) c-aligns))

  (match-let ([(cons header-row other-rows) rows-of-cells])
    `(txt "\\begin{table}[h!]\n"
          "  \\centering\n"
          "  \\begin{tabular}{" ,col-args "}\n"
          "    \\toprule\n"
          ,(apply pdf-tr-tag header-row)
          "    \\midrule\n"
          ,@(for/list ([row (in-list other-rows)]) (apply pdf-tr-tag row))
          "    \\bottomrule\n"
          "  \\end{tabular}\n"
          "\\end{table}\n")))
