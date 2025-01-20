#lang racket

(require pollen/decode
         pollen/setup       ; For current-poly-target
         pollen/file        ; get-markup-source
         pollen/core
         pollen/private/version
         txexpr
         pollen/tag
		 pollen/pagetree
		 "lib/common-helpers.rkt")         ; default-tag-function

(provide string-split
         pollen:version)
(provide (all-defined-out))
(provide for/s)

(module setup racket/base
  (require "lib/target.rkt")
  (provide (all-defined-out))
  (define poly-targets '(tree ltx pdf html)))

(define (attr-val key attributes)
  (let ([result (assq key attributes)])
       (if result (second result) #f)))

(define (ltx-escape-str str)
  (identity str))

(define (esc elems)
  (for/list ([e (in-list elems)])
            (if (string? e) (ltx-escape-str e) e)))

(define (pdf-link-decoder inline-txpr)
  (if (eq? 'zlink (get-tag inline-txpr))
      (let ([elems (get-elements inline-txpr)])
           `(txt "\\href{" ,(ltx-escape-str (first elems)) "}"
                 "{" ,@(esc (rest elems)) "}"))
      inline-txpr))

(define-tag-function (root attrs elems)
  (case (current-poly-target)
	[(ltx pdf) (let ()
		(define first-pass (decode-elements (get-elements (wrap-comment-section (txexpr 'root null (esc elems)) esc))
                                      #:inline-txexpr-proc (compose1 txt-decode pdf-link-decoder)
                                      #:string-proc (compose1 smart-quotes smart-dashes)
                                      #:exclude-tags '(script style figure txt-noescape)))
  (txexpr 'body null (decode-elements first-pass #:inline-txexpr-proc txt-decode)))]
	[else elems]))

(define-tag-function (header attrs elems)
  (case (current-poly-target)
	[(ltx pdf)  
        (if (current-inclusion-context)
                (case (param-render-as)
                  [("part") `(txt "\\ornamento\\part{" ,(attr-val 'title attrs) "}\n")]
                  [("chapter") `(txt "\\ornamento\\chapter{" ,(attr-val 'title attrs) "}\n")]
                  [else `(txt "\\ornamento\\section{" ,(attr-val 'title attrs) "}\n")])
                `(txt "\\begingroup
                          \\centering
                          {\\LARGE\\bf " ,(attr-val 'title attrs) " }\\\\[1em]
                          \\endgroup"))]
	[else elems]))

(define-tag-function (ignore attrs elems) `(txt ""))

; simple tag with no required or default attributes
(define-tag-function (p attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt "" ,@elems "}\n\n")]
	[else elems]))

(define-tag-function (b attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt "{\\bfseries " ,@(esc elems) "}")]
	[else elems]))
(define-tag-function (em attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt "{\\emph " ,@(esc elems) "}")]
	[else elems]))
(define-tag-function (i attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt "{\\itshape " ,@(esc elems) "}")]
	[else elems]))
(define-tag-function (caps attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt "{\\scshape " ,@(esc elems) "}")]
	[else elems]))
(define-tag-function (strike attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt "\\marginpar[\raggedleft " ,(attr-val 'left attrs) "]{" ,@(esc elems) "}")]
	[else elems]))

(define-tag-function (thm attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt "\\begin{theorem}" ,@elems "\\end{theorem}")]
	[else elems]))
(define-tag-function (proof attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt "\\begin{proof}" ,@elems "\\end{proof}")]
	[else elems]))

(define-tag-function (h1 attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt "\\newpage\\section{" ,@elems "}")]
	[else elems]))
(define-tag-function (h2 attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt "\\subsection{" ,@elems "}")]
	[else elems]))
(define-tag-function (h3 attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt "\\subsubsection{" ,@elems "}")]
	[else elems]))

(define-tag-function ($ attrs elems)
  (case (current-poly-target)
	[(ltx pdf) (apply string-append `("$" ,@elems "$"))]
	[else elems]))
(define-tag-function (eq attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt-noescape "\\begin{equation}" ,@elems "\\end{equation}")]
	[else elems]))
(define-tag-function (tex attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt-noescape "\\begin{equation}" ,@elems "\\end{equation}")]
	[else elems]))

(define-tag-function (? attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt-noescape "\\textbf{Question}" ,@elems "")]
	[else elems]))

(define-tag-function (qt attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt-noescape "``" ,@elems "\"")]
	[else elems]))
(define-tag-function (Qt attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt-noescape "\\begin{quote}" ,@elems "\\end{quote}")]
	[else elems]))
(define-tag-function (epigraph attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt-noescape "\\epigraph{" ,@(esc elems) "}{--- " ,(attr-val 'by attrs) "}")]
	[else elems]))

(define-tag-function (ol attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt "\\begin{itemize}" ,@elems "\\end{itemize}")]
	[else elems]))
(define-tag-function (ul attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt "\\begin{enumerate}[itemsep=2pt,parsep=2pt]" ,@elems "\\end{enumerate}")]
	[else elems]))
(define-tag-function (li attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt "\\item{" ,@elems "}")]
	[else elems]))

(define-tag-function (def attrs elems)
  (case (current-poly-target)
	[(ltx pdf) 
		#| (add-definition elems (attr-val 'def attrs)) |#
		`(txt "\\textbf{" ,@elems "}")]
	[else elems]))

;; TODO generalize:
;;	- lang
;;	- whether you should update
;;	- creating directories and such
(define (tangle attrs text)
  (define lang (attr-val 'lang attrs))
  (define exists (attr-val 'exists attrs))
  (define filename (attr-val 'filename attrs))
  ;;
  (define out (open-output-file #:exists 'append (string->path filename)))
  (display (string-join text) out)
  (close-output-port out))

(define-tag-function (code attrs elems)
  (case (current-poly-target)
	[(ltx pdf) (
		(tangle attrs elems)
		`(txt "\\texttt{"
			  ,@(esc (list (string-replace (apply string-append elems) "\\" "\\textbackslash "))) "}"))]
	[else elems]))
(define-tag-function (pre attrs elems)
  (case (current-poly-target)
	([ltx pdf] (let ()
		(define filename (attr-val 'filename attrs))
		(tangle attrs elems) ;; TODO
		(define caption
          ; Note that using title= instead of caption= prevents listings from showing up in
          ; the "List of Listings" in the table of contents
          (if (string>? filename "") (string-append "[title={" filename "}]") ""))
			`(txt-noescape "\\begin{lstlisting}" ,caption "\n" ,@elems "\n\\end{lstlisting}")))
	[else elems]))

(define current-inclusion-context (make-parameter #f))
(define param-render-as (make-parameter #f))

(define-tag-function (include attrs file)
  (case (current-poly-target)
	[(ltx pdf) (let () (define mode (attr-val 'mode attrs))
	#| (displayln (get-pagetree (build-path (current-directory-for-user) "pdf.ptree"))) |#
	(define filepath (symb-match-substring
        (get-pagetree (build-path (current-directory-for-user) "pdf.ptree")) (car file)))
	(displayln file)
	(displayln filepath)
	(if (attr-val 'flat attrs)
        `(txt "\\include{" ,(path->string
                               (path-replace-extension
                                 (symbol->string (car filepath)) #".tex")) "}")
        `(@ ,@(cdr (parameterize ([current-inclusion-context #t] [param-render-as mode])
                                 (get-doc (car filepath)))
                           ))))]
	[else file]))

(define-tag-function (link attrs elems)
  (case (current-poly-target)
	[(ltx pdf) `(zlink ,(attr-val attrs 'url) ,@elems)]
	[else elems]))

(define (split-by-element lst elem)
  (define (helper lst acc current)
    (cond
      [(null? lst)                ; If we reach the end of the list
       (reverse (cons (reverse current) acc))]  ; Append the last sublist

      [(equal? (first lst) elem)  ; When we hit the split element
       (helper (rest lst)
               (cons (reverse current) acc)
               '())]              ; Start a new sublist

      [else                       ; Otherwise, keep accumulating the current list
       (helper (rest lst)
               acc
               (cons (first lst) current))]))

  (helper lst '() '()))


(define (td-tag . elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt ,@(esc elems))]
	[else elems]))
(define (th-tag . elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt ,@(esc elems))]
	[else elems]))
(define (tr-tag . elems)
  (case (current-poly-target)
	[(ltx pdf) `(txt ,@(add-between elems " & ") " \\\\\n")]
	[else elems]))

(define-tag-function (table attrs elems)
  (case (current-poly-target)
	[(ltx pdf) (let ()
      ; A lot of code duplicated between this function and the HTML one.
      ; Decided to do it this way to get complete independence between the
      ; HTML and PDF paths.
	  (define c-aligns (attr-val 'columns attrs))
	  (cond [(not (or (equal? #f c-aligns) (column-alignments-string? c-aligns)))
         (raise-argument-error 'table "#:columns must be a string containing 'l', 'r', or 'c'" (assq 'columns attrs))])

  ; we need to figure out how to handle arbitrary xexprs
  (define joined (string-join elems))
  ; TODO split list by newlines
  ; Split the arguments into rows (at "\n"), and split any string values into
  ; separate cells (at "|") and remove extra whitespace.
  (define rows-parsed (for/list ([row (in-list (string-split joined "\n"))])
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
      (cons (map th-tag header-row)
            (for/list ([row (in-list other-rows)])
                      (map td-tag row)))))

  (define col-args (if (not c-aligns) (make-string (length (first table-rows)) #\l) c-aligns))

  (match-let ([(cons header-row other-rows) rows-of-cells])
    `(txt "\\begin{table}[h!]\n"
          "  \\centering\n"
          "  \\begin{tabular}{" ,col-args "}\n"
          "    \\toprule\n"
          ,(apply tr-tag header-row)
          "    \\midrule\n"
          ,@(for/list ([row (in-list other-rows)]) (apply tr-tag row))
          "    \\bottomrule\n"
          "  \\end{tabular}\n"
          "\\end{table}\n")))]
	[else elems]))

(provide for/s)
(define-syntax (for/s stx)
  (syntax-case stx ()
    [(_ thing listofthings result-expr ...)
     #'(for/splice ([thing (in-list listofthings)]) result-expr ...)]))
