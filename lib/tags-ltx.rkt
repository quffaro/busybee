#lang racket/base

(require racket/function
                 racket/list
                 racket/class
                 racket/match
                 racket/string
                 racket/sequence)

(require "polytag.rkt"
                 "common-helpers.rkt")

(require txexpr
                 file/md5
                 pollen/decode
                 pollen/core
                 pollen/setup
         pollen/pagetree
                 pollen/cache)

(provide (all-defined-out))

#|
        defunct
|#
(define definitions '())

(define (add-ltx-definition word definition)
  (set! definitions (cons (list word definition) definitions)))

(define (render-definitions)
  (define (render-one-def def)
    (format "~a: ~a" (first def) (second def)))
  `(txt ,(apply string-append
         (map (λ (d) (string-append (render-one-def d) "\n"))
              (reverse definitions)))))

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

(define (ltx-link-decoder inline-txpr)
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

(define (ltx-root attrs elements)
  (define first-pass (decode-elements (get-elements (wrap-comment-section (txexpr 'root null (esc elements)) esc))
                                      #:inline-txexpr-proc (compose1 txt-decode ltx-link-decoder)
                                      #:string-proc (compose1 smart-quotes smart-dashes)
                                      #:exclude-tags '(script style figure txt-noescape)))
  (txexpr 'body null (decode-elements first-pass #:inline-txexpr-proc txt-decode)))

(define (ltx-ignore attrs elem) `(txt ""))

(define (ltx-title attrs elems) `(txt "Title: " ,@elems "}"))
(define (ltx-taxon attrs elems) `(txt "Taxon: " ,@elems "}"))
(define (ltx-author attrs elems) `(txt "Author: " ,@elems "}"))
(define (ltx-import attrs elems) `(txt "Import: " ,@elems "}"))

;; GENERALIZE
(define (ltx-header attrs elems)
        (define the-title (attr-val 'title attrs))
        (define the-taxon (attr-val 'taxon attrs))
        (define the-author (attr-val 'author attrs))
        (if (current-inclusion-context)
                (case (param-render-as)
                  [("part") `(txt "\\ornamento\\part{" ,the-title "}\n")]
                  [("chapter") `(txt "\\ornamento\\chapter{" ,the-title "}\n")]
                  [else `(txt "\\ornamento\\section{" ,the-title "}\n")])
                `(txt "\\begingroup
                          \\centering
                          {\\LARGE\\bf " ,the-title " }\\\\[1em]
                          \\endgroup")))


(define (ltx-p attrs elems) `(txt "" ,@elems "}\n\n"))
(define (ltx-i attrs text) `(txt "{\\itshape " ,@(esc text) "}"))
(define (ltx-em attrs elems) `(txt "\\emph{" ,@(esc elems) "}"))
(define (ltx-b attrs text) `(txt "{\\bfseries " ,@(esc text) "}"))
(define (ltx-caps attrs text) `(txt "{\\scshape " ,@(esc text) "}"))
#| (define (ltx-strike attrs text) `(txt "\\st{" ,@(esc text) "}")) |#
(define (ltx-strike attrs text)
  `(txt "\\marginpar[\raggedleft " ,(attr-val 'left attrs) "]{" ,@(esc text) "}"))

(define (ltx-thm attrs elems) `(txt "\\begin{theorem}" ,@elems "\\end{theorem}"))
(define (ltx-proof attrs elems) `(txt "\\begin{proof}" ,@elems "\\end{proof}"))

(define (ltx-h1 attrs elems #:id [id 0]) `(txt "\\newpage\\section{" ,@elems "}"))
(define (ltx-h2 attrs elems #:id [id 0]) `(txt "\\subsection{" ,@elems "}"))
(define (ltx-h3 attrs elems #:id [id 0]) `(txt "\\subsubsection{" ,@elems "}"))

(define (ltx-$ attrs elems) (apply string-append `("$" ,@elems "$")))
(define (ltx-eq attrs elems) `(txt-noescape "\\begin{equation}" ,@elems "\\end{equation}"))
(define (ltx-tex attrs pkgs elems) `(txt-noescape "\\begin{equation}" ,@elems "\\end{equation}"))

(define (ltx-? attrs elems) `(txt "{\\textbf{Question} " ,@elems "}"))

(define (ltx-qt attrs elems) `(txt "``" ,@elems "\""))
(define (ltx-Qt attrs elems) `(txt "\\begin{quote}" ,@elems "\\end{quote}"))
(define (ltx-newthought attrs elems) `(txt "\\newthought{" ,@(esc elems) "}"))
(define (ltx-epigraph attrs elems) `(txt "\\epigraph{" ,@(esc elems) "}{--- " ,(attr-val 'by attrs) "}"))

(define (ltx-ol attrs elems) `(txt "\\begin{itemize}" ,@elems "\\end{itemize}"))
(define (ltx-ul attrs elems) `(txt "\\begin{enumerate}[itemsep=2pt,parsep=2pt]" ,@elems "\\end{enumerate}"))
(define (ltx-li attrs elems) `(txt "\\item{" ,@elems "}"))

(define (ltx-def attrs elems)
  (add-ltx-definition elems (attr-val 'def attrs))
  `(txt "\\textbf{" ,@elems "}"))

;; TODO generalize:
;;	- lang
;;	- whether you should update
;;	- creating directories and such
(define (ltx-tangle attrs text)
  (define lang (attr-val 'lang attrs))
  (define exists (attr-val 'exists attrs))
  (define filename (attr-val 'filename attrs))
  ;;
  (define out (open-output-file #:exists 'append (string->path filename)))
  (display (string-join text) out)
  (close-output-port out))

(define (ltx-code attrs text)
  (ltx-tangle attrs text)
  `(txt "\\texttt{"
        ,@(esc (list (string-replace (apply string-append text) "\\" "\\textbackslash ")))
        "}"))

#| (define (ltx-code attrs elems) `(txt "\\texttt{" ,@elems "}")) |#
(define (ltx-pre attrs text)
  (define filename (attr-val 'filename attrs))
  (ltx-tangle attrs text) ;; TODO
  (define caption
          ; Note that using title= instead of caption= prevents listings from showing up in
          ; the "List of Listings" in the table of contents
          (if (string>? filename "") (string-append "[title={" filename "}]") ""))
  `(txt-noescape "\\begin{lstlisting}" ,caption "\n" ,@text "\n\\end{lstlisting}"))

(define current-inclusion-context (make-parameter #f))
(define param-render-as (make-parameter #f))

; TODO filepath is a misnomer
; TODO want better error handling if file is not in pagetree
(define (ltx-include attrs file)
  (define mode (attr-val 'mode attrs))
  #| (displayln (get-pagetree (build-path (current-directory-for-user) "pdf.ptree"))) |#
  (define filepath (symb-match-substring
        (get-pagetree (build-path (current-directory-for-user) "pdf.ptree")) (car file)))
  (displayln file)
  (displayln filepath)
  #| (displayln (car filepath)) |#
  (if (attr-val 'flat attrs)
        `(txt "\\include{" ,(path->string
                               (path-replace-extension
                                 (symbol->string (car filepath)) #".tex")) "}")
        `(@ ,@(cdr (parameterize ([current-inclusion-context #t] [param-render-as mode])
                                 (get-doc (car filepath)))
                           ))))
; TODO need better error handling. "car" fails if there's no file. but it's better to raise an error.

(define (ltx-link url attrs elems) `(zlink ,url ,@elems))
(define (ltx-lank attrs elems) `(txt "[" ,@elems "]"))

(define (ltx-comment attrs contents)
  (check-required-attributes 'comment '(author datetime authorlink) attrs)
  (let ([author (attr-val 'author attrs)]
        [comment-date (attr-val 'datetime attrs)])
       `(txt-comment "\\begin{quote}\n" ,@(esc contents)
                     "\n\\attrib{" ,(ltx-escape-str author) ", " ,comment-date "}"
                     "\n\\end{quote}\n\n")))

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
; TODO table needs to wrap
