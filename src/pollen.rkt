#lang racket

(require pollen/decode
         pollen/setup       ; For current-poly-target
         pollen/file        ; get-markup-source
         pollen/core
         pollen/private/version
         txexpr
         pollen/tag         ; default-tag-function
         "lib/polytag.rkt"
         "lib/tags-html.rkt"
         "lib/tags-tree.rkt"
		 "lib/tags-ltx.rkt"
		 "lib/tags-pdf.rkt")

(provide string-split
         pollen:version
         (all-from-out "lib/tags-tree.rkt"))
(provide (all-defined-out))
(provide for/s)

(module setup racket/base
  (require "lib/target.rkt")
  (provide (all-defined-out)
           poly-targets)

 (require syntax/modresolve racket/runtime-path)
  (define-runtime-path lib/common-helpers.rkt "lib/common-helpers.rkt")
  (define-runtime-path lib/polytag.rkt "lib/polytag.rkt")
  (define-runtime-path lib/tags-html.rkt "lib/tags-html.rkt")
  (define-runtime-path lib/tags-tree.rkt "lib/tags-tree.rkt")
  (define-runtime-path lib/tags-ltx.rkt "lib/tags-ltx.rkt")
  (define-runtime-path lib/tags-pdf.rkt "lib/tags-pdf.rkt")
  
  (define cache-watchlist
    (map resolve-module-path
         (list lib/common-helpers.rkt
               lib/polytag.rkt
               lib/tags-html.rkt
               lib/tags-tree.rkt
			   lib/tags-ltx.rkt
			   lib/tags-pdf.rkt))))

(poly-branch-tag root)

(poly-branch-tag title)
(poly-branch-tag taxon)
(poly-branch-tag author)
(poly-branch-tag import)
(poly-branch-tag header (title "") (taxon "template") (author "Matt Cuffaro") (import ""))

; simple tag with no required or default attributes
(poly-branch-tag p)
(poly-branch-tag b)
(poly-branch-tag em)
(poly-branch-tag i)
(poly-branch-tag caps)
(poly-branch-tag strike)

(poly-branch-tag thm)
(poly-branch-tag proof)

#| (poly-branch-tag h1) |#
#| (poly-branch-tag h2) |#
#| (poly-branch-tag h3) |#

(poly-branch-tag $)
(poly-branch-tag eq)

(poly-branch-tag ?)

(poly-branch-tag qt)
(poly-branch-tag Qt)

(poly-branch-tag ol)
(poly-branch-tag ul)
(poly-branch-tag li)

(poly-branch-tag def)
(poly-branch-tag code)
(poly-branch-tag pre)

(poly-branch-tag include)
(poly-branch-tag link url)

(provide for/s)
(define-syntax (for/s stx)
  (syntax-case stx ()
    [(_ thing listofthings result-expr ...)
     #'(for/splice ([thing (in-list listofthings)]) result-expr ...)]))
