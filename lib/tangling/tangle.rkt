#lang racket

(provide tangle-source
         tangle-block
         tangle-file)

;; Regular expression to match code blocks with language and filename specifications
;; Example: ◊code[#:lang "python" #:file "example.py"]{...}
(define code-block-pattern
  #px"◊code\\[#:lang\\s+\"([^\"]+)\"\\s+#:file\\s+\"([^\"]+)\"\\]{([^}]+)}")

;; Extract a single code block and return its language, target file, and content
(define (tangle-block block-text)
  (define matches (regexp-match code-block-pattern block-text))
  (if matches
      (let ([lang (second matches)]
            [file (third matches)]
            [content (fourth matches)])
        (values lang file (string-trim content)))
      (values #f #f #f)))

;; Process source text and extract all code blocks
(define (tangle-source source-text)
  (define code-blocks (make-hash))
  
  ;; Find all code blocks in the source
  (let loop ([start 0])
    (define matches (regexp-match-positions code-block-pattern source-text start))
    (when matches
      (define block-text (substring source-text (caar matches) (cdar matches)))
      (define-values (lang file content) (tangle-block block-text))
      
      ;; If valid block found, add to our collection
      (when (and lang file content)
        (hash-set! code-blocks file
                   (cons (cons lang content)
                         (hash-ref code-blocks file '()))))
      
      (loop (cdar matches))))
  
  code-blocks)

;; Write extracted code blocks to their respective files
(define (write-code-blocks blocks output-dir)
  (hash-for-each
   blocks
   (λ (file contents)
     (define output-path (build-path output-dir file))
     (define output-dir-path (path-only output-path))
     
     ;; Create directory if it doesn't exist
     (when output-dir-path
       (make-directory* output-dir-path))
     
     ;; Write all blocks for this file
     (with-output-to-file output-path
       #:exists 'replace
       (λ ()
         (for ([block (reverse contents)])
           (printf "# Language: ~a\n" (car block))
           (display (cdr block))
           (newline)))))))

;; Main function to tangle a file
(define (tangle-file input-path output-dir)
  (define source (file->string input-path))
  (define blocks (tangle-source source))
  (write-code-blocks blocks output-dir))

;; Example usage:
#|
(tangle-file "document.html.pm" "output")

In your Pollen source file (document.html.pm):

◊code[#:lang "python" #:file "example.py"]{
def hello():
    print("Hello, world!")
}

◊code[#:lang "racket" #:file "util.rkt"]{
#lang racket
(provide greet)
(define (greet name)
  (printf "Hello, ~a!\n" name))
}
|#
