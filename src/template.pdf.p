◊(require racket/file racket/system)
◊(define latex-source ◊string-append{
    \documentclass[a4paper,12pt]{article}
    \usepackage{amsmath,amsfonts}
    \usepackage{tikz-cd}
    \usepackage[english]{babel}
    \usepackage[autostyle]{csquotes}
    \usepackage{epigraph}
    \begin{document}
    ◊(apply string-append (cdr doc))
    \end{document}})
◊(define working-directory
    (make-temporary-file "pollen-latex-work-~a" 'directory))
◊(define temp-ltx-path (build-path working-directory "temp.ltx"))
◊(display-to-file latex-source temp-ltx-path #:exists 'replace)
◊(define command (format "pdflatex -output-directory ~a ~a"
  working-directory temp-ltx-path))
◊(unless (system command) (error "pdflatex: rendering error"))
◊(let ([pdf (file->bytes (build-path working-directory "temp.pdf"))])
   (delete-directory/files working-directory)
   pdf)
