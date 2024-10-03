◊(require racket/file racket/system)
◊(define latex-source ◊string-append{
    \documentclass[a4paper,12pt]{article}
    \usepackage{ragged2e}
    \usepackage{amsmath,amsfonts}
    \usepackage{tikz-cd}
    \usepackage[english]{babel}
    \usepackage[autostyle]{csquotes}
    \usepackage{hyperref,booktabs}
    
    \usepackage{listings}
    \lstset{extendedchars}

    \usepackage{eurosym}
    \usepackage{fancyvrb}
    \usepackage{longtable,booktabs}
    \usepackage{attrib}
    \usepackage{graphicx}
    \usepackage{mathspec}
    \usepackage{xltxtra,xunicode}
    \usepackage{xspace}

    \usepackage[T1]{fontenc}

    %\def\faFileAlt{\symbol{"F0F6"}}
    %\newfontfamily{\FA}{FontAwesome}
    %\def\fileicon{{\FA\faFileAlt}}

    \defaultfontfeatures{Scale=MatchLowercase}

    \usepackage{microtype}
    \usepackage{fontspec}

    %% Typography defaults
    \newfontfamily\linenumberfont[Mapping=tex-text]{CMU Serif}

    % \setsansfont[
        % ItalicFont     = HelveticaNeue-Italic,
        % BoldFont       = HelveticaNeue-Bold,
        % BoldItalicFont = HelveticaNeue-BoldItalic]{HelveticaNeue}
    %\setmainfont[Mapping=tex-text,SmallCapsFeatures={LetterSpace=5.5}]{Bitstream Charter}
    \setmainfont{Charter}
    \setmonofont[%
        Scale = 0.8]{JuliaMono Nerd Font Propo}
    % \newfontfamily\NHLight[
       % ItalicFont     = HelveticaNeue-LightItalic,
       % BoldFont       = HelveticaNeue-UltraLight,
       % BoldItalicFont = HelveticaNeue-UltraLightItalic]{HelveticaNeue-Light}

    \usepackage{xcolor}
    \definecolor{mygray}{rgb}{0.7,0.7,0.7}
    \definecolor{light-gray}{gray}{0.95}
    \definecolor{tweet-cyan}{RGB}{154,228,232}
    \definecolor{almond}{rgb}{0.94, 0.87, 0.8}
    \definecolor{antiquewhite}{rgb}{0.98, 0.92, 0.84}

    \usepackage{textcomp}
    \usepackage{upquote}
    \usepackage{listingsutf8}
    \lstset{
        inputencoding=utf8,
        extendedchars=true,
        basicstyle=\scriptsize\ttfamily,
        columns=flexible,
        breaklines=true,
        numbers=left,
        upquote=true,
        backgroundcolor=\color{antiquewhite},
        numbersep=5pt,
        frame=single,
        framesep=\fboxsep,
        framerule=\fboxrule,
        rulecolor=\color{black},
        xleftmargin=\dimexpr\fboxsep+\fboxrule,
        xrightmargin=\dimexpr\fboxsep+\fboxrule,
        framexleftmargin=.25in,
        % belowcaptionskip=0pt,
        numberstyle=\scriptsize\color{mygray}\linenumberfont
    }

    % Add the lozenge to the list of extended characters in the `listings`
    % environment. This is a limited workaround for a problem where Unicod
    % characters appear out of order in code listings.
    % See https://tex.stackexchange.com/q/81674
    %
    \begingroup
        \catcode0=12 %
        \makeatletter
        \g@addto@macro\lst@DefEC{%
            \lst@CCECUse\lst@ProcessLetter
            ◊"◊"% *** add Unicode characters ***
            ^^00% end marker
        }%
    \endgroup

    \usepackage{epigraph}
    \newcommand{\Spec}{\textup{Spec}}
    \newcommand{\FF}{\mathbb{F}}
    \newcommand{\ZZ}{\mathbb{Z}}
    \begin{document}
    ◊(apply string-append (cdr doc))
    \end{document}})
◊(define working-directory
    (make-temporary-file "pollen-latex-work-~a" 'directory))
◊(define temp-ltx-path (build-path working-directory "temp.ltx"))
◊(display-to-file latex-source temp-ltx-path #:exists 'replace)
◊(define command (format "xelatex -output-directory ~a ~a"
  working-directory temp-ltx-path))
◊(unless (system command) (error "xelatex: rendering error"))
◊(let ([pdf (file->bytes (build-path working-directory "temp.pdf"))])
   (delete-directory/files working-directory)
   pdf)
