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

    % Manually reimplement \newthought from Tufte-LaTeX
    \newskip\tufteskipamount
    \tufteskipamount=1.0\baselineskip plus 0.5ex minus 0.2ex

    \newcommand{\tuftebreak}{\par\ifdim\lastskip<\tufteskipamount
      \removelastskip\penalty-100\tufteskip\fi}

    \newcommand{\tufteskip}{\vspace\tufteskipamount}
    \newcommand{\newthought}[1]{%
       \tuftebreak
       \noindent\textsc{#1}%
    }

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

        
    % see http://tex.stackexchange.com/questions/11263/how-can-i-remove-listing-from-listings-caption
    % and http://tex.stackexchange.com/questions/209764/how-can-i-make-the-width-of-the-caption-match-that-of-the-listing
    \usepackage{calc}
    \usepackage[skip=0pt,position=auto]{caption}
    \DeclareCaptionFont{white}{\scriptsize\color{white}\ttfamily}
    \DeclareCaptionFormat{listing}{%
        \fcolorbox{black}{gray}{\parbox{\textwidth-2\fboxsep-2\fboxrule}{#1#2#3}}%
    }
    \captionsetup[lstlisting]{format=listing,labelfont=white,textfont=white}

    \makeatletter
    \def\maxwidth{\ifdim\Gin@nat@width>\linewidth\linewidth\else\Gin@nat@width\fi}
    \def\maxheight{\ifdim\Gin@nat@height>\textheight\textheight\else\Gin@nat@height\fi}
    \makeatother

    % Scale images if necessary, so that they will not overflow the page
    % margins by default, and it is still possible to overwrite the defaults
    % using explicit options in \includegraphics[width, height, ...]{}
    \setkeys{Gin}{width=\maxwidth,height=\maxheight,keepaspectratio} 

    \hypersetup{breaklinks=true,
                bookmarks=true,
                colorlinks=true,
                citecolor=blue,
                urlcolor=blue,
                linkcolor=magenta,
                pdfborder={0 0 0}}
    \urlstyle{same}  % don't use monospace font for urls

    % Make links footnotes instead of hotlinks:
    \renewcommand{\href}[2]{#2\footnote{\url{#1}}}


    % Make margin notes (from Tufte-LaTeX) into regular footnotes
    \newcommand{\marginnote}[1]{\footnote{#1}}
    \newcommand{\smallcaps}[1]{\textsc{#1}}

    \setlength{\parindent}{0pt}
    \setlength{\parskip}{6pt plus 2pt minus 1pt}
    \setlength{\emergencystretch}{3em}  % prevent overfull lines

    \setcounter{secnumdepth}{0}

    \VerbatimFootnotes % allows verbatim text in footnotes

    %% Titling package allows for macros \thetitle \theauthor, etc

    %% Reduced margins
    %\usepackage[margin=1.2in]{geometry}

    %% Paragraph and line spacing
    %\linespread{1.05} % a bit more vertical space
    %\setlength{\parskip}{\baselineskip} % space between paragraphs spacing is one baseline unit

    %% Sections headings spacing: one baseline unit before, none after
    \usepackage{titlesec}
    \titlespacing{\section}{0pt}{\baselineskip}{0pt}
    \titlespacing{\subsection}{0pt}{\baselineskip}{0pt}
    \titlespacing{\subsubsection}{0pt}{\baselineskip}{0pt}

    % Customize footnotes so that, within the footnote, the footnote number is
    % the same size as the footnote text (per Bringhurst).
    %
    \usepackage[splitrule,multiple,hang]{footmisc}
    \makeatletter
    \renewcommand\@makefntext[1]{\parindent 1em%
        \noindent
        \hb@xt@0em{\hss\normalfont\@thefnmark.} #1}
    \def\splitfootnoterule{\kern-3\p@ \hrule width 1in \kern2.6\p@}
    \makeatother
    \renewcommand\footnotesize{\fontsize{10}{12} \selectfont}
    \renewcommand{\thefootnote}{\arabic{footnote}}

    % Allow use of \st for strikethrough
    \usepackage{soul}

    \newcommand{\Spec}{\textup{Spec}}

    \newcommand{\Ban}{\textsf{Ban}}
    \newcommand{\Man}{\textsf{Man}}
    \newcommand{\Meas}{\textsf{Meas}}
    \newcommand{\Para}{\textsf{Para}}

    \newcommand{\el}{\textsf{el}}

    \newcommand{\cM}{M}
    \newcommand{\cP}{P}

    \newcommand{\FF}{\mathbb{F}}
    \newcommand{\RR}{\mathbb{R}}
    \newcommand{\ZZ}{\mathbb{Z}}

    \begin{document}
    \RaggedRight

    %\begingroup
    %    \centering
    %    {\LARGE\bf \thetitle}\\[1em]
    %    \par
    %\endgroup

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
