◊(require racket/file racket/system)

◊(require (file "~/Documents/personal/repos/busybee/lib/ltx-helper.rkt"))

◊(define bf-commands (generate-all-math-letters "mathbf"
	 #:func (lambda (x) (string x x))))
◊(define bb-commands (generate-all-math-letters "mathbb"
	 #:func (lambda (x) (string x (char-downcase x)))))
◊(define sf-commands (generate-all-math-letters "mathsf"
	 #:func (lambda (x) (string (char-downcase x) x))))
◊(define fk-commands (generate-all-math-letters "mathfrak"
	 #:func (lambda (x) (string-append "fk" (string x)))))
◊(define fk-lower-commands (generate-all-math-letters "mathfrak"
	 #:func (lambda (x) (string-append "fk" (string-downcase (string x))))
	 #:transform char-downcase))
◊(define cal-commands (generate-all-math-letters "mathcal"
    #:func (lambda (x) (string-append "c" (string x) (string x)))))

◊(define (print-if thing fmt)
   (if thing (format fmt thing) ""))

◊(define latex-source ◊string-append{
    \documentclass[a4paper,twoside,12pt]{article}
    \usepackage{ragged2e}
    \usepackage{marginnote}
    \usepackage{amsmath,amsfonts,amsthm,amssymb}
    \usepackage{tikz-cd}
    \usepackage[english]{babel}
    \usepackage[autostyle]{csquotes}
    \usepackage{hyperref,booktabs}
    \usepackage{adjustbox}

    \newtheorem{theorem}{Theorem}

    \usepackage{listings}
    \lstset{extendedchars}

    \usepackage{enumitem}
    \usepackage{eurosym}
    \usepackage{fancyhdr}
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

    \usepackage[final,nopatch=footnote]{microtype}
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

    % this is for epigraphs
    \usepackage{epigraph}
    \setlength\epigraphwidth{8cm}
    \setlength\epigraphrule{0pt}
    \usepackage{etoolbox}
    \makeatletter
    \patchcmd{\epigraph}{\@epitext{#1}}{\itshape\@epitext{#1}}{}{}
    \makeatother

    \usepackage{color}
    \definecolor{marron}{RGB}{60,30,10}
    \definecolor{darkblue}{RGB}{0,0,80}
    \definecolor{lightblue}{RGB}{80,80,80}
    \definecolor{darkgreen}{RGB}{0,80,0}
    \definecolor{darkgray}{RGB}{0,80,0}
    \definecolor{darkred}{RGB}{80,0,0}
    \definecolor{shadecolor}{rgb}{0.97,0.97,0.97}
    \usepackage{fourier-orns}

    \fancyhf{}

    \newcommand{\ornamento}{\vspace{2em}\noindent \textcolor{darkgray}{\hrulefill~ \raisebox{-2.5pt}[10pt][10pt]{\leafright \decofourleft \decothreeleft  \aldineright \decotwo \floweroneleft \decoone   \floweroneright \decotwo \aldineleft\decothreeright \decofourright \leafleft} ~  \hrulefill \\ \vspace{2em}}}
    \newcommand{\ornpar}{\noindent \textcolor{darkgray}{ \raisebox{-1.9pt}[10pt][10pt]{\leafright} \hrulefill \raisebox{-1.9pt}[10pt][10pt]{\leafright \decofourleft \decothreeleft  \aldineright \decotwo \floweroneleft \decoone}}}
\newcommand{\ornimpar}{\textcolor{darkgray}{\raisebox{-1.9pt}[10pt][10pt]{\decoone \floweroneright \decotwo \aldineleft \decothreeright \decofourright \leafleft} \hrulefill \raisebox{-1.9pt}[10pt][10pt]{\leafleft}}}

    \fancyfoot[LO]{\ornimpar \\ \large \hfill \sffamily\bf \textcolor{darkgray}{\leafNE ~~~ \thepage}}
    \fancyfoot[RE]{\ornpar   \\ \large  \sffamily\bf \textcolor{darkgray}{\thepage ~~~ \reflectbox{\leafNE}}\hfill}


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
    \urlstyle{same}  % dont use monospace font for urls

    % Make links footnotes instead of hotlinks:
    \renewcommand{\href}[2]{{#2}\footnote{\url{#1}}}


    % Make margin notes (from Tufte-LaTeX) into regular footnotes
    %\newcommand{\marginnote}[1]{\footnote{#1}}
    %\newcommand{\smallcaps}[1]{\textsc{#1}}

    \setlength{\parindent}{0pt}
    \setlength{\parskip}{6pt plus 2pt minus 1pt}
    \setlength{\emergencystretch}{3em}  % prevent overfull lines

    \setcounter{secnumdepth}{0}

    \VerbatimFootnotes % allows verbatim text in footnotes

    %% Titling package allows for macros \thetitle \theauthor, etc
    \usepackage{titling}
    %\title{(select-from-metas 'title metas)}
    %◊(print-if (select-from-metas 'author metas) "\\author{~a}")

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

    \newcommand{\tens}{\otimes}

    \newcommand{\Spec}{\textup{Spec}}

    \newcommand{\Set}{\textsf{Set}}
    \newcommand{\FinSet}{\textsf{FinSet}}
    \newcommand{\FinStoch}{\textsf{FinStoch}}
    \newcommand{\Top}{\textsf{Top}}
    \newcommand{\Graph}{\textsf{Graph}}
    \newcommand{\Ban}{\textsf{Ban}}
    \newcommand{\BanMan}{\textsf{BanMan}}
    \newcommand{\Man}{\textsf{Man}}
    \newcommand{\CompMan}{\textsf{CompMan}}
    \newcommand{\Meas}{\textsf{Meas}}
    \newcommand{\Para}{\textsf{Para}}
    \newcommand{\Sch}{\textsf{Sch}}
    \newcommand{\Dynam}{\textsf{Dynam}}

    \newcommand{\Diff}{\textsf{Diff}}
    \newcommand{\PSh}{\textsf{PSh}}
    \newcommand{\Sh}{\textsf{Sh}}
    \newcommand{\Tw}{\textsf{Tw}}

    \newcommand{\Conf}{\textsf{Conf}}

    \newcommand{\Kl}{\textsf{Kl}}
    \newcommand{\el}{\textsf{el}}

    \newcommand{\cM}{M}
    \newcommand{\cP}{P}

    ◊|bf-commands|
    ◊|bb-commands|
    ◊|sf-commands|
    ◊|fk-commands|
    ◊|fk-lower-commands|
    ◊|cal-commands|

    \usetikzlibrary{calc,intersections,through,backgrounds}
    \usepackage{tkz-euclide}

    \begin{document}
    \RaggedRight

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
