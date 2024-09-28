\documentclass[a4paper,12pt]{article}
\begin{document}
◊(require racket/list)
◊(apply string-append (filter string? (flatten doc)))
\end{document}
