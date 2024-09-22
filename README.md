# Busybee

> One divides into Two

Busybee is a Pollen-based publishing application with special support for Forester. Consistent with the Pollen philosophy of a _single standard markup language_, writing notes can be done in a standard markup which can be transpiled to `html`, `md`, `tree`, `pdf`, or any other format you are willing to implement. 

Busybee intends to unite the Pollen design principle of [the book is a program](https://docs.racket-lang.org/pollen/big-picture.html#%28part._the-book-is-a-program) with Jon Sterling's meditations on [evergreen, atomic notes for scientific thought](https://www.jonmsterling.com/tfmt-0001.xml). Our notes should be programmable snippets of source code written in a customizable markup that remains independent of their target application, be it word processing, note-taking, etc. By abstracting ourselves from our notes' dependencies on their authorship platform, we can better achieve "evergreen" notes without retreating from our criteria (atavism) or refusing to change tools (inertia).

## Why Pollen?

Pollen's objective is to offer a **single** **programmable** markup language, not to promote a specific language. This is a different approach to achieving *atomicity* and *evergreen* notes.

## Getting Started

### Busybee

You will need Pollen installed on your machine.

You will need a config.toml file.

### If you already use Forester

You can add your forest as a git submodule. This allows you to generate trees using Pollen while keeping the repositories git-wise independent.

```
git submodule add $your_forest_url forest
git -C forest checkout main
```

## FAQs

### Why not Markdown?

Now Markdown is a common source format for note-taking and is widely supported. Consider this [criticism of Markdown](https://docs.racket-lang.org/pollen/second-tutorial.html#:~:text=6.2%C2%A0Optional%20reading%3A%20the%20case%20against%20Markdown), written by the author of Pollen, Matt Butterick.

### Will I have abandon my lovingly-crafted Forester setup?

No! Busybee uses Pollen to transpile your notes into your `trees/busybee` directory, which means you can continue using Forester as normal. If you decide to abandon Busybee, your notes will just exist in that directory.

### Will I have to learn Racket?

To some degree, yes. While Busybee comes with many markup tags already implemented, extending it with new code targets or methods requires some comfort with Pollen's syntax. As some consolation, Racket is generally used as an educational programming language, and Pollen is used by many nonprofessional programmers as a principled means of writing composition. For programmers or scientists who are looking for multiple-dispatch publishing solutions, 

So while Racket is yet another language, it reduces the problem of adding features to programming them (compare with Vimscript or emacs LISP). But the content of our notes are also enriched S-expressions called X-expressions, and therefore easier to parse.

## Acknowledgements}

After experimenting with different architectures, I've settled on copying @otherjoel's project [thenotepad](https://github.com/otherjoel/thenotepad), which introduces a macro for producing tags in each code target. Rather than use `case` expressions for every tag, methods for each code target are separated in their own files. I am indebited to @theotherjoel and his code. 

