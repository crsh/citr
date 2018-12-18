citr: RStudio Addin to Insert Markdown Citations
================

[![CRAN
status](http://www.r-pkg.org/badges/version/citr)](https://cran.r-project.org/package=citr)
[![Download
counter](http://cranlogs.r-pkg.org/badges/citr)](https://cran.r-project.org/package=citr)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build
status](https://api.travis-ci.org/crsh/citr.svg?branch=master)](https://travis-ci.org/crsh/citr)

`citr` provides functions and an [RStudio
addin](https://rstudio.github.io/rstudioaddins/) to search a BibTeX-file
to create and insert formatted Markdown citations into the current
document.

## Installation

You can either use the stable version of `citr` from CRAN,

``` r
install.packages("citr")
```

or the current development snapshot from this GitHub repository:

``` r
devtools::install_github("crsh/citr")
```

## How to use citr

### The RStudio addin

Once `citr` is installed and you have restarted your R session, the
addin appears in the menus. Alternatively, you can define a [keyboard
shortcut](https://rstudio.github.io/rstudioaddins/#keyboard-shorcuts) to
call the addin.

![](tools/images/addin_demo.gif)

The addin will automatically look up the bibliography files specified in
the YAML front matter. If the document does not contain a YAML front
matter the addin will attempt to locate a parent document and look up
the bibliography file specified therein. That is, the addin works its
automagic even if you edit R Markdown documents that are included as
[children](http://yihui.name/knitr/demo/child/) in another R Markdown
document. The expected names of a parent document default to
`c("index.Rmd", "master.Rmd")`, it thus works with
[`bookdown`](https://bookdown.org/) out of the box, but can be
customized (e.g., `options(citr.parent_documents = "my_parent.Rmd")`).

### Using citr without RStudio

The following call searches a bibliography file and creates formatted
Markdown citations for the results.

``` r
library("citr")
md_cite("foo 2016", bib_file = "references.bib")
```

`md_cite()` searches the author, year, title, and journal fields of your
references.

### Better Bib(La)TeX integration

If you are using Zotero `citr` can access your reference database
directly. For this to work, you need to install the [Better Bib(La)TeX
extension](https://github.com/retorquere/zotero-better-bibtex/wiki).
Once the extension is installed and your reference manager is running,
`citr` can access all your references and keep your bibliography file
updated by adding missing references.

![](tools/images/addin_zotero_demo.gif)

### Tidying bibliography files

When you are done writing, your bibliography file is likely to contain
some unneeded references, which you added while writing but removed
during revisions. `tidy_bib_file()` removes unneeded (or duplicate)
entries from your bibliography file.

``` r
tidy_bib_file(
  rmd_file = "report.Rmd"
  , messy_bibliography = "references.bib"
  , file = "tidy_references.bib"
)
```

## Known problems

`citr` relies on `RefManager::ReadBib()` and, thus, indirectly on
`bibtex::read.bib()` to load bibliographies. I have noticed that very
long Bib(La)TeX fields can cause these underlying functions to fail
(e.g., `Error in do_read_bib(file, encoding = .Encoding, srcfile): lex
fatal error: input buffer overflow, can't enlarge buffer because scanner
uses REJECT`). To avoid such problems, I strongly recommend to disable
the export of potentially long metadata fields, such as abstract and
notes. The Better Bib(La)TeX-plugin allows users to specify fields to
omit:

![](tools/images/bbt_omit_abstract.png)

Once, the above error has occurred, it may be necessary to restart the R
session. Otherwise reading any other file may fail with `Error: lex
fatal error: fatal flex scanner internal error--end of buffer missed`.

## Other RStudio addins

If you are interested in other handy addins take a look at this
[list](https://github.com/daattali/addinslist#readme). There you can
find other useful addins, such as
[rcrossref](https://github.com/ropensci/rcrossref) or
[wordcountaddin](https://github.com/benmarwick/wordcountaddin).

# Package dependencies

![](tools/images/dependency_plot-1.png)<!-- -->
