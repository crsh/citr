citr: RStudio Addin to Insert Markdown Citations
================

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Build status](https://travis-ci.org/crsh/citr.svg?branch=master)](https://travis-ci.org/crsh/citr)

`citr` provides functions and an [RStudio addin](https://rstudio.github.io/rstudioaddins/) to search a BibTeX-file to create and insert formatted Markdown citations into the current document.

Installation
------------

Until `citr` is on CRAN you can install it from this GitHub repository:

``` {r}
devtools::install_github("crsh/citr")
```

How to use citr
---------------

### The RStudio addin

Once `citr` is installed and you have restarted your R session, the addin appears in the menus. Alternatively, you can define a [keyboard shortcut](https://rstudio.github.io/rstudioaddins/#keyboard-shorcuts) to call the addin.

![](inst/images/addin_demo.gif)

### Using citr without RStudio

The following call searches a BibTeX-file and creates formatted Markdown citations for the results.

``` {r}
library("citr")
md_cite("foo 2016", bib_file = "references.bib")
```

`md_cite()` searches the author, year, title, and journal fields of your references.

Known problems
--------------

`citr` relies on `RefManager::ReadBib()` and, thus, indirectly `bibtex::read.bib()` to load bibliographies.

-   I have noticed that very long can cause the underlying functions to fail (e.g., `Error: lex fatal error: fatal flex scanner internal error--end of buffer missed`). A restart of the R session may be neccessary to resolve the issue.

-   Currently, a [bug](https://github.com/mwmclean/RefManageR/issues/16) in `tools::latexToUtf8()` can cause `RefManager::ReadBib()` to hang and never finish. This problem has been worked around in the development version of `RefManager` (&gt; 0.10.13). If you experience problems like this, try installing `RefManager` [from GitHub](https://github.com/mwmclean/RefManageR).

Other RStudio addins
--------------------

If you are interested in other handy addins take a look at this [list](https://github.com/daattali/addinslist#readme). There you can find other useful addins, such as the [word count addin](https://github.com/benmarwick/wordcountaddin).
