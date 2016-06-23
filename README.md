citr: RStudio Addin to Insert Markdown Citations
================

`citr` provides functions and an [RStudio addin](https://rstudio.github.io/rstudioaddins/) to search a BibTeX-file, create, and insert formatted Markdown citations into the current document.

Installation
------------

Until `citr` is on CRAN you can install it from this GitHub repository:

``` {r}
devtools::install_github("crsh/citr")
```

How to use citr
---------------

### The RStudio addin

Once `citr` is installed you should find the addin in the menus.

![](inst/images/addin_demo.gif)

You can also define a [keyboard shortcut](https://rstudio.github.io/rstudioaddins/#keyboard-shorcuts) to call the addin.

### Using citr without RStudio

The following call searches a BibTeX-file and creates formatted Markdown citations for the results.

``` {r}
library("citr")
md_cite("foo 2016", bib_file = "references.bib")
```

`md_cite()` searches author, year, title, and journal fields of your references.

Other RStudio addins
--------------------

If you are interested in other handy addins take a look at this [list](https://github.com/daattali/addinslist#readme). There you can find other useful addins, such as the [word count addin](https://github.com/benmarwick/wordcountaddin).
