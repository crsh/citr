#' Query bibliography
#'
#' Search entries in a bibliography.
#'
#' @param x Character. Search terms used to filter bibliography (by author, year, title, and journal
#'    fields); Regex is supported.
#' @param bib_file Character. Path to Bib(La)TeX-file. See details.
#' @param cache Logical. Cached bibliography is used, if available. If \code{cache = FALSE} bibliography
#'    is re-imported on every function call.
#'
#' @details The path to the BibTeX-file can be set in the global options and is set to
#'    \code{references.bib} when the package is loaded. Once the path is changed in the
#'    RStudio addin, the global option is updated.
#'
#' @return Returns list of class \code{\link[RefManageR]{BibEntry}} including all matching bibliography entries.
#' @seealso \code{\link{md_cite}}, \code{\link{insert_citation}}
#'
#' @import assertthat

query_bib <- function(
  x
  , bib_file = options("citr.bibliography_path")
  , cache = TRUE
) {
  assert_that(is.string(x))
  bib_file <- unlist(bib_file)
  assert_that(is.string(bib_file))
  assert_that(is.flag(cache))

  # Use cached bibliography, if available
  if(is.null(options("citr.bibliography_cache")[[1]]) || !cache) {
    bib <- RefManageR::ReadBib(file = bib_file)
    options(citr.bibliography_cache = bib)
  } else {
    bib <- options("citr.bibliography_cache")[[1]]
  }

  pasted_bib <- paste_references(bib) # Create searchable text strins for references
  entries <- bib[grepl(x, pasted_bib, ignore.case = TRUE)]
  if(length(entries) > 0) entries else NULL
}


paste_references <- function(bib) {
  assert_that(methods::is(bib, "bibentry"))

  author_names <- sapply(bib, function(x) {
    author_names <- x$author$family
    if(is.null(author_names)) {
      author_names <- unlist(x$author$given)
    }

    author_names <- if(is.list(author_names)) {
      unlist(lapply(author_names, paste, collapse = " "))
    } else if(is.null(author_names)) {
      ""
    } else {
      paste(author_names, collapse = " ")
    }

    n_authors <- length(author_names)
    if(n_authors == 1) {
      author_names
    } else if(n_authors == 2) {
      paste(author_names, collapse = " & ")
    } else if(n_authors > 2 & n_authors < 6) {
      paste(
        paste(author_names[-n_authors], collapse = ", ")
        , author_names[n_authors]
        , sep = ", & "
      )
    } else if(n_authors >= 6) {
      paste(author_names[1], "et al.")
    }
  })

  year <- sapply(bib, function(x) {
    if(!is.null(x$year)) x$year else {
      if(!is.null(x$date)) {
        tryCatch(format(as.Date(unlist(x$date)), "%Y"), error = function(e) x$date)
      } else {
        "n.d."
      }
    }
  })

  author_names <- gsub("\\}|\\{", "", author_names)
  titles <- gsub("\\}|\\{|\\\\", "", bib$title)
  titles <- gsub("\\n", " ", titles)

  # if author_names is null replace by title
  no_authors <- author_names == ""
  author_names[no_authors] <- titles[no_authors]
  titles <- paste0(" ", titles, ".")
  titles[no_authors] <- ""

  journals <- gsub("\\}|\\{|\\\\", "", bib$journal)
  journals <- paste0(" ", journals, ".")
  journals[journals == " NULL."] <- ""

  paste0(author_names, " (", year, ").", titles, journals)
}
