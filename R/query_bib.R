#' Query bibliography
#'
#' Search entries in a bibliography.
#'
#' @param x Character. Search terms used to filter bibliography (by author, year, title, and journal
#'    fields); Regex is supported.
#' @param bib_file Character. Path to BibTeX-file. See details.
#'
#' @details The path to the BibTeX-file can be set in the global options and is set to
#'    \code{references.bib} when the package is loaded. Once the path is changed in the
#'    RStudio addin, the global option is updated.
#'
#' @return Returns matching entries form bibliography in class \code{\link[RefManageR]{BibEntry}}.
#' @seealso \code{\link{md_cite}}, \code{\link{insert_citation}}

query_bib <- function(
  x
  , bib_file = options("bibliography_path")
) {
  bib <- RefManageR::ReadBib(file = bib_file)
  pasted_bib <- paste_references(bib) # Create searchable text strins for references
  entries <- bib[grepl(x, pasted_bib, ignore.case = TRUE)]
  entries
}


paste_references <- function(bib) {
  author_names <- sapply(bib, function(x) {
    author_names <- unlist(x$author$family)
    if(is.null(author_names)) {
      author_names <- unlist(x$author$given)
    }
    n_authors <- length(author_names)
    if(n_authors == 1) {
      author_names
    } else if(n_authors == 2) {
      paste(author_names, collapse = " & ")
    } else if(n_authors > 2) {
      paste(
        paste(author_names[-n_authors], collapse = ", ")
        , author_names[n_authors]
        , sep = ", & "
      )
    }
  })
  
  year <- sapply(bib, function(x) {
    if(!is.null(x$year)) x$year else {
      tryCatch(format(as.Date(unlist(x$date)), "%Y"), error = function(e) x$date)
    }
  })
  
  author_names <- gsub("\\}|\\{", "", author_names)
  titles <- gsub("\\}|\\{|\\\\", "", bib$title)
  journals <- gsub("\\}|\\{|\\\\", "", bib$journal)
  journals <- paste0(" ", journals, ".")
  journals[journals == " NULL."] <- ""
  
  paste0(author_names, " (", year, "). ", titles, ".", journals)
}
