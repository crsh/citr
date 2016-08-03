#' Insert citation in Markdown format
#'
#' Look up entries in bibliography and insert citation in Markdown format if exactly one match is returned.
#'
#' @inheritParams query_bib
#' @param in_paren Logical. Determines if citation is in parentheses.
#'
#' @details The path to the Bib(La)TeX-file can be set in the global options and is set to
#'    \code{references.bib} when the package is loaded. Once the path is changed in the
#'    RStudio addin, the global option is updated.
#'
#' @return If the bibliography contains exactly one match the formated citation is returned, otherwise
#'    returns \code{NULL}. \code{md_cite} returns an in-text citation (\code{"@foo2016"}), \code{md_cite}
#'    returns an in-parenthesis citation (\code{"[@foo2016]"}).
#'
#' @seealso \code{\link{insert_citation}}
#'
#' @examples
#' \dontrun{
#'   md_cite("foo 2016", bib_file = "references.bib")
#' }
#'
#' @import assertthat
#' @export

md_cite <- function(
  x
  , in_paren = TRUE
  , bib_file = options("citr.bibliography_path")
  , cache = TRUE
) {
  assert_that(is.flag(in_paren))

  # Query BibTeX file
  selected_entries <- query_bib(x, bib_file = bib_file, cache = cache)
  if(length(selected_entries) == 0) return(NULL)
  selected_keys <- names(selected_entries)

  # Print queried references
  tmp <- lapply(selected_entries, function(y) {
    reference <- paste_references(y)
    cat("\t", reference, "\n")
  })
  cat("\n")

  # Return citation keys
  paste_citation_keys(selected_keys, in_paren)
}


paste_citation_keys <- function(keys, in_paren = FALSE) {
  if(!is.null(keys)) assert_that(is.character(keys)) else return(NULL)
  assert_that(is.flag(in_paren))

  if(in_paren) {
    keys <- paste(keys, collapse = "; @")
    paste0("[@", keys, "]")
  } else {
    n_keys <- length(keys)
    if(n_keys == 2) {
      keys <- paste(keys, collapse = " and @")
    } else if(n_keys > 2) {
      keys <- paste(
        paste(keys[-n_keys], collapse = ", @")
        , keys[n_keys]
        , sep = ", and @"
      )
    }

    paste0("@", keys)
  }
}