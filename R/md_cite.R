#' Insert citation in Markdown format
#'
#' Look up entries in bibliography and insert citation in Markdown format if exactly one match is returned.
#'
#' @inheritParams query_bib
#' @param in_paren Logical. Determines if citation is in parentheses.
#'
#' @details The path to the BibTeX-file can be set in the global options and is set to
#'    \code{references.bib} when the package is loaded. Once the path is changed in the
#'    RStudio addin, the global option is updated. If \code{use_betterbiblatex = TRUE} references
#'    are imported from Zotero/Juris-M rather than from the Bib(La)TeX-file. The Bib(La)TeX-file
#'    is then updated to include the inserted reference.
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
  , bib_file = getOption("citr.bibliography_path")
  , cache = TRUE
  , use_betterbiblatex = getOption("citr.use_betterbiblatex")
) {
  assert_that(is.flag(in_paren))

  # Query BibTeX file
  selected_entries <- query_bib(
    x
    , bib_file = bib_file
    , cache = cache
    , use_betterbiblatex = use_betterbiblatex
  )
  if(length(selected_entries) == 0) return(NULL)

  # Print queried references
  pasted_references <- paste_references(selected_entries)

  lapply(pasted_references, function(y) cat("\t", y, "\n"))
  cat("\n")

  # Add references to bib_file
  if(use_betterbiblatex && betterbiblatex_available()) {
    append_bib_entries(selected_entries, bib_file)
  }

  # Return citation keys
  if(length(selected_entries) > 1) selected_entries <- selected_entries[order(pasted_references)]
  paste_citation_keys(names(selected_entries), in_paren)
}


paste_citation_keys <- function(keys, in_paren = FALSE) {
  if(!is.null(keys)) assert_that(is.character(keys)) else return(NULL)
  assert_that(is.flag(in_paren))
  n_keys <- length(keys)

  if(in_paren) {
    keys <- paste(keys, collapse = "; @")
    paste0("[@", keys, "]")
  } else {
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

append_bib_entries <- function(x, bib_file) {
  if(file.exists(bib_file)) {
    existing_bib <- RefManageR::ReadBib(bib_file, check = FALSE)
    new_references <- !names(x) %in% names(existing_bib)
    if(sum(new_references) > 0) RefManageR::WriteBib(x[new_references], file = bib_file, append = TRUE)
  } else {
    RefManageR::WriteBib(x, file = bib_file)
  }
}