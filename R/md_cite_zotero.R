#' Insert citation in Markdown format from Zotero
#'
#' Look up entries in Zotero database and insert citation in Markdown format.
#'
#' @param in_paren Logical. Determines if citation is in parentheses.
#' @param bib_file Character. Path to Bib(La)TeX-file. See details.
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
#'   md_cite_zotero(bib_file = "references.bib")
#' }
#'
#' @import assertthat

md_cite_zotero <- function(
  in_paren = TRUE
  , bib_file = options("citr.bibliography_path")
) {
  if(!betterbiblatex_available()) stop("Could not connect to Zotero's Better-BibTeX extension. Is Zotero up and running?")
  assert_that(is.flag(in_paren))

  query_string <- paste0(
    "http://localhost:23119/better-bibtex/cayw?format=pandoc&clipboard=yes"
    , if(in_paren) "&brackets=yes"
  )
  rawToChar(curl::curl_fetch_memory(url = query_string)$content)
}
