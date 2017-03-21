#' Tidy bibliography file
#' 
#' Removes duplicate and unneeded entries from a Bib(La)Tex-file.
#'
#' @param rmd_file Character. One or more paths to the R Markdown files that use the messy bibliography file.
#' @param messy_bibliography Character. Path to the messy bibliography file.
#' @param file Character. Path and name for the to-be-created tidy bibliography. If \code{NULL} the messy bibliography is replaced.
#' @param encoding Character. The name of the encoding to be assumed. See the \code{\link{connection}}.
#'
#' @export
#'
#' @examples
#' NULL

tidy_bib_file <- function(
  rmd_file
  , messy_bibliography
  , file = NULL
  , encoding = getOption("encoding")
) {
  assert_that(is.character(rmd_file))
  assert_that(is.string(redundant_bibliography))
  if(!is.null(file)) {
    assert_that(is.string(file))
  } else {
    file <- redundant_bibliography
  }
  assert_that(is.string(encoding))
  assert_that(length(encoding) == 1)
  
  rmd <- c()
  for(i in seq_along(rmd_file)) {
    rmd <- paste(c(rmd, readLines(rmd_path, encoding = encoding)), collapse = " ")
  }
  
  if(nchar(rmd) == 0){
    stop("The R Markdown file contains no text.")
  }
  
  manuscript_test <- prep_text(rmd)
  
  reference_handles <- stringi::stri_extract_all(manuscript_test, regex = "@[^;\\s\\],]+")[[1]]
  reference_handles <- gsub("@", "", unique(reference_handles))
  
  if(length(reference_handles) == 0) stop("Found no references in ", rmd_path)
  
  complete_bibliography <- RefManageR::ReadBib(lookup_bibliography, .Encoding = encoding)
  
  necessary_bibliography <- complete_bibliography[names(complete_bibliography) %in% reference_handles]
  
  if(length(necessary_bibliography) == 0) stop("Found none of the ", length(reference_handles), " necessary references in the look-up bibliography.")
  
  if(length(necessary_bibliography) < length(reference_handles)) warning("Only found ", length(necessary_bibliography), " out of ", length(reference_handles), " necessary references in the look-up bibliography. The following references could not be found:\n  ", paste(reference_handles[!reference_handles %in% names(complete_bibliography)], collapse = ", "))
  
  message("Removing ", length(complete_bibliography) - length(unique(necessary_bibliography)), " unneeded bibliography entries.")
  
  RefManageR::WriteBib(unique(necessary_bibliography), file = file)
}


# Adapted from wordcountaddin (https://github.com/benmarwick/wordcountaddin/blob/master/R/hello.R)

prep_text <- function(text){
  
  # don't include front yaml
  text <- gsub("---.+?---", "", text)
  
  # don't include text in code chunks: https://regex101.com/#python
  text <- gsub("```\\{.+?\\}.+?```", "", text)
  
  # don't include text in in-line R code
  text <- gsub("`r.+?`", "", text)
  
  # don't include HTML comments
  text <- gsub("<!--.+?-->", "", text)
  
  # don't include inline markdown URLs
  text <- gsub("\\(http.+?\\)", "", text)
  
  # don't include images with captions
  text <- gsub("!\\[.+?\\)", "", text)
  
  #remove cross-references
  text <- gsub("\\\\@ref\\(.+?\\)", "", text)
  
  text
}
