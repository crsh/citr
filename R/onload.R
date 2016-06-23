.onLoad <- function(libname, pkgname) {
  op <- options()
  op_citr <- list(bibliography_path = "references.bib")
  toset <- !(names(op_citr) %in% names(op))
  if(any(toset)) options(op_citr[toset])

  invisible()
}