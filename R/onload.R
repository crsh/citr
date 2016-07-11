.onLoad <- function(libname, pkgname) {
  op <- options()
  op_citr <- list(
    citr.bibliography_path = "references.bib"
    , citr.bibliography_cache = NULL
  )
  toset <- !(names(op_citr) %in% names(op))
  if(any(toset)) options(op_citr[toset])

  invisible()
}

.onUnload <- function(libname, pkgname) {
  options <- options()[-grep("citr\\.", names(options()))]
}