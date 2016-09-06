.onLoad <- function(libname, pkgname) { # nocov start
  op <- options()
  op_citr <- list(
    citr.bibliography_path = "./references.bib"
    , citr.bibliography_cache = NULL
    , citr.use_betterbiblatex = TRUE
    , citr.parent_documents = c("index.Rmd", "master.Rmd")
  )
  toset <- !(names(op_citr) %in% names(op))
  if(any(toset)) options(op_citr[toset])

  invisible()
} # nocov end
