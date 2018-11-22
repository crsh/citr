.onLoad <- function(libname, pkgname) { # nocov start
  op <- options()
  op_citr <- list(
    citr.bibliography_path = "./references.bib"
    , citr.bibliography_cache = NULL
    , citr.citation_key_cache = NULL
    , citr.use_betterbiblatex = FALSE
    , citr.exclude_betterbiblatex_library = NULL
    , citr.betterbiblatex_format = "bibtex"
    , citr.bib_file_last_modified = Sys.time()
    , citr.bib_file_cache = NULL
    , citr.update_bib = "./references.bib"
    , citr.parent_documents = c("index.Rmd", "master.Rmd")
    , citr.encoding = "UTF-8"
  )
  toset <- !(names(op_citr) %in% names(op))
  if(any(toset)) options(op_citr[toset])

  invisible()
} # nocov end
