#' Query bibliography
#'
#' Search entries in a bibliography.
#'
#' @param x Character. Search terms used to filter bibliography (by author, year, title, and journal
#'    fields); Regex is supported.
#' @param bib_file Character. Path to Bib(La)TeX-file. See details.
#' @param cache Logical. If \code{cache = TRUE} cached bibliography is used, if available. If
#'    \code{cache = FALSE} bibliography is re-imported on every function call.
#' @param use_betterbiblatex Logical. If \code{use_betterbiblatex = TRUE} references are imported from Zotero/Juris-M.
#'    Requires that the \href{https://github.com/retorquere/zotero-better-bibtex}{Better Bib(La)TeX} is installed and
#'    Zotero/Juris-M is running.
#'
#' @details The path to the BibTeX-file can be set in the global options and is set to
#'    \code{references.bib} when the package is loaded. Once the path is changed in the
#'    RStudio addin, the global option is updated. If \code{use_betterbiblatex = TRUE} references
#'    are imported from Zotero/Juris-M rather than from the Bib(La)TeX-file. The Bib(La)TeX-file
#'    is then updated to include the inserted reference.
#'
#' @return Returns list of class \code{\link[RefManageR]{BibEntry}} including all matching bibliography entries.
#' @seealso \code{\link{md_cite}}, \code{\link{insert_citation}}
#'
#' @import assertthat

query_bib <- function(
  x
  , bib_file = getOption("citr.bibliography_path")
  , cache = TRUE
  , use_betterbiblatex = getOption("citr.use_betterbiblatex")
) {
  assert_that(is.string(x))
  assert_that(is.string(bib_file))
  assert_that(is.flag(cache))
  assert_that(is.flag(use_betterbiblatex))
  if(use_betterbiblatex && !betterbiblatex_available()) {
    message("Could not connect to Zotero's Better-BibTeX extension; importing references from", bib_file, ". Is Zotero up and running?")
  }

  # Use cached bibliography, if available
  if(is.null(getOption("citr.bibliography_cache")) || !cache) {

    if(use_betterbiblatex & betterbiblatex_available()) {
      bib <- load_betterbiblatex_bib()
    } else {
      bib <- RefManageR::ReadBib(file = bib_file, check = FALSE)
    }

    if(cache) options(citr.bibliography_cache = bib)
  } else {
    bib <- getOption("citr.bibliography_cache")
  }

  pasted_bib <- paste_references(bib) # Create searchable text strings for references
  entries <- bib[grepl(gsub("\\s", ".+", x), pasted_bib, ignore.case = TRUE)]
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
        date <- unlist(x$date)
        regmatches(date, regexpr("[0-9]{4}", date))
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
  journals <- paste0(" ", journals, ifelse(journals != "", ".", NULL))
  journals[journals == " NULL."] <- ""

  paste0(author_names, " (", year, ").", titles, journals)
}


betterbiblatex_available <- function() {
  tryCatch(
    rawToChar(curl::curl_fetch_memory(url = "http://localhost:23119/better-bibtex/cayw?probe=probe")$content) == "ready"
    , error = function(e) FALSE
  )
}

load_betterbiblatex_bib <- function() {
  betterbibtex_url <- "http://localhost:23119/better-bibtex/library?library.biblatex"
  betterbibtex_bib <- rawToChar(curl::curl_fetch_memory(url = betterbibtex_url)$content)
  betterbibtex_bib <- strsplit(betterbibtex_bib, "@comment\\{jabref-meta")[[1]][1] # Remove jab-ref comments
  betterbibtex_entries <- strsplit(gsub("(@\\w+\\{)", "~\\1", betterbibtex_bib), "~" )[[1]]

  # Create and read multiple biblatex files because bibtex::read.bib does not work with large files
  bib <- c()
  no_batches <- length(betterbibtex_entries) %/% 100 + 1
  for(i in seq_len(no_batches)) {
    tmp_bib_file <- paste0(paste(sample(c(letters, LETTERS, 0:9), size = 32, replace = TRUE), collapse = ""), ".bib")
    writeLines(betterbibtex_entries[((i-1) * 100 + 1):min(i * 100, length(betterbibtex_entries))], con = tmp_bib_file)
    bib <- c(bib, RefManageR::ReadBib(file = tmp_bib_file, check = FALSE))
    file.remove(tmp_bib_file)
  }

  class(bib) <- c("BibEntry", "bibentry")
  bib
}
