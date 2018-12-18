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
#' @param betterbiblatex_format Character. Bibliography format to export from Zotero/Juris-M. Can be either \code{"bibtex"} or \code{"biblatex"}. Ignored if \code{use_betterbiblatex = FALSE}.
#'    Requires that the \href{https://github.com/retorquere/zotero-better-bibtex}{Better Bib(La)TeX} is installed and
#'    Zotero/Juris-M is running.
#' @param exclude_betterbiblatex_library Character. A vector of Zotero/Juris-M library names not to query.
#' @param encoding Character. Encoding of the Bib(La)TeX-file.
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
  , betterbiblatex_format = getOption("citr.betterbiblatex_format")
  , exclude_betterbiblatex_library = getOption("citr.exclude_betterbiblatex_library")
  , encoding = getOption("citr.encoding")
) {
  assert_that(is.string(x))
  assert_that(is.string(bib_file))
  assert_that(is.flag(cache))
  assert_that(is.flag(use_betterbiblatex))
  if(use_betterbiblatex && !betterbiblatex_available()) {
    message(
      "Could not connect to Zotero's Better-BibTeX extension; importing references from"
      , bib_file
      , ". Is Zotero up and running?"
    )
  }
  assert_that(is.string(betterbiblatex_format))
  if(!betterbiblatex_format %in% c("bibtex", "biblatex")) {
    stop("Bibliography format not supported. Use either 'bibtex'  or 'biblatex'.")
  }
  assert_that(is.string(encoding))

  # Use cached bibliography, if available
  if(is.null(getOption("citr.bibliography_cache")) || !cache) {

    if(use_betterbiblatex & betterbiblatex_available()) {
      cat("Connecting to Zotero...\n")
      bib <- load_betterbiblatex_bib(
        encoding
        , betterbiblatex_format
        , exclude_betterbiblatex_library
      )
    } else {
      bib <- RefManageR::ReadBib(
        file = bib_file
        , check = FALSE
        , .Encoding = encoding
      )
    }

    if(cache) options(citr.bibliography_cache = bib)
  } else {
    bib <- getOption("citr.bibliography_cache")
  }

  pasted_bib <- paste_references(bib) # Create searchable text strings for references
  entries <- bib[grepl(gsub("\\s", ".+", x, useBytes = TRUE), pasted_bib, ignore.case = TRUE)]
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

  author_names <- gsub("\\}|\\{", "", author_names, useBytes = TRUE)
  titles <- gsub("\\}|\\{|\\\\", "", bib$title, useBytes = TRUE)
  titles <- gsub("\\n", " ", titles, useBytes = TRUE)
  titles <- gsub("\\.+$", "", titles, useBytes = TRUE)

  # if author_names is null replace by title
  no_authors <- author_names == ""
  author_names[no_authors] <- titles[no_authors]
  titles <- paste0(" ", titles, ".")
  titles[no_authors] <- ""

  journals <- gsub("\\}|\\{|\\\\", "", bib$journal, useBytes = TRUE)
  journals <- paste0(" ", journals, ifelse(journals != "", ".", NULL))
  journals[journals == " NULL."] <- ""

  paste0(author_names, " (", year, ").", titles, journals)
}


betterbiblatex_available <- function() {
  tryCatch(
    rawToChar(curl::curl_fetch_memory(
      url = "http://localhost:23119/better-bibtex/cayw?probe=probe")$content
    ) == "ready"
    , error = function(e) FALSE
  )
}

#' Load bibliography from Zotero
#'
#' @param increment_progress logical switch which will use the shiny function \code{incProgress} when \code{TRUE}.
#' \code{FALSE} by default.
#' @inheritParams query_bib
#' @inheritParams tidy_bib_file
#' @inheritParams query_bib
#'
#' This function loads into RAM the bibliography stored in Zotero.
#' May take a several seconds if you have many hundreds of references.
#'
#' @export
#' @examples \dontrun{
#' b <- load_betterbiblatex_bib(encoding = "UTF-8")
#' }
load_betterbiblatex_bib <- function(
  encoding
  , betterbiblatex_format = "bibtex"
  , exclude_betterbiblatex_library = NULL
  , increment_progress = FALSE
) {

  bib <- c()
  bbt_libraries <- query_bbt_libraries()

  if(is.null(bbt_libraries)) {
    betterbibtex_url <- paste0(
      "http://localhost:23119/better-bibtex/library?library."
      , betterbiblatex_format
    )

    bib <- import_bbt(bib, betterbibtex_url, encoding)
    if(increment_progress) shiny::incProgress(1)

  } else {
    betterbibtex_baseurl <- "http://localhost:23119/better-bibtex/library?/"
    bbt_libraries <- bbt_libraries[
      !unlist(bbt_libraries[, "name"] %in% exclude_betterbiblatex_library)
      ,
      , drop = FALSE
    ]
    group_library_id <- unlist(bbt_libraries[, "id"])

    for(bib_i in group_library_id) {
      betterbibtex_url_i <- paste0(
        betterbibtex_baseurl
        , bib_i
        , "/library."
        , betterbiblatex_format
      )

      group_library_name_i <- unlist(bbt_libraries[unlist(bbt_libraries[, "id"]) == bib_i, "name"])
      if(increment_progress) {
        setProgress(detail = paste0("\n'", group_library_name_i[[1]], "'"))
      } else {
        cat("Importing '", group_library_name_i, "'...\n", sep = "")
      }

      bib <- import_bbt(bib, betterbibtex_url_i, encoding)
      if(increment_progress) shiny::incProgress(1 / length(group_library_id))
    }
  }

  class(bib) <- c("BibEntry", "bibentry")
  bib
}


import_bbt <- function(x, location, encoding) {
  betterbibtex_bib <- rawToChar(curl::curl_fetch_memory(url = location)$content)

  # Remove jab-ref comments
  betterbibtex_bib <- strsplit(betterbibtex_bib, "@comment\\{jabref-meta", useBytes = TRUE)[[1]][1]

  # Download bibliography
  tmp_bib_file <- paste0(
    paste(
      sample(c(letters, LETTERS, 0:9), size = 32, replace = TRUE)
      , collapse = ""
    )
    , ".bib"
  )
  tmp_con <- file(tmp_bib_file, encoding = encoding)
  writeLines(betterbibtex_bib, con = tmp_con)
  close(tmp_con)
  try(on.exit(suppressWarnings(file.remove(tmp_bib_file))), silent = TRUE)

  # If ever bibtex supports reading from connections
  # tmp_con <- textConnection(betterbibtex_bib, encoding = encoding)
  # try(on.exit(suppressWarnings(close(tmp_con))), silent = TRUE)

  c(x, RefManageR::ReadBib(file = tmp_bib_file, check = FALSE, .Encoding = encoding))
}


query_bbt_libraries <- function(x) {
  # If installed BBT is >= 5.0.50 use JSON-RPC to qurey library IDs
  bbt_json_response <- httr::POST(
    "http://localhost:23119/better-bibtex/json-rpc"
    , httr::add_headers("Content-Type" = "application/json")
    , body = '{"jsonrpc": "2.0", "method": "user.groups" }'
    , encode = "json"
  )

  if(any(grepl("No endpoint found", httr::content(bbt_json_response)))) {
    NULL
  } else {
    do.call(rbind, httr::content(bbt_json_response)$result)
  }
}

