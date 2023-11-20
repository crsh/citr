get_rstudio_context <- function() { # Also use in addin
  if(rstudioapi::isAvailable("0.99.1111")) {
    context <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  }
  if((exists("context") && is.null(context)) || rstudioapi::isAvailable("0.99.796")) {
    context <- rstudioapi::getActiveDocumentContext()
  } else stop(
    "The use of this addin requires RStudio 0.99.796 or newer (your version is "
    , rstudioapi::versionInfo()$version
    , ")."
  )

  context
}


bbt_cayw_call <- function(endpoint) {
  httr::content(
    httr::GET(paste0("http://127.0.0.1:23119/better-bibtex/cayw?", endpoint))
    , as = "text"
    , encoding = "UTF-8"
  )
}

bbt_cayw_available <- function() {  # betterbiblatex_available()
  res <- bbt_cayw_call(endpoint = "probe=true")

  res == "ready"
}

bbt_cayw_query <- function() {
  if(!isTRUE(bbt_cayw_available())) {
    warning("")
  } else {
    bbt_cayw_call(endpoint = "format=pandoc")
  }
}


bbt_cayw_insert <- function() {

  context <- get_rstudio_context()

  citation_keys <- bbt_cayw_query()

  if(citation_keys != "") {
    rstudioapi::insertText(text = citation_keys, id = context$id)
  }
}
