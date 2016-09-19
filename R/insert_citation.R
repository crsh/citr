#' Invoke RStudio add-in to insert Markdown citations
#'
#' @inheritParams query_bib
#'
#' @details The path to the Bib(La)TeX-file can be set in the global options and is set to
#'    \code{./references.bib} when the package is loaded. Once the path is changed in the
#'    RStudio addin, the global option is updated. If \code{use_betterbiblatex = TRUE} references
#'    are imported from Zotero/Juris-M rather than from the Bib(La)TeX-file. The Bib(La)TeX-file
#'    is then updated to include the inserted reference.
#'
#'    If \code{insert_citation} is called while the focus is on a R Markdown document,
#'    which includes a YAML front matter with paths to one or more bibliography files,
#'    \code{bib_file} is ignored. Instead the file(s) from the YAML front matter are used.
#'
#'    The addin caches bibliographies to avoid unnecessary hard drive access. If
#'    the specified bibliography path or the file paths in the YAML header change the files
#'    are reloaded. To manually reload a bibliography at an unchanged location click the
#'    action link.
#'
#' @return Inserts selected Markdown citation(s) at currenct location.
#'
#' @examples
#' \dontrun{
#'  insert_citation()
#' }
#'
#' @import miniUI
#' @import shiny
#' @import assertthat
#' @export

insert_citation <- function(bib_file = getOption("citr.bibliography_path"), use_betterbiblatex = getOption("citr.use_betterbiblatex")) {
  assert_that(is.character(bib_file))
  assert_that(is.flag(use_betterbiblatex))

  if(rstudioapi::isAvailable("0.99.1111")) {
    context <- rstudioapi::getSourceEditorContext()
  } else if(rstudioapi::isAvailable("0.99.796")) {
    context <- rstudioapi::getActiveDocumentContext()
  } else stop("The use of this addin requires RStudio 0.99.796 or newer (your version is ", rstudioapi::versionInfo()$version, ").")

  betterbiblatex <- betterbiblatex_available()

  # Get bibliography files from YAML front matter if available
  ## Let's hope this doesn't cause too much trouble; this is a lot more sofisticated in rmarkdown, but the functions are not exported.
  yaml_bib_file <- NULL
  yaml_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", context$contents)

  ## Always look up index.Rmd if document is in bookdown directory
  candidate_parents <- getOption("citr.parent_documents")
  if(any(file.exists(paste(dirname(context$path), c("_bookdown.yml", "_site.yml"), sep = "/")))) {
    if(!"index.Rmd" %in% candidate_parents) candidate_parents <- c(candidate_parents, "index.Rmd")
  }

  parents_path <- paste(dirname(context$path), candidate_parents, sep = "/")
  parents <- file.exists(parents_path)

  ## Search current document for specified bib files
  if(length(yaml_delimiters) >= 2 &&
     (yaml_delimiters[2] - yaml_delimiters[1] > 1) &&
     grepl("^---\\s*$", context$contents[yaml_delimiters[1]])) {
    if(context$path == "") {
      message("\nUnsaved R Markdown document: Cannot locate Bib(La)TeX file(s); falling back to manual path specification.\n")
    } else {
      yaml_bib_file <- yaml::yaml.load(paste(context$contents[(yaml_delimiters[1] + 1):(yaml_delimiters[2] - 1)], collapse = "\n"))$bibliography
    }

  ## Search parent documents
  }
  if(any(parents)) {
    if(sum(parents) > 1) stop("More than one parent document found. See getOption('citr.parent_documents').")

    parent_document <- readLines(parents_path[parents])
    parent_yaml_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", parent_document)

    if(length(parent_yaml_delimiters) >= 2 &&
       (parent_yaml_delimiters[2] - parent_yaml_delimiters[1] > 1) &&
       grepl("^---\\s*$", parent_document[parent_yaml_delimiters[1]])) {
        yaml_bib_file <- c(yaml_bib_file, yaml::yaml.load(paste(parent_document[(parent_yaml_delimiters[1] + 1):(parent_yaml_delimiters[2] - 1)], collapse = "\n"))$bibliography)
    }
  }

  if(!is.null(yaml_bib_file)) {
    relative_paths <- !grepl("^\\/|~", yaml_bib_file)
    absolute_yaml_bib_file <- yaml_bib_file
    absolute_yaml_bib_file[relative_paths] <- paste(dirname(context$path), yaml_bib_file[relative_paths], sep = "/")

    if(betterbiblatex && use_betterbiblatex) {
      yaml_choices <- absolute_yaml_bib_file
      names(yaml_choices) <- yaml_bib_file
    }

    bib_message <- paste0("Bibliography file(s) found in YAML front matter", ifelse(exists("parent_document"), paste0(" of '", candidate_parents[parents], "'"), ""), ":")

    # Reload if new bibliography paths are used
    if(!all(absolute_yaml_bib_file == getOption("citr.bibliography_path")) & (!betterbiblatex | !use_betterbiblatex)) {
      options(citr.bibliography_path = absolute_yaml_bib_file)
      options(citr.bibliography_cache = NULL)
    }
  }

  ui <- miniPage(
    miniContentPanel(
      stableColumnLayout(
        selectizeInput(
          "selected_key"
          , choices = c(`No references found` = "")
          , label = ""
          , width = 700
          , multiple = TRUE
        )
      ),
      verbatimTextOutput("rendered_key"),
      stableColumnLayout(
        checkboxInput("in_paren", "In parentheses", value = TRUE),
        div(
          align = "right"
          , miniTitleBarButton("done", "  Insert citation  ", primary = TRUE)
          , miniTitleBarCancelButton()
        )
      ),
      hr(),
      uiOutput("bib_file"),
      uiOutput("zotero_status")
    )
  )

  server <- function(input, output, session) {

    reactive_variables <- reactiveValues(reload_bib = "init", use_betterbiblatex = use_betterbiblatex) # Set initial value

    # Zotero use
    observeEvent(input$disconnect_zotero, {
      options(citr.use_betterbiblatex = FALSE)
      reactive_variables$use_betterbiblatex <- FALSE

      ## Discard cache
      options(citr.bibliography_cache = NULL)
      reactive_variables$reload_bib <- paste0(sample(letters, 100, replace = TRUE), collapse = "") # Do stuff to trigger reload_bib reactive
    })

    observeEvent(input$connect_zotero, {
      options(citr.use_betterbiblatex = TRUE)
      reactive_variables$use_betterbiblatex <- TRUE

      ## Discard cache
      options(citr.bibliography_cache = NULL)
      reactive_variables$reload_bib <- paste0(sample(letters, 100, replace = TRUE), collapse = "") # Do stuff to trigger reload_bib reactive
    })

    output$zotero_status <- renderUI({
      if(betterbiblatex) {
        if(reactive_variables$use_betterbiblatex) {
          helpText(
            "Connected to Zotero."
            , actionLink("discard_cache", "Reload database")
            , "|"
            , actionLink("disconnect_zotero", "Disconnect")
          )
        } else {
          helpText(
            "Not connected to Zotero."
            , actionLink("connect_zotero", "Connect")
          )
        }
      }
    })

    output$bib_file <- renderUI({
      if(is.null(yaml_bib_file)) {
        div(
          textInput(
            ifelse(betterbiblatex && reactive_variables$use_betterbiblatex, "update_bib", "read_bib")
            , ifelse(betterbiblatex && reactive_variables$use_betterbiblatex, "Bib(La)Tex file to update", "Bib(La)Tex file to read")
            , value = bib_file
            , width = 700
          ),
          helpText(
            "YAML front matter missing or no bibliography file(s) specified."
            , if(!betterbiblatex || !reactive_variables$use_betterbiblatex) actionLink("discard_cache", "Reload file")
          )
        )
      } else {
        if(betterbiblatex && reactive_variables$use_betterbiblatex) {
          div(
            selectInput("update_bib", "Bib(La)Tex file to update", choices = yaml_choices),
            helpText(bib_message)
          )
        } else {
          helpText(
            bib_message
            , code(paste(yaml_bib_file, collapse = ", "))
            , actionLink("discard_cache", "Reload file(s)")
          )
        }
      }
    })

    # Discard cache reactive
    observeEvent(input$discard_cache, {
      options(citr.bibliography_cache = NULL)
      reactive_variables$reload_bib <- paste0(sample(letters, 100, replace = TRUE), collapse = "") # Do stuff to trigger reload_bib reactive
    })
    reload_bib <- reactive({reactive_variables$reload_bib})

    # Load bibliography
    bibliography <- reactive({
      trigger <- reload_bib() # Triggers reactive when event link is clicked
      if(
        (!is.null(input$read_bib) && !(all(input$read_bib == getOption("citr.bibliography_path")))) ||
        !reactive_variables$use_betterbiblatex #&& !is.null(input$update_bib) && !(all(input$update_bib == getOption("citr.bibliography_path"))))
      ) {
        options(citr.bibliography_cache = NULL)
      }

      # Use cached bibliography, if available
      if(
        is.null(getOption("citr.bibliography_cache")) #||
        # (!is.null(yaml_bib_file) && !isTRUE(all.equal(absolute_yaml_bib_file, getOption("citr.bibliography_path"))))
      ) {
        withProgress({
          if(betterbiblatex && reactive_variables$use_betterbiblatex) {

            if(!is.null(input$update_bib)) options(citr.bibliography_path = input$update_bib)
            current_bib <- load_betterbiblatex_bib()

          } else {
            if(is.null(yaml_bib_file)) { # Use specified bibliography

              if(!is.null(input$read_bib)) {
                options(citr.bibliography_path = input$read_bib)
                current_bib <- tryCatch(RefManageR::ReadBib(file = input$read_bib, check = FALSE), error = error_handler)
              } else {
                current_bib <- tryCatch(RefManageR::ReadBib(file = getOption("citr.bibliography_path"), check = FALSE), error = error_handler)
              }

            } else if(!is.null(yaml_bib_file)) { # Use YAML bibliography, if available

              options(citr.bibliography_path = absolute_yaml_bib_file)

              if(length(yaml_bib_file) == 1) {
                current_bib <- tryCatch(RefManageR::ReadBib(file = absolute_yaml_bib_file, check = FALSE), error = error_handler)
              } else {
                bibs <- lapply(absolute_yaml_bib_file, function(file) tryCatch(RefManageR::ReadBib(file, check = FALSE), error = error_handler))

                ## Merge if multiple bib files were imported succesfully
                not_found <- sapply(bibs, is.null)
                if(any(not_found)) warning("Unable to read bibliography file(s) ", paste(paste0("'", yaml_bib_file[not_found], "'"), collapse = ", "))
                current_bib <- do.call(c, bibs[!not_found])
              }
            }
          }
        }, message = "Loading bibliography...")

        ## Cache bibliography
        options(citr.bibliography_cache = current_bib)
      } else {
        current_bib <- getOption("citr.bibliography_cache")
      }

      current_bib
    })

    ## Update items in selection list
    observe({
      citation_keys <- names(bibliography())

      if(length(citation_keys > 0)) {
        current_references <- paste_references(bibliography())
        citation_keys <- citation_keys[order(current_references)]
        names(citation_keys) <- current_references[order(current_references)]

        updateSelectInput(session, "selected_key", choices = c(`Search terms` = "", citation_keys), label = "")
      } else {
        updateSelectInput(session, "selected_key", c(`.bib-file(s) not found` = ""), label = "")
      }
    })

    # Create citation based on current selection
    current_key <- reactive({
      if(length(input$selected_key) > 1 & input$in_paren) {
        paste_citation_keys(input$selected_key[order(input$selected_key)], input$in_paren)
      } else {
        paste_citation_keys(input$selected_key, input$in_paren)
      }
    })
    output$rendered_key <- renderText({if(!is.null(current_key())) current_key() else "No reference selected."})

    new_entries <- reactive({
      if(betterbiblatex && reactive_variables$use_betterbiblatex) {
        if(file.exists(input$update_bib)) {

          existing_bib <- tryCatch(RefManageR::ReadBib(input$update_bib, check = FALSE), error = error_handler)
          if(length(existing_bib) > 0) {
            new_references <- !input$selected_key %in% names(existing_bib)
          } else {
            new_references <- rep(TRUE, length(input$selected_key))
          }

          if(length(input$selected_key) > 0 && sum(new_references) > 0) {
            return(bibliography()[key = paste0("^", input$selected_key[new_references], "$", collapse = "|")])
          } else return(NULL)

        } else {
          return(bibliography()[key = paste0("^", input$selected_key, "$", collapse = "|")])
        }
      } else NULL
    })

    # Insert citation when button is clicked
    observeEvent(
      input$done
      , {
        # Update bib file
        if(betterbiblatex && reactive_variables$use_betterbiblatex) {
          if(!is.null(new_entries())) RefManageR::WriteBib(new_entries(), file = input$update_bib, append = TRUE)
        }

        # Insert citation
        if(!(current_key() %in% c("[@]", "@"))) rstudioapi::insertText(current_key())
        invisible(stopApp())
      }
    )
  }

  viewer <- dialogViewer("Insert citation", width = 600, height = 500)
  runGadget(ui, server, viewer = viewer)
}


stableColumnLayout <- function(...) {
  dots <- list(...)
  n <- length(dots)
  width <- 12 / n
  class <- sprintf("col-xs-%s col-md-%s", width, width)
  fluidRow(
    lapply(dots, function(el) {
      div(class = class, el)
    })
  )
}

error_handler <- function(x) {
  if(x$message != "unable to open file to read") {
    print(x)
    cat("\n")
  }

  NULL
}
