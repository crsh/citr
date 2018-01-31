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

insert_citation <- function(
  bib_file = getOption("citr.bibliography_path")
  , use_betterbiblatex = getOption("citr.use_betterbiblatex")
  , betterbiblatex_format = getOption("citr.betterbiblatex_format")
  , encoding = getOption("citr.encoding")
) {
  assert_that(is.character(bib_file))
  assert_that(is.flag(use_betterbiblatex))
  assert_that(is.character(betterbiblatex_format))
  assert_that(is.character(encoding))

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

  betterbiblatex <- betterbiblatex_available()
  if(betterbiblatex) {
    bbt_libraries <- query_bbt_libraries()
    bbt_libraries_options <- unlist(bbt_libraries[, "name"])
    names(bbt_libraries_options) <- unlist(bbt_libraries[, "name"])
  }

  # Get bibliography files from YAML front matter if available
  if(context$path == "") {
    message(
      "\nUnsaved R Markdown document: Cannot locate Bib(La)TeX file(s);
         falling back to manual path specification.\n"
    )
    yaml_bib_file <- NULL
  } else {
    yaml_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", context$contents)
    yaml_bib_file <- get_bib_from_yaml(
      yaml_delimiters
      , context$contents
      , bib_file = NULL
    )
  }

  ## Always look up index.Rmd if document is in bookdown directory
  candidate_parents <- getOption("citr.parent_documents")
  bookdown_files <- paste(dirname(context$path), c("_bookdown.yml", "_site.yml"), sep = "/")
  if(any(file.exists(bookdown_files))) {
    if(!"index.Rmd" %in% candidate_parents) {
      candidate_parents <- c(candidate_parents, "index.Rmd")
    }
  }

  parents_path <- paste(dirname(context$path), candidate_parents, sep = "/")
  parents <- file.exists(parents_path)

  ## Search parent documents for bibliography files
  if(any(parents)) {
    if(sum(parents) > 1) {
      stop("More than one parent document found. See getOption('citr.parent_documents').")
    }

    parent_document <- readLines(parents_path[parents], encoding = encoding)
    parent_yaml_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", parent_document)

    yaml_bib_file <- get_bib_from_yaml(
      parent_yaml_delimiters
      , parent_document
      , bib_file = yaml_bib_file
    )
  }

  if(!is.null(yaml_bib_file)) {
    relative_paths <- !grepl("^(\\/|~|\\w*:\\/+)", yaml_bib_file)
    absolute_yaml_bib_file <- yaml_bib_file
    absolute_yaml_bib_file[relative_paths] <- paste(
      dirname(context$path)
      , yaml_bib_file[relative_paths]
      , sep = "/"
    )

    if(betterbiblatex && use_betterbiblatex) {
      yaml_choices <- absolute_yaml_bib_file
      names(yaml_choices) <- yaml_bib_file
    }

    bib_message <- paste0(
      "Bibliography file(s) found in YAML front matter"
      , ifelse(
        exists("parent_document")
        , paste0(" of '", candidate_parents[parents], "'")
        , "")
      , ":"
    )

    # Reload if new bibliography paths are used
    if(
      !all(absolute_yaml_bib_file == getOption("citr.bibliography_path")) &
      (!betterbiblatex | !use_betterbiblatex)
    ) {
      options(citr.bibliography_path = absolute_yaml_bib_file)
      options(citr.bibliography_cache = NULL)
    }
  }

  ui <- miniPage(
    miniContentPanel(

      tags$head(
        tags$style(HTML("
          .shiny-output-error-validation {
            color: red;
            font-weight: bold;
          }
        "))
      ),

      stableColumnLayout(
        selectizeInput(
          "selected_key"
          , choices = c("No references found" = "")
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
      uiOutput("zotero_status"),
      uiOutput("read_error"),
      uiOutput("bbt_libraries")
    )
  )

  server <- function(input, output, session) {

    # Set initial value
    reactive_variables <- reactiveValues(
      reload_bib = "init"
      , use_betterbiblatex = use_betterbiblatex
      , show_zotero_options = FALSE
    )

    # Zotero use
    observeEvent(input$disconnect_zotero, {
      options(citr.use_betterbiblatex = FALSE)
      reactive_variables$use_betterbiblatex <- FALSE

      reactive_variables <- discard_cache(reactive_variables)
    })

    observeEvent(input$connect_zotero, {
      options(citr.use_betterbiblatex = TRUE)
      reactive_variables$use_betterbiblatex <- TRUE

      reactive_variables <- discard_cache(reactive_variables)
    })

    observeEvent(input$zotero_toggle_options, {
      reactive_variables$show_zotero_options <- !reactive_variables$show_zotero_options
    })

    output$zotero_status <- renderUI({
      if(betterbiblatex) {
        if(reactive_variables$use_betterbiblatex) {
          helpText(
            "Connected to Zotero:"
            , actionLink(
              "discard_cache"
              , if(length(bibliography()) == 0) "Load libraries" else "Reload libraries"
            )
            , "|"
            , actionLink(
              "zotero_toggle_options"
              , if(!reactive_variables$show_zotero_options) "Select libraries" else "Hide libraries"
            )
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

    output$bbt_libraries <- renderUI({
      if(betterbiblatex) {
        zotero_groups_checkbox <- checkboxGroupInput(
          "zotero_groups"
          , label = NULL
          , choices = as.list(bbt_libraries_options)
          , selected = bbt_libraries_options[
            !bbt_libraries_options %in% c(getOption("citr.exclude_betterbiblatex_library"), "citr_dummy")
            ]
          , inline = TRUE
        )

        if(
          !length(bbt_libraries_options) > 1 ||
          !reactive_variables$use_betterbiblatex ||
          !reactive_variables$show_zotero_options
        ) {
          zotero_groups_checkbox$attribs$style <- "display:none;"
        }

        zotero_groups_checkbox
      }
    })


    # Discard cache reactive
    observeEvent(input$discard_cache, {
      reactive_variables <- discard_cache(reactive_variables)
    })

    reload_bib <- reactive({reactive_variables$reload_bib})


    # Load bibliography
    bibliography <- reactive({
      trigger <- reload_bib() # Triggers reactive when event link is clicked

      bib_file_specified <- !is.null(input$read_bib)
      bib_files_known <- all(input$read_bib == getOption("citr.bibliography_path"))

      # Discard cache
      if(
        (bib_file_specified && !bib_files_known) &&
        !reactive_variables$use_betterbiblatex
      ) {
        options(citr.bibliography_cache = NULL)
      }

      # Use cached bibliography, if available
      cached_bibliography <- getOption("citr.bibliography_cache")

      if(
        !is.null(cached_bibliography) &&
        !check_for_errorneous_bib_cache(cached_bibliography)
      ) {
        current_bib <- cached_bibliography
      } else {

        if(betterbiblatex && reactive_variables$use_betterbiblatex) {

          if(!is.null(input$update_bib)) {
            options(citr.bibliography_path = input$update_bib)
          }

          if(reactive_variables$reload_bib == "init") { # Don't load Zotero bibs automatically
            current_bib <- c()
          } else {
            # Update list of bibs to exclude
            exclude_betterbiblatex_library <- c(
              bbt_libraries_options[!bbt_libraries_options %in% input$zotero_groups]
              , "citr_dummy"
            )
            options("citr.exclude_betterbiblatex_library" = exclude_betterbiblatex_library)

            # Load remaining bibs
            if(!all(bbt_libraries_options %in% exclude_betterbiblatex_library)) {
              shiny::withProgress({
                current_bib <- load_betterbiblatex_bib(
                  encoding = encoding
                  , betterbiblatex_format = betterbiblatex_format
                  , exclude_betterbiblatex_library = exclude_betterbiblatex_library
                  , increment_progress = TRUE
                )
              }, message = "Loading Zotero libraries...")
            } else {
              current_bib <- c()
            }
          }

        } else {
          shiny::withProgress({
            if(is.null(yaml_bib_file)) { # Use specified bibliography

              if(bib_file_specified) {
                options(citr.bibliography_path = input$read_bib)
                setProgress(detail = input$read_bib)
                bib_to_read <- input$read_bib
              } else {
                setProgress(detail = getOption("citr.bibliography_path"))
                bib_to_read <- getOption("citr.bibliography_path")
              }

              current_bib <- read_bib_catch_error(bib_to_read, encoding)

            } else if(!is.null(yaml_bib_file)) { # Use YAML bibliography, if available

              options(citr.bibliography_path = absolute_yaml_bib_file)

              if(length(yaml_bib_file) == 1) {
                setProgress(detail = basename(absolute_yaml_bib_file))
                current_bib <- read_bib_catch_error(absolute_yaml_bib_file, encoding)
              } else {
                bibs <- lapply(absolute_yaml_bib_file, function(file) {
                  setProgress(detail = basename(file))
                  read_bib_catch_error(file, encoding)
                  shiny::incProgress(1/length(absolute_yaml_bib_file))
                  })

                ## Merge if multiple bib files were imported succesfully
                not_found <- sapply(bibs, is.null)
                if(any(not_found)) {
                  warning(
                    "Unable to read bibliography file(s) "
                    , paste(paste0("'", yaml_bib_file[not_found], "'"), collapse = ", ")
                  )
                }
                current_bib <- do.call(c, bibs[!not_found])
              }
            }
          }, message = "Loading bibliography file(s)...")
        }


        ## Cache bibliography
        options(citr.bibliography_cache = current_bib)
      }

      current_bib
    })

    ## Update items in selection list
    observe({
      citation_keys <- names(bibliography())
      cached_citation_keys <- getOption("citr.citation_key_cache")

      if(length(citation_keys > 0)) {
        if(
          is.null(cached_citation_keys) ||
          length(setdiff(citation_keys, cached_citation_keys)) != 0
        ) {
          shiny::withProgress({
            current_references <- paste_references(bibliography())
            incProgress(1/4)
            citation_keys <- citation_keys[order(current_references)]
            incProgress(1/4)
            names(citation_keys) <- current_references[order(current_references)]
            incProgress(1/4)
            options(citr.citation_key_cache = citation_keys)
            incProgress(1/4)
          }
          , message = "Indexing bibliography..."
          )
        } else {
          citation_keys <- getOption("citr.citation_key_cache")
        }

        updateSelectInput(
          session
          , "selected_key"
          , choices = c("Search terms" = "", citation_keys)
          , label = ""
        )
      } else {
        if(
          betterbiblatex &&
          reactive_variables$use_betterbiblatex
        ) {
          selected_key_default <- c("No references found. (Re-)Load Zotero libraries." = "")
        } else {
          selected_key_default <- c(".bib-file(s) not found" = "")
        }
        updateSelectInput(session, "selected_key", selected_key_default, label = "")
      }
    })

    # Create citation based on current selection
    current_key <- reactive({
      if(length(input$selected_key) > 1 & input$in_paren) {
        paste_citation_keys(
          input$selected_key[order(input$selected_key)]
          , input$in_paren
        )
      } else {
        paste_citation_keys(
          input$selected_key
          , input$in_paren
        )
      }
    })
    output$rendered_key <- renderText(
      {if(!is.null(current_key())) current_key() else "No reference selected."}
    )

    new_entries <- reactive({
      shiny::withProgress({
        if(betterbiblatex && reactive_variables$use_betterbiblatex) {
          if(file.exists(input$update_bib)) {

            if(
              file.info(input$update_bib)$mtime != getOption("citr.bib_file_last_modified") ||
              is.null(getOption("citr.bib_file_cache"))
            ) {
              existing_bib <- read_bib_catch_error(input$update_bib, encoding)
              options(citr.bib_file_cache = existing_bib)
            } else {
              existing_bib <- getOption("citr.bib_file_cache")
            }

            if(length(existing_bib) > 0) {
              new_references <- !input$selected_key %in% names(existing_bib)
            } else {
              new_references <- rep(TRUE, length(input$selected_key))
            }

            if(length(input$selected_key) > 0 && sum(new_references) > 0) {
              new_bib_key <- paste0("^", input$selected_key[new_references], "$", collapse = "|")
              return(bibliography()[key = new_bib_key])
            } else return(NULL)

          } else {
            return(bibliography()[key = paste0("^", input$selected_key, "$", collapse = "|")])
          }
        } else NULL
      }, message = "Updating bibliography file...")
    })

    # Insert citation when button is clicked
    observeEvent(
      input$done
      , {
        # Update bib file
        if(betterbiblatex && reactive_variables$use_betterbiblatex) {
          if(!is.null(new_entries())) {
            RefManageR::WriteBib(
              new_entries()
              , file = input$update_bib
              , append = TRUE
            )
            options(citr.bib_file_last_modified = file.info(input$update_bib)$mtime)

            # This is equivalent to check = FALSE for c.BibEntry()
            old_BibOptions <- RefManageR::BibOptions(check.entries = FALSE)
            options(
              citr.bib_file_cache = c(
                getOption("citr.bib_file_cache")
                , RefManageR::as.BibEntry(new_entries())
              )
            )
            RefManageR::BibOptions(old_BibOptions)
          }
        }

        # Insert citation
        if(!(current_key() %in% c("[@]", "@"))) {
          rstudioapi::insertText(text = current_key(), id = context$id)
        }
        invisible(stopApp())
      }
    )


    output$bib_file <- renderUI({
      if(is.null(yaml_bib_file)) {
        div(
          textInput(
            ifelse(
              betterbiblatex && reactive_variables$use_betterbiblatex
              , "update_bib"
              , "read_bib"
            )
            , ifelse(
              betterbiblatex && reactive_variables$use_betterbiblatex
              , "Bib(La)Tex file to update"
              , "Bib(La)Tex file to read"
            )
            , value = bib_file
            , width = 700
          ),
          helpText(
            "YAML front matter missing or no bibliography file(s) specified."
            , if(!betterbiblatex || !reactive_variables$use_betterbiblatex) {
              actionLink("discard_cache", "Reload file")
            }
          )
        )
      } else {
        if(betterbiblatex && reactive_variables$use_betterbiblatex) {
          div(
            selectInput(
              "update_bib"
              , "Bib(La)Tex file to update"
              , choices = yaml_choices
            ),
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

    output$read_error <- renderText({
      validate(
        need(
          !check_for_errorneous_bib_cache(bibliography())
          , bibliography()
        )
      )
    })
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
    error_message <- print(x)
    paste(
      "Error in RefManageR::ReadBib(): Failed to read Bib(La)TeX-file\n\n"
      , error_message
      , "\n"
    )
  }
}

## Let's hope this doesn't cause too much trouble; this is a lot more
## sofisticated in rmarkdown, but the functions are not exported.
get_bib_from_yaml <- function(yaml_delimiters, file_contents, bib_file = NULL) {
  if(
    length(yaml_delimiters) >= 2 &&
    (yaml_delimiters[2] - yaml_delimiters[1] > 1) &&
    grepl("^---\\s*$", file_contents[yaml_delimiters[1]])
  ) {
    yaml_front_matter <- paste(
      file_contents[(yaml_delimiters[1] + 1):(yaml_delimiters[2] - 1)]
      , collapse = "\n"
    )
    c(bib_file, yaml::yaml.load(yaml_front_matter)$bibliography)
  } else {
    NULL
  }
}

# Create randomness to trigger reloading bib reactive
make_hash <- function() {
  paste0(sample(letters, 100, replace = TRUE), collapse = "")
}

discard_cache <- function(x) {
  options(citr.bibliography_cache = NULL)
  x$reload_bib <- make_hash()
  x
}

check_for_errorneous_bib_cache <- function(x) {
  is.character(x) && grepl("^Error: ", x)
}

read_bib_catch_error <- function(x, encoding) {
  tryCatch(
    RefManageR::ReadBib(x, check = FALSE, .Encoding = encoding)
    , error = error_handler
  )
}
