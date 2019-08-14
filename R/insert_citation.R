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
#' @return Inserts selected Markdown citation(s) at current location.
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

  if (!missing(encoding)) {
    warning("Argument 'encoding' is deprecated; set the global option 'citr.encoding' instead.", call. = FALSE)
  }

  if (!missing(use_betterbiblatex)) {
    warning("Argument 'use_betterbiblatex' is deprecated; set the global option 'citr.use_betterbiblatex' instead.", call. = FALSE)
  }

  if (!missing(betterbiblatex_format)) {
    warning("Argument 'use_betterbiblatex' is deprecated; set the global option 'citr.betterbiblatex_format' instead.", call. = FALSE)
  }

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
  } else {
    bbt_libraries_options <- NULL
  }

  # Get bibliography files from YAML front matter if available
  if(context$path == "") {
    message(
      "\nUnsaved R Markdown document: Cannot locate bibliography files;
         falling back to manual path specification.\n"
    )
    yaml_bib_file <- NULL
  } else {
    yaml_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", context$contents)
    yaml_bib_file <- get_bib_from_yaml(
      yaml_delimiters
      , context$contents
      , rmd_path = dirname(context$path)
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

    parent_document <- readLines(parents_path[parents], warn = FALSE)
    parent_yaml_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", parent_document)

    yaml_bib_file <- get_bib_from_yaml(
      parent_yaml_delimiters
      , parent_document
      , rmd_path = dirname(parents_path[parents])
      , bib_file = yaml_bib_file
    )
  }

  if(!is.null(yaml_bib_file)) {
    relative_paths <- !grepl("^(\\/|~|\\w*:\\/+)", yaml_bib_file)
    absolute_yaml_bib_file <- sapply(yaml_bib_file, tools::file_path_as_absolute)
    # absolute_yaml_bib_file[relative_paths] <- paste(
    #   dirname(context$path)
    #   , yaml_bib_file[relative_paths]
    #   , sep = "/"
    # )

    # if(betterbiblatex && use_betterbiblatex) {
      yaml_choices <- absolute_yaml_bib_file
      names(yaml_choices) <- yaml_bib_file
    # }

    bib_message <- paste0(
      "Bibliography files found in YAML front matter"
      , ifelse(
        exists("parent_document")
        , paste0(" of '", candidate_parents[parents], "'")
        , "")
      , ":"
    )

    # Reload if new bibliography paths are used
    if(
      !all(absolute_yaml_bib_file == getOption("citr.bibliography_path")) &
      (!betterbiblatex | !getOption("citr.use_betterbiblatex"))
    ) {
      options(citr.bibliography_path = absolute_yaml_bib_file)
      options(citr.bibliography_cache = NULL)
    }
  }

  ui <- miniPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
          .shiny-output-error-validation {
            color: red;
            font-weight: bold;
          }
        ")),
      tags$script(
        type = "text/javascript"
        , "focus_searchbox = function() {
            	let select = $('#selected_key').selectize();
            	select[0].selectize.focus();
            };
        ")
    ),

    miniTabstripPanel(
      id = "tabs"
      , miniTabPanel(
        title = "Insert citation"
        , icon = icon("paste")
        , miniContentPanel(

          selectizeInput(
            "selected_key"
            , choices = c("No bibliography found" = "")
            , label = ""
            , width = "100%"
            , multiple = TRUE
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
          uiOutput("read_error")
        )
      )
      # , miniTabPanel(
      #   "Search Zotero"
      #   , icon = icon("link")
      #   , NULL
      # )
      , miniTabPanel(
        title = "Settings"
        , icon = icon("cog")
        , miniContentPanel(
          stableColumnLayout(
            div(
              selectizeInput(
                inputId = "parent_documents"
                , label = "Possible parent document names"
                , choices = candidate_parents
                , selected = getOption("citr.parent_documents")
                , multiple = TRUE
                , options = list(create = TRUE)
              )
            ),
            selectizeInput(
              inputId = "bib_encoding"
              , label = "Bibliography file encoding"
              , choices = c("latin1", "UTF-8", "unknown")
              , selected = getOption("citr.encoding")
              , multiple = FALSE
            )
          ),
          hr(),
          h4("Zotero connection"),
          # checkboxInput(
          #   inputId = "use_bbt"
          #   , label = "Search Zotero database"
          #   , value = getOption("citr.use_betterbiblatex")
          # ),
          helpText(span("Requires the"), a("Better BibTeX", href = "https://github.com/retorquere/zotero-better-bibtex"), span("extension and Zotero to run.")),
          stableColumnLayout(
            uiOutput("bbt_libraries"),
            div(
              uiOutput("zotero_bib_file"),
              radioButtons(
                inputId = "bbt_format"
                , label = NULL # "References format"
                , choices = list("BibTeX" = "bibtex", "BibLaTeX" = "biblatex")
                , selected = getOption("citr.betterbiblatex_format")
                , inline = TRUE
              )
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

    # Set initial value
    reactive_variables <- reactiveValues(
      reload_bib = "init"
      , use_betterbiblatex = getOption("citr.use_betterbiblatex")
      , exclude_betterbiblatex_library = getOption("citr.exclude_betterbiblatex_library")
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

    observeEvent(input$switch_to_preferences, {
      updateTabsetPanel(
        session
        , inputId = "tabs"
        , selected = "Settings"
      )
    })

    observeEvent(input$zotero_groups, {
      new_excludes <- bbt_libraries_options[!bbt_libraries_options %in% input$zotero_groups]
      options("citr.exclude_betterbiblatex_library" = new_excludes)
      reactive_variables$exclude_betterbiblatex_library <- new_excludes
    })

    output$zotero_status <- renderUI({
      if(betterbiblatex) {
        if(reactive_variables$use_betterbiblatex) {
          helpText(
            "Connected to Zotero"
            , if(length(bbt_libraries_options[!bbt_libraries_options %in% reactive_variables$exclude_betterbiblatex_library]) > 0) {
              list(
                "libraries"
                , code(paste(bbt_libraries_options[!bbt_libraries_options %in% reactive_variables$exclude_betterbiblatex_library], collapse = ", "))
                , ". References are added to"
                , code(basename(c(input$update_bib, getOption("citr.update_bib"))[1]))
                , "."
                  # , actionLink("switch_to_preferences", "Select file")
                , actionLink("discard_cache2", "Reload libraries")
              )
            } else {
              actionLink("switch_to_preferences", "Select libraries")
            }
            , "|"
            , actionLink("disconnect_zotero", "Disconnect")
          )
        } else {
          helpText(
            "Zotero connection available."
            , actionLink("connect_zotero", "Connect and load libraries")
          )
        }
      }
    })

    output$bbt_libraries <- renderUI({
      # if(betterbiblatex) {
      #   zotero_groups_checkbox <- checkboxGroupInput(
      #     "zotero_groups"
      #     , label = "Zotero libraries to load"
      #     , choices = as.list(bbt_libraries_options)
      #     , selected = bbt_libraries_options[
      #       !bbt_libraries_options %in% c(getOption("citr.exclude_betterbiblatex_library"), "citr_dummy")
      #       ]
      #     , inline = TRUE
      #   )
      #
      #   if(
      #     !length(bbt_libraries_options) > 1 ||
      #     !reactive_variables$use_betterbiblatex ||
      #     !reactive_variables$show_zotero_options
      #   ) {
      #     zotero_groups_checkbox$attribs$style <- "display:none;"
      #   }
      #
      #   zotero_groups_checkbox
      # }

      selectizeInput(
        "zotero_groups"
        , label = "Zotero libraries to load"
        , choices = as.list(bbt_libraries_options)
        , selected = bbt_libraries_options[
          !bbt_libraries_options %in% getOption("citr.exclude_betterbiblatex_library")
          ]
        , multiple = TRUE
      )
    })


    # Discard cache reactive
    observeEvent(input$discard_cache, {
      reactive_variables <- discard_cache(reactive_variables)
    })

    observeEvent(input$discard_cache2, { # Required to avoid conflict between reload files and reload libraries
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

          # if(reactive_variables$reload_bib == "init") { # Don't load Zotero bibs automatically
          #   current_bib <- c()
          # } else {
            # # Update list of bibs to exclude
            # reactive_variables$exclude_betterbiblatex_library <- c(
            #   bbt_libraries_options[!bbt_libraries_options %in% input$zotero_groups]
            #   , "citr_dummy"
            # )
            # options("citr.exclude_betterbiblatex_library" = reactive_variables$exclude_betterbiblatex_library)

            # Load remaining bibs
            if(!all(bbt_libraries_options %in% reactive_variables$exclude_betterbiblatex_library)) {
              shiny::withProgress({
                current_bib <- load_betterbiblatex_bib(
                  encoding = getOption("citr.encoding")
                  , betterbiblatex_format = getOption("citr.betterbiblatex_format")
                  , exclude_betterbiblatex_library = reactive_variables$exclude_betterbiblatex_library
                  , increment_progress = TRUE
                )
              }, message = "Loading Zotero libraries...")
            } else {
              current_bib <- c()
            }
          # }

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

              current_bib <- read_bib_catch_error(bib_to_read, getOption("citr.encoding"))

            } else if(!is.null(yaml_bib_file)) { # Use YAML bibliography, if available

              options(citr.bibliography_path = absolute_yaml_bib_file)

              if(length(yaml_bib_file) == 1) {
                setProgress(detail = basename(absolute_yaml_bib_file))
                current_bib <- read_bib_catch_error(absolute_yaml_bib_file, getOption("citr.encoding"))
              } else {
                bibs <- lapply(absolute_yaml_bib_file, function(file) {
                  setProgress(detail = basename(file))
                  bib <- read_bib_catch_error(file, getOption("citr.encoding"))
                  shiny::incProgress(1/length(absolute_yaml_bib_file))
                  bib
                  })

                ## Merge if multiple bib files were imported succesfully
                not_found <- sapply(bibs, is.null)
                if(any(not_found)) {
                  warning(
                    "Unable to read bibliography files "
                    , paste(paste0("'", yaml_bib_file[not_found], "'"), collapse = ", ")
                  )
                }
                current_bib <- do.call(c, bibs[!not_found])
              }
            }
          }, message = "Loading bibliography files...")
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

        citation_keys <- c("Search references" = "", citation_keys)
        names(citation_keys) <- enc2native(names(citation_keys))

        updateSelectizeInput(
          session
          , "selected_key"
          , choices = citation_keys
          , label = ""
        )

      } else {
        if(
          betterbiblatex &&
          reactive_variables$use_betterbiblatex
        ) {
          selected_key_default <- c("No references found. (Re-)Load Zotero libraries." = "")
        } else {
          selected_key_default <- c("Bibliography files not found" = "")
        }
        updateSelectizeInput(session, "selected_key", selected_key_default, label = "")
      }
    })

    observe({
      citation_keys <- names(bibliography())

      if(length(citation_keys > 0)) {
        shinyjs::delay(100, shinyjs::runjs("focus_searchbox();"))
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
          if(file.exists(getOption("citr.update_bib"))) {

            if(
              file.info(getOption("citr.update_bib"))$mtime != getOption("citr.bib_file_last_modified") ||
              is.null(getOption("citr.bib_file_cache"))
            ) {
              existing_bib <- read_bib_catch_error(getOption("citr.update_bib"), getOption("citr.encoding"))
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
            # This is equivalent to check = FALSE for c.BibEntry()
            old_BibOptions <- RefManageR::BibOptions(check.entries = FALSE)

            RefManageR::WriteBib(
              new_entries()
              , file = getOption("citr.update_bib")
              , append = TRUE
            )
            options(citr.bib_file_last_modified = file.info(getOption("citr.update_bib"))$mtime)

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
            "read_bib"
            , "Bibliography file to read"
            , value = bib_file
            , width = 700
          ),
          helpText(
            "YAML front matter missing or no bibliography files specified."
            , if(!betterbiblatex || !reactive_variables$use_betterbiblatex) {
              actionLink("discard_cache", "Reload file")
            }
          )
        )
      } else {
        if(betterbiblatex && reactive_variables$use_betterbiblatex) {
          # helpText(
          #   "References are added to"
          #   , code(paste(yaml_bib_file, collapse = ", "))
          #   # , actionLink("switch_to_preferences", "Select file")
          # )
        } else {
          helpText(
            bib_message
            , code(paste(gsub(dirname(context$path), ".", yaml_bib_file), collapse = ", "))
            , actionLink("discard_cache", "Reload files")
          )
        }
      }
    })

    output$zotero_bib_file <- renderUI({
      if(is.null(yaml_bib_file)) {
        div(
          textInput(
            inputId = "update_bib"
            , label = "Add references to"
            , value = getOption("citr.update_bib")
          ),
          helpText("YAML front matter missing or no bibliography files specified.")
        )
      } else {
        update_bib_files <- unique(c(yaml_choices, gsub("./", paste0(dirname(context$path), "/"), getOption("citr.update_bib"), fixed = TRUE)))
        # names(update_bib_files) <- basename(update_bib_files)
        # duplicated_basenames <- names(update_bib_files) %in% names(update_bib_files)[duplicated(names(update_bib_files))]
        # if(any(duplicated_basenames)) {
          # update_bib_names <- names(update_bib_files)
          # update_bib_names[duplicated_basenames] <- paste0(names(update_bib_files[duplicated_basenames]), " - ", dirname(update_bib_files[duplicated_basenames]), "/")
          update_bib_names <- paste0(basename(update_bib_files), " - ", dirname(update_bib_files), "/")
          update_bib_names <- gsub("./", paste0(dirname(context$path), "/"), update_bib_names, fixed = TRUE)
          names(update_bib_files) <- update_bib_names
        # }

        selectizeInput(
          "update_bib"
          , "Add references to"
          , choices = update_bib_files
          , selected = getOption("citr.update_bib")
          , options = list(
            create = TRUE
            , sortField = "text"
            , render = I(
              '{
    item: function(item, escape) {
      var splittedLabel = escape(item.label).split(" - ");
      if (splittedLabel.length > 1) {
        return "<div>" + splittedLabel[0] + " <br><i style=\'color: #A9A9A9; font-size: 0.8em;\'>" + splittedLabel[1] + "</i></div>";
      } else {
        return "<div>" + splittedLabel[0] + "</div>";
      }
    },
    option: function(item, escape) {
      var splittedLabel = escape(item.label).split(" - ");
      if (splittedLabel.length > 1) {
        return "<div>" + splittedLabel[0] + " <br><i style=\'color: #A9A9A9; font-size: 0.8em;\'>" + splittedLabel[1] + "</i></div>";
      } else {
        return "<div>" + splittedLabel[0] + "</div>";
      }
    }
  }'
            )
          )
        )
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

    # Preferences
    observeEvent(
      input$parent_documents
      , {
        options("citr.parent_documents" = input$parent_documents)
      }
    )

    observeEvent(
      input$bib_encoding
      , {
        options("citr.encoding" = input$bib_encoding)
      }
    )

    # observeEvent(
    #   input$use_bbt
    #   , {
    #     reactive_variables$use_betterbiblatex <- input$use_bbt
    #     options("citr.use_betterbiblatex" = input$use_bbt)
    # })

    observeEvent(
      input$bbt_format
      , {
        options("citr.betterbiblatex_format" = input$bbt_format)
      }
    )

    observeEvent(
      input$update_bib
      , {
        options("citr.update_bib" = input$update_bib)
      }
    )
  }


  viewer <- dialogViewer("Insert citation", width = 600, height = 420)
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
      "Error in RefManageR::ReadBib(): Failed to read bibliography file\n\n"
      , error_message
      , "\n"
    )
  }
}

## Let's hope this doesn't cause too much trouble; this is a lot more
## sofisticated in rmarkdown, but the functions are not exported.
get_bib_from_yaml <- function(yaml_delimiters, file_contents, rmd_path, bib_file = NULL) {
  if(
    length(yaml_delimiters) >= 2 &&
    (yaml_delimiters[2] - yaml_delimiters[1] > 1) &&
    grepl("^---\\s*$", file_contents[yaml_delimiters[1]])
  ) {
    yaml_front_matter <- paste(
      file_contents[(yaml_delimiters[1] + 1):(yaml_delimiters[2] - 1)]
      , collapse = "\n"
    )
    absolute_bib_file <- sapply(
      file.path(rmd_path, yaml::yaml.load(yaml_front_matter)$bibliography)
      , tools::file_path_as_absolute
    )
    c(bib_file, absolute_bib_file)
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
  options(citr.citation_key_cache = NULL)
  x$reload_bib <- make_hash()
  x
}

check_for_errorneous_bib_cache <- function(x) {
  is.character(x) && grepl("^Error: ", x)
}

read_bib_catch_error <- function(x, encoding) {
  bib <- tryCatch(
    RefManageR::ReadBib(x, check = FALSE, .Encoding = encoding)
    , error = error_handler
  )
  bib
}
