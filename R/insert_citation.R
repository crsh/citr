#' Invoke RStudio addin to insert Markdown citations
#'
#' @param bib_file Character. Path to BibTeX-file. See details.
#'
#' @details The path to the BibTeX-file can be set in the global options and is set to
#'    \code{references.bib} when the package is loaded. Once the path is changed in the
#'    RStudio addin, the global option is updated.
#'
#' @import miniUI
#' @import shiny
#'
#' @return Inserts selected Markdown citation(s) at currenct location.
#' @seealso \code{\link{insert_citation}}
#' @export
#'
#' @examples
#' \dontrun{
#'  insert_citation(bib_file = "references.bib")
#' }

insert_citation <- function(bib_file = options("bibliography_path")) {

  ui <- miniPage(

    miniContentPanel(
      stableColumnLayout(
        selectizeInput(
          "selected_key"
          , choices = c(`BibTex file not found` = "")
          , label = ""
          , width = 700
          , multiple = TRUE
        )
      ),
      verbatimTextOutput("rendered_key"),
      stableColumnLayout(
        checkboxInput("in_paren", "In parentheses", value = TRUE),
        fluidRow(
          miniTitleBarButton("done", "  Insert citation  ", primary = TRUE),
          miniTitleBarCancelButton()
        )
      ),
      br(),
      textInput("bib_file", "Path to BibTeX file:", value = bib_file, width = 700)
    )
  )

  server <- function(input, output, session) {

    # Load bibliography
    bibliography <- reactive({
      options(bibliography_path = input$bib_file)
      tryCatch(bibtex::read.bib(file = input$bib_file), error = function(e) NULL)
    })

    ## Update items in selection list
    observe({
      citation_keys <- names(bibliography())

      if(length(citation_keys > 0)) {
        names(citation_keys) <- paste_references(bibliography())

        updateSelectInput(session, "selected_key", choices = c(`Search terms` = "", citation_keys), label = "")
      }
    })

    # Create citation based on current selection
    current_key <- reactive({paste_citation_keys(input$selected_key, input$in_paren)})
    output$rendered_key <- renderText({if(!(current_key() %in% c("[@]", "@"))) current_key() else "No reference selected."})

    # Insert citation when button is clicked
    observeEvent(
      input$done
      , {
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