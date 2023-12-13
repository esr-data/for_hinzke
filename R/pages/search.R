#' Necessary Packages/Functions

box::use(
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, fluidRow, tagList,
    uiOutput,
    h2, div, icon
  ],
  shinyWidgets[searchInput],
  shinycssloaders[withSpinner]
)

#' Missing description
#' @noRd

module_search_ui <- function(id = "search", label = "m_search", type = "all") {
  ns <- NS(id)
  tagList(
    fluidPage(
      div(
        class = "panel-content",
        h2("Suche"),
        fluidRow(
          style = "padding: 10px; display: flex; margin: 0;",
          searchInput(
            inputId     = ns("search"),
            label       = "Click search icon to update or hit 'Enter'",
            placeholder = "A placeholder",
            btnSearch   = icon("search"),
            btnReset    = icon("remove"),
            width       = "100%"
          )
        ),
        withSpinner(uiOutput(ns("results")))
      )
    )
  )
}

#' Missing description
#' @noRd

module_search_server <- function(id = "search", type = "all", con) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # observeEvent(
      #   input$search, {
      #     output$results <-
      #       renderUI({
      #         input_search <- input$search
      #         if (!is.null(input_search)){
      #           if (!is.na(input_search)){
      #             if (nchar(input_search) > 0){
      #               results <- get_results(input_search, limit_results_to = 10)
      #               test    <- as.data.frame(results[[2]])[,2:6]
      #               test$rank <- unlist(test$rank)
      #               test$ranking_weight <- round(unlist(test$ranking_weight), 2)
      #               reactable(test[order(test$rank, decreasing = FALSE),])
      #             }
      #           }
      #         }
      #
      #       })
      #
      #   }
      # )

    }
  )
}

