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

module_suchen_ui <- function(id = "suchen", label = "m_suchen", type = "all") {
  ns <- NS(id)
  tagList(
    fluidPage(
      div(
        class = "panel-content",
        h2("Suche"),
        fluidRow(
          style = "padding: 10px; display: flex; margin: 0;",
          searchInput(
            inputId     = ns("suchen"),
            label       = "Click suchen icon to update or hit 'Enter'",
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

module_suchen_server <- function(id = "suchen", type = "all") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # observeEvent(
      #   input$suchen, {
      #     output$results <-
      #       renderUI({
      #         input_suchen <- input$suchen
      #         if (!is.null(input_suchen)){
      #           if (!is.na(input_suchen)){
      #             if (nchar(input_suchen) > 0){
      #               results <- get_results(input_suchen, limit_results_to = 10)
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

