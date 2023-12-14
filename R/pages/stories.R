#' Necessary Packages/Functions

box::use(
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, fluidRow, tagList,
    uiOutput, renderUI,
    div,
    HTML
  ],
  shiny.router[get_query_param, get_page]
)

#' Missing description
#' @export

module_stories_ui <- function(id = "stories", label = "m_stories", type = "all") {
  ns <- NS(id)
  tagList(
    fluidPage(
      div(
        class = "panel-content",
        uiOutput(ns("stories"))
      )
    )
  )
}

#' Missing description
#' @export

module_stories_server <- function(id = "stories", con) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observeEvent(
        get_query_param(), {
          if (get_page() == "stories"){

            param_hf <- get_query_param("hf")
            if (is.null(param_hf)) param_hf <- 0

            if (param_hf == 1){
              ui_stories <- "<embed src='stories/index.html#category=Projekt' style = 'width: 100%; height: 90vh;'>"
            } else if (param_hf == 2){
              ui_stories <- "<embed src='stories/index.html#category=Coding' style = 'width: 100%; height: 90vh;'>"
            } else {
              ui_stories <- "<embed src='stories/index.html' style = 'width: 100%; height: 90vh;'>"
            }

            output$stories <- renderUI({HTML(ui_stories)})
          }
        }, ignoreNULL = FALSE
      )
    }
  )
}
