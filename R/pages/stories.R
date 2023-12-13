#' Necessary Packages/Functions

box::use(
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, fluidRow, tagList,
    uiOutput,
    div,
    HTML
  ]
)

#' Missing description
#' @export

module_stories_ui <- function(id = "stories", label = "m_stories", type = "all") {
  ns <- NS(id)
  tagList(
    fluidPage(
      div(
        class = "panel-content",
        HTML(
          ifelse(
            type == "handlung1",
            "<embed src='stories/index.html#category=Projekt' style = 'width: 100%; height: 90vh;'>",
            ifelse(
              type == "handlung2",
              "<embed src='stories/index.html#category=Coding' style = 'width: 100%; height: 90vh;'>",
              "<embed src='stories/index.html' style = 'width: 100%; height: 90vh;'>"
            )
          )
        )
      )
    )
  )
}

#' Missing description
#' @export

module_stories_server <- function(id = "stories", con, type = "all") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

    }
  )
}
