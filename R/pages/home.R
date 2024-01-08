#' Necessary Packages/Functions

box::use(
  ../../R/utils/database[get_query],
  shiny[
    NS, moduleServer, observeEvent, observe,
    reactiveValues, reactiveValuesToList,
    fluidPage, tagList, h2, div, markdown, p,
    uiOutput, renderUI, HTML,
    actionButton
  ]
)

#' Missing description
#' @export

module_home_ui <- function(id = "home", label = "m_home") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      markdown(readLines("md/willkommen.md")),
      div(
        style = "display: flex; flex-direction: row;",

      )
    )
  )
}

#' Missing description
#' @export

module_home_server <- function(id = "home") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns


    }
  )
}

