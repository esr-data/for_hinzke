#' Necessary Packages/Functions

box::use(
  ../../R/utils/js[get_js],
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, div, HTML, h2,
    uiOutput, renderUI
  ],
  shiny.router[change_page],
)

#' Missing description
#' @export

module_monitor_ui <- function(id = "monitor", label = "m_monitor") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      h2(paste("Monitor", "!")),
      div(
        id = ns("mon"),
        uiOutput(ns("monitor_svg"))
      ),
      get_js("monitor_svg_click")
    )
  )
}

#' Missing description
#' @export

module_monitor_server <- function(id = "monitor", con) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$monitor_svg <- renderUI({HTML(readLines("www/img/Test_Monitor.svg"))})
      observeEvent(
        input$mon, {
          new_url <- paste0("monitor_inhalt?hf=1&ind=1&tp=", gsub("circle_", "", input$mon))
          change_page(new_url)
        }
      )
    }
  )
}
