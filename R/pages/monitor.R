#' Necessary Packages/Functions

box::use(
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, tagList, titlePanel,
    div, tags, HTML,
    uiOutput, renderUI,
    renderPrint, req
  ],
  shiny.router[change_page],
)

#' Missing description
#' @export

module_monitor_ui <- function(id = "monitor", label = "m_monitor") {
  ns <- NS(id)
  tagList(
    fluidPage(
      div(
        class = "panel-content",
        titlePanel(paste("Monitor", "!")),
        div(
          id = ns("mon"),
          uiOutput(ns("monitor_svg"))
        ),
        tags$script(
          HTML(paste(readLines("js/monitor_svg_click.js"), collapse = " "))
        )
      )
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
          new_url <- paste0("monitor_bildung?hf=1&tp=", gsub("circle_", "", input$mon))
          change_page(new_url)
        }
      )
    }
  )
}
