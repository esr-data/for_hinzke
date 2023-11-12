#' Missing description
#' @noRd

module_monitor_ui <- function(id = "monitor", label = "m_monitor", type = "all") {
  ns <- NS(id)
  tagList(
    fluidPage(
      shinyjs::useShinyjs(),
      titlePanel(paste("Monitor", "!")),
      div(
        id = ns("mon"),
        uiOutput(ns("monitor_svg")),
        verbatimTextOutput(ns("debug"))
      ),
      tags$script(
        HTML(
          "
            shinyjs.init = function() {
              $('body').on('click', '.side_circle', function(ev) {
                Shiny.setInputValue('monitor-mon', ev.target.id, {priority: 'event'});
              });
            };
          "
        )
      )
    )
  )
}

#' Missing description
#' @noRd

module_monitor_server <- function(id = "monitor", con, type = "all") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # simple linking?

      # observeEvent(input$mon, {
      #   shinyjs::runjs(
      #     sprintf(
      #       "window.location.href = 'R/pages/subpages_monitor/%s';",
      #       input$mon
      #     )
      #   )
      # })

      output$debug <- renderPrint(input$mon)

      output$monitor_svg <- renderUI({
        HTML(
          readLines("www/img/Test_Monitor.svg")
        )
      })
    }
  )
}
