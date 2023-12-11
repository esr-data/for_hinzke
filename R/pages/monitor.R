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

      subject <- reactive({
        sub("circle_", "", input$mon)
      })

      output$debug <- renderPrint(input$mon)


      observe({
        # Verwende isTruthy(), um zu überprüfen, ob input$mon existiert und einen Wert hat
        if (isTruthy(input$mon)) {
          # Wenn input$mon eine bestimmte ID hat, rufe das gewünschte Modul auf
          if (input$mon == "circle_bildung_ganztag") {
            output$monitor_svg <- renderUI({
              # Hier wird das Modul aufgerufen
              module_monitor_subject_ui("monitor_ganztag_als_bildungszeit")
            })
          } else {
            # Andernfalls zeige die SVG
            output$monitor_svg <- renderUI({
              HTML(readLines("www/img/Test_Monitor.svg"))
            })
          }
        }
      })




    }
  )
}
