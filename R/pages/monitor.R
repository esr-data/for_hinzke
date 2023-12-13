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

      output$monitor_svg <- renderUI({
        HTML(readLines("www/img/Test_Monitor.svg"))
      })

      subject <- reactive({
        req(input$mon)
        sub_id <- sub("circle_", "", input$mon)
        if (sub_id %in% names(content_list_monitor_subpages_structure_full)) {
          content_list_monitor_subpages_structure_full[[sub_id]]
        } else {
          NULL
        }
      })

      observe({
        if (!is.null(subject())) {
          content_list_monitor_subpage_structure <<- subject()
          change_page("#!/handlung1_monitor_subpage")
        }
      })

      output$debug <- renderPrint(sub("circle_", "", input$mon))

    }
  )
}
