#' Necessary Packages/Functions

box::use(
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, tagList, titlePanel,
    div, tags, HTML,
    uiOutput, renderUI,
    verbatimTextOutput,
    reactive, observe,
    renderPrint, req
  ],
  shiny.router[change_page],
)

#' Missing description
#' @export

module_monitor_ui <- function(id = "monitor", label = "m_monitor", type = "all") {
  ns <- NS(id)
  tagList(
    fluidPage(
      div(
        class = "panel-content",
        titlePanel(paste("Monitor", "!")),
        div(
          id = ns("mon"),
          uiOutput(ns("monitor_svg")),
          # verbatimTextOutput(ns("debug"))
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

  )
}

#' Missing description
#' @export

module_monitor_server <- function(id = "monitor", con, type = "all", mon_value) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$monitor_svg <- renderUI({HTML(readLines("www/img/Test_Monitor.svg"))})

      observeEvent(
        input$mon, {
          if (!is.null(input$mon)){
            mon_value(input$mon)
            # print(paste0("monitor_bildung_inhalt?tp=", gsub("circle_", "", input$mon)))
            change_page(paste0("monitor_bildung_inhalt?tp=", gsub("circle_", "", input$mon)))
            # output$debug <- renderPrint(sub("circle_", "", input$mon))
          }
        }
      )

    }
  )
}
