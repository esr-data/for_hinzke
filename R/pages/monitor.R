module_monitor_ui <- function(id = "monitor", label = "m_monitor", type = "all") {
  ns <- NS(id)
  tagList(
    fluidPage(
      shinyjs::useShinyjs(),
      tags$script(
        HTML(
          paste0(
            "document.addEventListener('click', function(e) {
              if (e.target.closest('svg')) {
                var elemId = e.target.id;
                var elemClass = e.target.className.baseVal;

                console.log('Geklickte Element-ID:', elemId);
                console.log('Geklickte Element-Klasse:', elemClass);

                if (typeof Shiny !== 'undefined') {
                  Shiny.setInputValue('", ns("clickedElementId"), "', elemId, {priority: 'event'});
                  Shiny.setInputValue('", ns("clickedElementClass"), "', elemClass, {priority: 'event'});
                }
              }
            });"
          )
        )
      ),
      titlePanel(paste("Monitor", "!")),
      HTML(
        readLines("www/img/Test_Monitor.svg")
      )
    )
  )
}

module_monitor_server <- function(id = "monitor", con, type = "all") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observeEvent(input[[ns('clickedElementId')]], {
        print(paste("Geklickte Element-ID:", input[[ns('clickedElementId')]]))
      })

      observeEvent(input[[ns('clickedElementClass')]], {
        print(paste("Geklickte Element-Klasse:", input[[ns('clickedElementClass')]]))
      })

    }
  )
}
