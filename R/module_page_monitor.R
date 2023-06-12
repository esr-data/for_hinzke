#' Missing description
#' @noRd

module_monitor_ui <- function(id = "monitor", label = "m_monitor", type = "all") {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel(paste("Monitor", type, "!")),
      navlistPanel(
        "Header A",
        tabPanel("Component 1"),
        tabPanel("Component 2"),
        "Header B",
        tabPanel("Component 3"),
        tabPanel("Component 4"),
        "-----",
        tabPanel("Component 5")
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

    }
  )
}
