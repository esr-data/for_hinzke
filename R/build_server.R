#' Missing description
#' @noRd

server <- function(input, output, session) {

  # --- Routing ------------------------------------------------------------------------------------

  router_server()

  observeEvent(input$navigation_bar, {
    page <- isolate(input$navigation_bar)
    if (page == "SV Data")     change_page("")
    if (page == "Stories")     change_page("stories")
    if (page == "Explorer")    change_page("explorer")
    if (page == "Studies")     change_page("studies")
    if (page == "Monitor")     change_page("monitor")
    if (page == "Handlung 1")  change_page("handlung_1")
    if (page == "Handlung 2")  change_page("handlung_2")
  })

}


