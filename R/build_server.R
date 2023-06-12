#' Missing description
#' @noRd

server <- function(input, output, session) {

  sidebar_button <- reactiveValues(value_1 = "", value_2 = "", locked = FALSE)

  # --- Sidebar ------------------------------------------------------------------------------------

  observeEvent(input$sb_handlung1, {sidebar_button$value_1 <- update_button_sidebar("handlung1", isolate(sidebar_button$value_1))})
  observeEvent(input$sb_handlung2, {sidebar_button$value_1 <- update_button_sidebar("handlung2", isolate(sidebar_button$value_1))})
  observeEvent(input$sb_stories,   {sidebar_button$value_2 <- update_button_sidebar("stories",   isolate(sidebar_button$value_2))})
  observeEvent(input$sb_monitor,   {sidebar_button$value_2 <- update_button_sidebar("monitor",   isolate(sidebar_button$value_2))})
  observeEvent(input$sb_explorer,  {sidebar_button$value_2 <- update_button_sidebar("explorer",  isolate(sidebar_button$value_2))})
  observeEvent(input$sb_studies,   {sidebar_button$value_2 <- update_button_sidebar("studies",   isolate(sidebar_button$value_2))})

  observeEvent(sidebar_button$value_2, {
    value_2 <- isolate(sidebar_button$value_2)
    if (value_2 == "")         output$sidebar_dynamic <- renderUI({draw_sidebar_home()})
    if (value_2 == "stories")  output$sidebar_dynamic <- renderUI({draw_sidebar_stories()})
    if (value_2 == "studies")  output$sidebar_dynamic <- renderUI({draw_sidebar_studies()})
    if (value_2 == "monitor")  output$sidebar_dynamic <- renderUI({draw_sidebar_monitor()})
    if (value_2 == "explorer") output$sidebar_dynamic <- renderUI({draw_sidebar_explorer()})
  })

  observeEvent(session$clientData$url_hash, {
    url <- isolate(session$clientData$url_hash)
    if (!isolate(sidebar_button$locked)){
      if (grepl("handlung1", url)) sidebar_button$value_1 <- "handlung1"
      if (grepl("handlung2", url)) sidebar_button$value_1 <- "handlung2"
      if (grepl("stories",   url)) sidebar_button$value_2 <- "stories"
      if (grepl("studies",   url)) sidebar_button$value_2 <- "studies"
      if (grepl("monitor",   url)) sidebar_button$value_2 <- "monitor"
      if (grepl("explorer",  url)) sidebar_button$value_2 <- "explorer"
    }
  })

  # --- Routing ------------------------------------------------------------------------------------

  router_server()

  observe({
    value_1 <- sidebar_button$value_1
    value_2 <- sidebar_button$value_2
    sidebar_button$locked <- TRUE

    value_1 <- isolate(value_1)
    value_2 <- isolate(value_2)

    for (i in c("handlung1", "handlung2", "studies", "stories", "monitor", "explorer")){
      if (i %in% c(value_1, value_2)){
        updateButton(session, inputId = paste0("sb_", i),  style = "warning")
      } else {
        updateButton(session, inputId = paste0("sb_", i),  style = "default")
      }
    }

    sidebar_button$locked <- FALSE
    change_url(isolate(value_1), isolate(value_2))
  })

}

#' Missing description
#' @noRd

update_button_sidebar <- function(button, actual_value){
  if (actual_value == button){
    actual_value <- ""
  } else {
    actual_value <- button
  }
  return(actual_value)
}

#' Missing description
#' @noRd

change_url <- function(value_1, value_2){
  values <- c(value_1, value_2)
  values <- values[values != ""]
  if (length(values) == 0){
    change_page("")
  } else {
    change_page(paste(values, collapse = "_"))
  }
  return(invisible())
}
