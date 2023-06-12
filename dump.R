# router_server()
#
# observeEvent(input$navigation_bar, {
#   page <- isolate(input$navigation_bar)
#
#   if (page == "SV Data")  change_page("")
#   if (page == "Stories")  change_page("stories")
#   if (page == "Explorer") change_page("explorer")
#   if (page == "Studies")  change_page("studies")
#   if (page == "Monitor")  change_page("monitor")
#
# })

# navigation_bar <-
#   navbarPage(
#     title = "SV dataVerse",
#     id    = "navigation_bar",
#     tabPanel("SV Data"),
#     tabPanel("Stories"),
#     tabPanel("Monitor"),
#     tabPanel("Explorer"),
#     tabPanel("Studies"),
#     # footer      = HTML(
#     #   '<footer class="footer">
#     #     Copyright &copy; 2021-2022 <a href="http://mycompanyurl.com">MY COMPANY NAME</a>
#     #     </footer>'
#     # ),
#     fluid       = TRUE,
#     collapsible = TRUE
#   )

# ui <-
  # tagList(
  #   tags$style("navbar-header,.navbar-header{margin-right: 30%!important;}"),
  #   #HTML("<style>.container-fluid{padding:0;margin:0;}</style>"),
  #   navigation_bar,
  #   title = NULL,
  #   router_ui(
  #     route("/",        module_home_ui()),
  #     route("stories",  module_stories_ui()),
  #     route("monitor",  module_monitor_ui()),
  #     route("explorer", module_explorer_ui()),
  #     route("studies",  module_studies_ui())
  #   ),
  #   createFooter()
  # )
#
# draw_ui <- function(){
#   tagList(
#     #tags$style("navbar-header,.navbar-header{margin-right: 30%!important;}"),
#     tags$style('a[data-value="SV Dataverse"] {margin-right: 35vw!important;};'),
#     navbarPage(
#       title       = NULL,
#       windowTitle = "SV Dataverse",
#       id          = "navbar_page",
#       collapsible = TRUE,
#       theme       = draw_bootstrap(),
#       tabPanel(title = "SV Dataverse", module_home_ui()),
#       tabPanel(title = "Stories",      module_stories_ui()),
#       tabPanel(title = "Monitor",      module_monitor_ui()),
#       tabPanel(title = "Explorer",     module_explorer_ui()),
#       tabPanel(title = "Studies",      module_studies_ui()),
#       footer = draw_footer()
#     )
#   )
# }
# observeEvent(input$navbar_page, {
#   page <- isolate(input$navbar_page)
#   new_url <- paste0(
#     session$clientData$url_protocol, "//",
#     session$clientData$url_hostname, ":",
#     session$clientData$url_port,
#     session$clientData$url_pathname, "#",
#     translate_navbar(page)
#   )
#   updateQueryString(new_url, mode = "replace", session)
# })
#
# observe({
#   page <- sub("#", "", session$clientData$url_hash)
#   if(!is.null(page)){
#     updateNavbarPage(session, "navbar_page", selected = translate_navbar(page))
#   }
# })

#' Missing description
#' @noRd
#
# translate_navbar <- function(x){
#   dict <-
#     rbind(
#       cbind("home",     "SV Dataverse"),
#       cbind("stories",  "Stories"),
#       cbind("explorer", "Explorer"),
#       cbind("studies",  "Studies"),
#       cbind("monitor",  "Monitor")
#     )
#   if (x %in% dict[,1]){
#     return(dict[match(x, dict[,1]), 2])
#   } else {
#     return(dict[match(x, dict[,2]), 1])
#   }
# }

# observeEvent(input$navigation_bar, {
#   page <- isolate(input$navigation_bar)
#   if (page == "SV Data")     change_page("")
#   if (page == "Stories")     change_page("stories")
#   if (page == "Explorer")    change_page("explorer")
#   if (page == "Studies")     change_page("studies")
#   if (page == "Monitor")     change_page("monitor")
#   if (page == "Handlung 1")  change_page("handlung_1")
#   if (page == "Handlung 2")  change_page("handlung_2")
# })

#' Missing description
#' @noRd

server <- function(input, output, session) {

  sidebar_button <- reactiveValues(value_1 = "", value_2 = "", locked = FALSE)

  # output$sidebar_dynamic <- renderUI({draw_sidebar_home()})

  observeEvent(input$sb_handlung1, {
    sidebar_button$value_1 <- update_button_sidebar(session, "handlung1", isolate(sidebar_button$value_1), c("handlung1", "handlung2"))
  })

  observeEvent(input$sb_handlung2, {
    sidebar_button$value_1 <- update_button_sidebar(session, "handlung2", isolate(sidebar_button$value_1), c("handlung1", "handlung2"))
  })

  observeEvent(input$sb_stories, {
    sidebar_button$value_2 <- update_button_sidebar(session, "stories", isolate(sidebar_button$value_2))
  })

  observeEvent(input$sb_monitor, {
    sidebar_button$value_2 <- update_button_sidebar(session, "monitor", isolate(sidebar_button$value_2))
  })

  observeEvent(input$sb_explorer, {
    sidebar_button$value_2 <- update_button_sidebar(session, "explorer", isolate(sidebar_button$value_2))
  })

  observeEvent(input$sb_studies, {
    sidebar_button$value_2 <- update_button_sidebar(session, "studies", isolate(sidebar_button$value_2))
  })

  observeEvent(sidebar_button$value_2, {
    value_2 <- isolate(sidebar_button$value_2)
    if (value_2 == "")         output$sidebar_dynamic <- renderUI({draw_sidebar_home()})
    if (value_2 == "stories")  output$sidebar_dynamic <- renderUI({draw_sidebar_stories()})
    if (value_2 == "studies")  output$sidebar_dynamic <- renderUI({draw_sidebar_studies()})
    if (value_2 == "monitor")  output$sidebar_dynamic <- renderUI({draw_sidebar_monitor()})
    if (value_2 == "explorer") output$sidebar_dynamic <- renderUI({draw_sidebar_explorer()})
  })

  # --- Routing ------------------------------------------------------------------------------------

  router_server()

  observe({
    value_1 <- isolate(sidebar_button$value_1)
    value_2 <- isolate(sidebar_button$value_2)
    sidebar_button$locked <- TRUE

    for (i in c("handlung1", "handlung2", "studies", "stories", "monitor", "explorer")){
      if (i %in% c(value_1, value_2)){
        print(i)
        updateButton(session, inputId = paste0("sb_", i),  style = "danger")
      } else {
        updateButton(session, inputId = paste0("sb_", i),  style = "default")
      }
    }

    change_url(isolate(value_1), isolate(value_2))
    sidebar_button$locked <- FALSE
  })

  observe({
    print(1)
    url <- session$clientData$url_hash
    if (!isolate(sidebar_button$locked)){
      url <- isolate(url)
      if (grepl("handlung1", url)) sidebar_button$value_1 <- "handlung1"
      if (grepl("handlung2", url)) sidebar_button$value_1 <- "handlung2"
      if (grepl("stories",   url)) sidebar_button$value_2 <- "stories"
      if (grepl("studies",   url)) sidebar_button$value_2 <- "studies"
      if (grepl("monitor",   url)) sidebar_button$value_2 <- "monitor"
      if (grepl("explorer",  url)) sidebar_button$value_2 <- "explorer"
    }
  })

}

#' Missing description
#' @noRd

update_button_sidebar <- function(session, button, actual_value, button_set = c("studies", "monitor", "explorer", "stories")){
  id <- paste0("sb_", button)
  if (actual_value == button){
    #updateButton(session, inputId = id,  style = "default")
    actual_value <- ""
  } else {
    #updateButton(session, inputId = id,  style = "warning")
    actual_value <- button
  }
  # for (i in button_set){
  #   if (button != i)  updateButton(session, inputId = paste0("sb_", i),  style = "default")
  # }
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
