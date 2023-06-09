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
