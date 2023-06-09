#' Missing description
#' @noRd

draw_ui <- function(){
  tagList(
    #tags$style("navbar-header,.navbar-header{margin-right: 30%!important;}"),
    #HTML("<style>.container-fluid{padding:0;margin:0;}</style>"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    navigation_bar,
    title = NULL,
    router_ui(
      route("/",           module_home_ui()),
      route("stories",     module_stories_ui()),
      route("monitor",     module_monitor_ui()),
      route("explorer",    module_explorer_ui()),
      route("studies",     module_studies_ui()),
      route("handlung_1",  module_handlung_1_ui()),
      route("handlung_2",  module_handlung_2_ui())
    ),
    draw_footer()
  )
}
#.nav>li:has(a[data-value="Handlung 1"])
#' Missing description
#' @noRd

navigation_bar <-
  navbarPage(
    title = NULL,
    windowTitle = "SV dataVerse",
    id    = "navigation_bar",
    selected = "SV Data",
    tabPanel("Handlung 1", icon = icon("graduation-cap")),
    tabPanel("Handlung 2", icon = icon("lightbulb") ),
    tabPanel("SV Data",    icon = icon("chart-simple")),
    tabPanel("Stories",    icon = icon("newspaper")),
    tabPanel("Monitor",    icon = icon("chart-pie")),
    tabPanel("Explorer",   icon = icon("magnifying-glass")),
    tabPanel("Studies",    icon = icon("square-poll-vertical")),
    fluid       = TRUE,
    collapsible = TRUE
  )

#' Missing description
#' @noRd

draw_bootstrap <- function(){
  bootstrap <- bslib::bs_theme(version = 5)
  return(bootstrap)
}

#' Missing description
#' @noRd

draw_footer <- function() {
  footer <-
    HTML('<footer class="main-footer" data-fixed="false"></footer>')
  return(footer)
}
