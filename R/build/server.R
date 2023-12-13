
#' Necessary Packages/Functions

box::use(
  ../../R/pages/explorer[module_explorer_server],
  ../../R/pages/indikator[module_indikator_server],
  ../../R/pages/search[module_search_server],
  ../../R/pages/home[module_home_server],
  ../../R/pages/monitor[module_monitor_server],
  ../../R/build/sidebar[
    draw_sidebar_home,
    draw_sidebar_stories,
    draw_sidebar_studies,
    draw_sidebar_monitor,
    draw_sidebar_explorer
  ],
  ../../R/utils/tutorial[plan_tutorial_tour],
  shiny[
    observeEvent, observe,
    isolate,
    reactiveValues,
    renderUI
  ],
  shinyjs[removeCssClass, addCssClass],
  shiny.router[change_page, router_server],
  DBI[dbConnect],
  RSQLite[SQLite]
)


# Global Variables

con   <- dbConnect(SQLite(), "data/magpie.sqlite")
guide <- plan_tutorial_tour()

#' Missing description
#' @export

server <- function(input, output, session) {

  # onStop(function() {dbDisconnect(con)})

  sidebar_button    <- reactiveValues(value_1 = "", value_2 = "", locked = FALSE, start = TRUE)
  sidebar_minimized <- reactiveValues(status = FALSE)

  # --- Sidebar ------------------------------------------------------------------------------------

  observeEvent(
    input$button_minimize, {
      sidebar_minimized$status <- !sidebar_minimized$status
      if (sidebar_minimized$status){
        addCssClass("sidebar",    "sidebar_minimized")
      } else {
        removeCssClass("sidebar", "sidebar_minimized")
      }
    }
  )

  observeEvent(input$sb_stories,   {sidebar_button$value_2 <- update_button_sidebar("stories",   isolate(sidebar_button$value_2))})
  observeEvent(input$sb_monitor,   {sidebar_button$value_2 <- update_button_sidebar("monitor",   isolate(sidebar_button$value_2))})
  observeEvent(input$sb_explorer,  {sidebar_button$value_2 <- update_button_sidebar("explorer",  isolate(sidebar_button$value_2))})
  observeEvent(input$sb_studies,   {sidebar_button$value_2 <- update_button_sidebar("studies",   isolate(sidebar_button$value_2))})

  observeEvent(
    input$sb_handlung1, {
      sidebar_button$value_1 <- update_button_sidebar("handlung1", isolate(sidebar_button$value_1))
      if (!(sidebar_button$value_2 %in% c("stories", "monitor", "explorer", "studies"))){
        sidebar_button$value_2 <- ""
      }
    }
  )

  observeEvent(
    input$sb_handlung2, {
      sidebar_button$value_1 <- update_button_sidebar("handlung2", isolate(sidebar_button$value_1))
      if (!(sidebar_button$value_2 %in% c("stories", "monitor", "explorer", "studies"))){
        sidebar_button$value_2 <- ""
      }
    }
  )


  observeEvent(
    input$sb_impressum, {
      sidebar_button$value_1 <- ""
      sidebar_button$value_2 <- update_button_sidebar("impressum", isolate(sidebar_button$value_2))
    }
  )

  observeEvent(
    input$sb_datenschutz, {
      sidebar_button$value_1 <- ""
      sidebar_button$value_2 <- update_button_sidebar("datenschutz", isolate(sidebar_button$value_2))
    }
  )

  observeEvent(
    input$sb_team, {
      sidebar_button$value_1 <- ""
      sidebar_button$value_2 <- update_button_sidebar("team", isolate(sidebar_button$value_2))
    }
  )

  observeEvent(sidebar_button$value_2, {
    value_2 <- isolate(sidebar_button$value_2)
    if (value_2 == "stories"){
      output$sidebar_dynamic <- renderUI({draw_sidebar_stories()})
    } else if (value_2 == "studies"){
      output$sidebar_dynamic <- renderUI({draw_sidebar_studies()})
    } else if (value_2 == "monitor"){
      output$sidebar_dynamic <- renderUI({draw_sidebar_monitor()})
    } else if (value_2 == "explorer"){
      output$sidebar_dynamic <- renderUI({draw_sidebar_explorer()})
    } else {
      output$sidebar_dynamic <- renderUI({draw_sidebar_home()})
    }
  })

  observeEvent(session$clientData$url_hash, {
    url <- isolate(session$clientData$url_hash)
    if (!isolate(sidebar_button$locked)){
      if (grepl("handlung1",   url)) sidebar_button$value_1 <- "handlung1"
      if (grepl("handlung2",   url)) sidebar_button$value_1 <- "handlung2"
      if (grepl("stories",     url)) sidebar_button$value_2 <- "stories"
      if (grepl("studies",     url)) sidebar_button$value_2 <- "studies"
      if (grepl("monitor",     url)) sidebar_button$value_2 <- "monitor"
      if (grepl("explorer",    url)) sidebar_button$value_2 <- "explorer"
      if (grepl("impressum",   url)) sidebar_button$value_2 <- "impressum"
      if (grepl("datenschutz", url)) sidebar_button$value_2 <- "datenschutz"
      if (grepl("team",        url)) sidebar_button$value_2 <- "team"
    }
  })


  # --- Tutorial -----------------------------------------------------------------------------------

  observeEvent(
    input$sb_help, {
      guide$init()$start()
    }
  )

  # --- Routing ------------------------------------------------------------------------------------

  router_server()

  observe({
    value_1 <- sidebar_button$value_1
    value_2 <- sidebar_button$value_2
    sidebar_button$locked <- TRUE

    value_1 <- isolate(value_1)
    value_2 <- isolate(value_2)

    for (i in c(
      "handlung1", "handlung2",
      "studies",   "stories",     "monitor", "explorer",
      "impressum", "datenschutz", "team"
    )){
      if (i %in% c(value_1, value_2)){
        addCssClass(paste0("sb_", i), "btn-warning")
      } else {
        removeCssClass(paste0("sb_", i), "btn-warning")
      }
    }

    sidebar_button$locked <- FALSE
    if (!isolate(sidebar_button$start)){
      change_url(isolate(value_1), isolate(value_2))
    } else {
      sidebar_button$start <- FALSE
    }
  })

  observeEvent(input$sb_home, {
    sidebar_button$value_1 <- ""
    sidebar_button$value_2 <- ""
  })

  # --- Server der Shiny-Modules -------------------------------------------------------------------

  module_explorer_server()
  module_monitor_server()
  module_indikator_server(con = con)
  module_search_server(con = con)
  module_home_server(con = con)

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

