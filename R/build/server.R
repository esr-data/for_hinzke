
#' Necessary Packages/Functions

box::use(
  ../../R/pages/explorer[module_explorer_server, report_explorer_subpages],
  ../../R/pages/indikator[module_indikator_server],
  ../../R/pages/suchen[module_suchen_server],
  ../../R/pages/home[module_home_server],
  ../../R/pages/monitor[module_monitor_server],
  ../../R/pages/studies[module_studies_server],
  ../../R/pages/stories[module_stories_server],
  ../../R/pages/monitor_inhalt[module_monitor_inhalt_server],
  ../../R/build/sidebar[
    draw_sidebar_home,
    draw_sidebar_stories,
    draw_sidebar_studies,
    draw_sidebar_monitor,
    draw_sidebar_explorer
  ],
  ../../R/utils/tutorial[plan_tutorial_tour],
  ../../R/utils/routing[get_hf_param],
  shiny[
    observeEvent, observe,
    isolate,
    reactiveValues,
    reactiveVal,
    reactive,
    renderUI
  ],
  shinyjs[
    removeCssClass,
    addCssClass
  ],
  shiny.router[
    change_page,
    router_server,
    get_page,
    get_query_param
  ]
)

# Global Variables
print_events <-FALSE
guide <- plan_tutorial_tour()
explorer_subpages <- report_explorer_subpages()

#' Missing description
#' @export

server <- function(input, output, session) {

  # --- Sidebar ------------------------------------------------------------------------------------

  sidebar <-
    reactiveValues(
      button    = list(value_1 = "", value_2 = ""),
      start     = TRUE,
      minimized = FALSE
    )

  current <-
    reactiveValues(
      page = "start",
      sidebar = "start"
    )

  # Button-Events

  observeEvent(input$button_minimize, {
    sidebar$minimized <- !sidebar$minimized
    if (sidebar$minimized){
      addCssClass("sidebar", "sidebar_minimized")
    } else {
      removeCssClass("sidebar", "sidebar_minimized")
    }
  })

  observeEvent(input$sb_stories, {
    sidebar$button$value_2 <- update_button_sidebar("stories", sidebar$button$value_2)
  })

  observeEvent(input$sb_monitor, {
    sidebar$button$value_2 <- update_button_sidebar("monitor", sidebar$button$value_2)
  })

  observeEvent(input$sb_explorer, {
    sidebar$button$value_2 <- update_button_sidebar("explorer", sidebar$button$value_2)
  })

  observeEvent(input$sb_studies, {
    sidebar$button$value_2 <- update_button_sidebar("studies", sidebar$button$value_2)
  })

  observeEvent(input$sb_handlung1, {
    sidebar$button$value_1 <- update_button_sidebar("handlung1", sidebar$button$value_1)
    if (!(sidebar$button$value_2 %in% c("stories", "monitor", "explorer", "studies"))){
      sidebar$button$value_2 <- ""
    }
  })

  observeEvent(input$sb_handlung2, {
    sidebar$button$value_1 <- update_button_sidebar("handlung2", sidebar$button$value_1)
    if (!(sidebar$button$value_2 %in% c("stories", "monitor", "explorer", "studies"))){
      sidebar$button$value_2 <- ""
    }
  })

  observeEvent(input$sb_impressum, {
    sidebar$button$value_1 <- ""
    sidebar$button$value_2 <- update_button_sidebar("impressum", isolate(sidebar$button$value_2))
  })

  observeEvent(input$sb_datenschutz, {
    sidebar$button$value_1 <- ""
    sidebar$button$value_2 <- update_button_sidebar("datenschutz", isolate(sidebar$button$value_2))
  })

  observeEvent(input$sb_team, {
    sidebar$button$value_1 <- ""
    sidebar$button$value_2 <- update_button_sidebar("team", isolate(sidebar$button$value_2))
  })

  observeEvent(input$sb_home, {
    sidebar$button$value_1 <- ""
    sidebar$button$value_2 <- ""
  })

  observeEvent(input$sbd_explorer, {
    change_page_by_subpage_button("explorer")
  })

  observeEvent(input$sbd_explorer_suche, {
    change_page_by_subpage_button("suchen")
  })

  observeEvent(input$sbd_explorer_indikator, {
    change_page_by_subpage_button("indikator")
  })

  observeEvent(input$sbd_explorer_vergleich, {
    change_page_by_subpage_button("vergleichen")
  })

  observeEvent(input$sbd_explorer_datensatz, {
    change_page_by_subpage_button("datensaetze")
  })

  observeEvent(input$sbd_explorer_karten, {
    change_page_by_subpage_button("karten")
  })

  # Observe URL

  observeEvent(
    session$clientData$url_hash, {
      url <- session$clientData$url_hash
      if (!(url %in% c("", "#!/"))){
        if (print_events) message("Event: New URL to be checked")

        values <- get_button_values_from_url(url)

        to_change <- FALSE

        if (values[1] != sidebar$button$value_1){
          sidebar$button$value_1 <- values[1]
          to_change <- TRUE
        }

        if (values[2] != sidebar$button$value_2){
          sidebar$button$value_2 <- values[2]
          to_change <- TRUE
        }

        if (to_change){
          adjust_sidebar_button_classes(sidebar$button$value_1, sidebar$button$value_2)
        }
      }
    }
  )

  # values in sidebar$button

  observeEvent(
    sidebar$button, {

      if (print_events) message("Event: Update URL")

      value_1 <- sidebar$button$value_1
      value_2 <- sidebar$button$value_2

      if (!sidebar$start){
        if (print_events) message("Event: Update URL - url changed")
        change_url_by_button_values(value_1, value_2, current_url = session$clientData$url_hash)
        adjust_sidebar_button_classes(value_1, value_2)
      } else {
        sidebar$start <- FALSE
      }
    }
  )

  observeEvent(
    get_page(),{
      page <- get_page()
      if (page != current$page) current$page <- page
    }
  )

  observeEvent(
    current$page, {
      if (sidebar$button$value_2 != current$sidebar){
        value_2 <- sidebar$button$value_2
        current$sidebar <- value_2
        output$sidebar_dynamic <- renderUI({update_subpage_sidebar(value_2, current$page)})
      } else {
        update_sidebar_subpages_classes(current$page)
      }
    }
  )

  # --- Tutorial -----------------------------------------------------------------------------------

  observeEvent(
    input$sb_help, {
      guide$init()$start()
    }
  )

  # --- Routing ------------------------------------------------------------------------------------

  router_server()

  # --- Server der Shiny-Modules -------------------------------------------------------------------

  module_explorer_server()
  module_indikator_server()
  module_suchen_server()
  module_home_server()
  module_studies_server()
  module_stories_server()
  module_monitor_server()
  module_monitor_inhalt_server()
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

change_url_by_button_values <- function(value_1, value_2, current_url){

  page <-
    current_url |>
    gsub(pattern = "#!/", replacement = "", fixed = TRUE) |>
    strsplit("?", fixed = TRUE)
  page <- (page[[1]])[1]

  param_hf <- get_hf_param(current_url)
  explorer <- explorer_subpages

  #TODO DEBUGING - SPÄTER LOESCHEN
  # current_url <<- current_url
  # page     <<- page
  # param_hf <<- param_hf
  # explorer <<- explorer
  # value_1  <<- value_1
  # value_2  <<- value_2

  if (value_2 == "explorer" & page %in% explorer$url) {
    value_2 <- page
  } else if (
    value_2 == "monitor" &
    page == "monitor_inhalt" &
    value_1 %in% c("handlung1", "handlung2") &
    (
      (grepl("tp=bildung",    current_url) & value_1 == "handlung1") |
      (grepl("tp=innovation", current_url) & value_1 == "handlung2")
    )
  ){
    value_2 <- page
  }

  url <- ""
  to_change <- TRUE

  if (value_1 == "" & value_2 == ""){
    # Startseite:
    if (page %in% c("", "/")) to_change <- FALSE
    url <- ""

  } else if (value_1 == "" & value_2 != ""){
    # Nur Format
    if (param_hf %in% 0 & page %in% value_2){
      to_change <- FALSE
    }
    url <- paste0(value_2, "?hf=0")

  } else if (value_1 != "" & value_2 != ""){
    # Format und Handlungsfeld
    if (
      (param_hf %in% gsub("handlung", "", value_1) |
       (param_hf == 0 & value_1 == "")) &
      page %in% value_2
    ){
      to_change <- FALSE
    }
    url <- paste0(value_2, gsub("handlung", "?hf=", value_1))

  } else if (value_1 != "" & value_2 == ""){
    # Nur Handlungsfeld
    if (page %in% value_1) to_change <- FALSE
    url <- value_1
  }

  if (to_change){
    if (print_events) message(paste("change_url_by_button_values:", url))
    change_page(url)
  }

  return(invisible())
}

#' Missing description
#' @noRd

change_page_by_subpage_button <- function(url){
  url <- paste0(url, "?hf=", get_hf_param())
  if (print_events) message(paste("change_page_by_subpage_button:", url))
  change_page(url)
}


#' Missing description
#' @noRd

adjust_sidebar_button_classes <- function(value_1, value_2){

  for (i in c(
    "handlung1", "handlung2",
    "studies",   "stories",
    "monitor",   "explorer",
    "impressum", "datenschutz",
    "team"
  )){
    if (i %in% c(value_1, value_2)){
      addCssClass(paste0("sb_", i), "btn-warning")
    } else {
      removeCssClass(paste0("sb_", i), "btn-warning")
    }
  }

  return(invisible(NULL))
}

#' Missing description
#' @noRd

get_button_values_from_url <- function(url){

  url <- gsub("#!/", "", url)
  url <- strsplit(url, "?", fixed = TRUE)[[1]]

  value_1 <- ""
  if (length(url) > 1){
    url_param <- strsplit(url[2], "&")[[1]]
    if (any(url_param %in% paste0("hf=", 1:2))){
      url_param <- substr(url_param[match("hf", substr(url_param, 1, 2))], 4, 4)
      value_1 <- paste0("handlung", url_param)
    }
    rm(url_param)
  }

  value_2 <- url[1]
  if (length(value_2) == 0){
    value_2 <- ""
  }

  if (value_2 %in% explorer_subpages$url){
    value_2 <- "explorer"
  }

  if (value_2 %in% c("monitor", "monitor_inhalt")){
    value_2 <- "monitor"
  }

  return(c(value_1, value_2))
}

#' Missing description
#' @noRd

update_sidebar_subpages_classes <- function(url){

  url      <- gsub("#!/", "", url)
  explorer <- explorer_subpages

  if (url %in% explorer$url){
    for (i in explorer$url){
      id <- explorer$id[match(i, explorer$url)]
      if (i == url){
        addCssClass(id,    "btn_selected")
      } else {
        removeCssClass(id, "btn_selected")
      }
      rm(id)
    }
    rm(i)
  }

  return(invisible(NULL))
}

#' Missing description
#' @noRd

update_subpage_sidebar <- function(button_value_2, url){

  if (print_events) message("Event: Update Sidebar")

  if (button_value_2 == "stories"){
    return(draw_sidebar_stories())
  } else if (button_value_2 == "studies"){
    return(draw_sidebar_studies())
  } else if (button_value_2 == "monitor"){
    return(draw_sidebar_monitor())
  } else if (button_value_2 == "explorer"){
    return(draw_sidebar_explorer(url))
  }

  return(draw_sidebar_home())
}
