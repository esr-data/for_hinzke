
#' Necessary Packages/Functions

box::use(
  ../../R/pages/explorer[module_explorer_server, report_explorer_subpages],
 # ../../R/pages/indikator[module_indikator_server],
 # ../../R/pages/vergleichen[module_vergleichen_server],
  ../../R/pages/analysetool[module_analysetool_server],
  ../../R/pages/suchen[module_suchen_server],
  ../../R/pages/suchen_ergebnis[module_suche_ergebnis_server],
  ../../R/pages/home[module_home_server],
  ../../R/pages/monitor[module_monitor_server],
  ../../R/pages/studies[module_studies_server],
  ../../R/pages/stories[module_stories_server],
  ../../R/pages/stories_inhalt[module_stories_inhalt_server],
  ../../R/pages/monitor_inhalt[module_monitor_inhalt_server],
  ../../R/utils/routing[add_param_in_url, recode_param_int],
  ../../R/utils/tutorial[plan_tutorial_tour],
  ../../R/utils/routing[get_hf_param],
  ../../R/pages/fdz[module_fdz_server],
  ../../R/pages/team[module_team_server],

  shiny[
    observeEvent, reactiveValues,
    renderUI, HTML, div,
    actionButton
  ],
  shinyjs[
    removeCssClass,
    addCssClass,
    runjs
  ],
  shiny.router[
    change_page,
    router_server,
    get_page,
    get_query_param
  ],
  shinyWidgets[
    updateSearchInput,
    updateRadioGroupButtons
  ],
  utils[URLencode]
)

# Global Variables
GUIDE             <- plan_tutorial_tour()
EXPLORER_SUBPAGES <- report_explorer_subpages()
HF_FILTER         <- data.frame(id = 0:2, label = c("alle", "bildung", "forschung"))

#' Missing description
#' @export

server <- function(input, output, session) {

  sidebar_dynamic_explorer <- renderUI(HTML(""))
  sidebar_dynamic_monitor  <- renderUI(HTML(""))
  sidebar_dynamic_stories  <- renderUI(HTML(""))
  sidebar_dynamic_studies  <- renderUI(HTML(""))

  #  Reaktive Werte erstellen ----------------------------------------------------------------------

  sidebar <-
    reactiveValues(
      start     = TRUE,
      minimized = FALSE
    )

  current <-
    reactiveValues(
      page    = "start",
      sidebar = "start"
    )

  # URL Events -------------------------------------------------------------------------------------

  observeEvent(
    get_query_param(), {

      param_hf <-
        get_query_param("hf") |>
        recode_param_int()
      if (param_hf == ""){
        param_hf <- 0
      }

      check_inhalt <- HF_FILTER$label[match(param_hf, HF_FILTER$id)]
      if (input$sb_handlung != check_inhalt){
        updateRadioGroupButtons(
          inputId = "sb_handlung",
          selected = check_inhalt
        )
      }

    }
  )

  observeEvent(
    get_page(),{
      page <- get_page()
      if (page != current$page){
        current$page <- page
        runjs("window.scrollTo(0,0);")
      }
    }
  )

  observeEvent(
    current$page, {

      test_none_explorer <- TRUE

      if (
        current$page %in%
        c(
          EXPLORER_SUBPAGES$url,
          "monitor", "monitor_inhalt",
          "stories", "stories_inhalt",
          "studies"
        )
      ){
        if (current$page %in% EXPLORER_SUBPAGES$url){
          output$sidebar_dynamic_explorer <- renderUI({draw_sidebar_explorer(current$page)})
          test_none_explorer <- FALSE
        }
        removeCssClass("sidebar_group_filter", "div_hide")
      } else {
        addCssClass("sidebar_group_filter", "div_hide")
      }

      page_category <- current$page
      page_category <- ifelse(page_category %in% EXPLORER_SUBPAGES$url, "explorer", page_category)
      page_category <- ifelse(page_category %in% "monitor_inhalt", "monitor", page_category)
      page_category <- ifelse(page_category %in% "stories_inhalt", "stories", page_category)

      for (i in c(
        "studies",   "stories",
        "monitor",   "explorer",
        "impressum", "datenschutz",
        "team", "fdz"
      )){
        if (i %in% c(page_category)){
          addCssClass(paste0("sb_", i), "btn-warning")
        } else {
          removeCssClass(paste0("sb_", i), "btn-warning")
        }
      }

      if (test_none_explorer){
        output$sidebar_dynamic_explorer <- renderUI({HTML("")})
      }
    }
  )

  # Button-Events ----------------------------------------------------------------------------------

  observeEvent(input$button_minimize, {
    sidebar$minimized <- !sidebar$minimized
    if (sidebar$minimized){
      addCssClass("sidebar", "sidebar_minimized")
    } else {
      removeCssClass("sidebar", "sidebar_minimized")
    }
  })

  observeEvent(input$sb_stories,     {change_page("stories?hf=0")})
  observeEvent(input$sb_monitor,     {change_page("monitor?hf=0")})
  observeEvent(input$sb_explorer,    {change_page("explorer?hf=0")})
  observeEvent(input$sb_studies,     {change_page("studies?hf=0")})
  observeEvent(input$sb_impressum,   {change_page("impressum")})
  observeEvent(input$sb_team,        {change_page("team")})
  observeEvent(input$sb_datenschutz, {change_page("datenschutz")})
  observeEvent(input$sb_fdz,         {change_page("fdz")})

  observeEvent(input$sbd_explorer_suche,     {change_page("suchen")})
  # observeEvent(input$sbd_explorer_indikator, {change_page("indikator")})
  # observeEvent(input$sbd_explorer_vergleich, {change_page("vergleichen")})
  observeEvent(input$sbd_explorer_analysetool, {change_page("analysetool")})
  observeEvent(input$sbd_explorer_datensatz, {change_page("datensaetze")})
  observeEvent(input$sbd_explorer_karten,    {change_page("karten")})

  observeEvent(input$sb_handlung, {
    current_url <- session$clientData$url_hash
    new_url <-
      add_param_in_url(
        current_url  = current_url,
        current_page = get_page(),
        parameter    = "hf",
        value        = HF_FILTER$id[match(input$sb_handlung, HF_FILTER$label)],
        old_value    = get_query_param("hf")
      )
    if (new_url != current_url){
      change_page(new_url)
    }
  })

  observeEvent(
    input$sb_help, {
      GUIDE$init()$start()
    }
  )

  # Suche im Header --------------------------------------------------------------------------------

  observeEvent(input$nav_suchen, {
    suchwort <- input$nav_suchen
    if (!is.null(suchwort)){
      if (length(suchwort) == 1){
        if (!is.na(suchwort)){
          if (suchwort != ""){
            updateSearchInput(session = session, inputId = "nav_suchen", value = "")
            runjs("document.getElementById('nav_suchen_text').blur();")
            change_page(paste0("suchen?term=", URLencode(suchwort, reserved = TRUE)))
          }
        }
      }
    }

  })

  # --- Routing ------------------------------------------------------------------------------------

  router_server()

  # --- Server der Shiny-Modules -------------------------------------------------------------------

  module_explorer_server()
 # module_indikator_server()
  #module_vergleichen_server()
  module_analysetool_server()
  module_suchen_server()
  module_suche_ergebnis_server()
  module_home_server()
  module_studies_server()
  module_stories_server()
  module_stories_inhalt_server()
  module_monitor_server()
  module_monitor_inhalt_server()
  module_fdz_server()
  module_team_server()

}

#' Erstellen der Navigation für den Explorer
#' @noRd

draw_sidebar_explorer <- function(url = NULL){
  div(
    class = "sidebar_dynamic",
    apply(
      EXPLORER_SUBPAGES[EXPLORER_SUBPAGES$url != "explorer",], 1,
      \(x){
        actionButton(
          inputId = x["id"],
          label   = x["label"],
          class   =
            paste0(
              "sidebar_dynamic_button",
              ifelse(x["url"] %in% url, " btn_selected", "")
            )
        )
      }
    )
  )
}
