#' Necessary Packages/Functions

box::use(
  ../../R/pages/home[module_home_ui],
  ../../R/pages/indikator[module_indikator_ui],
  ../../R/pages/suchen[module_suchen_ui],
  ../../R/pages/vergleichen[module_vergleichen_ui],
  ../../R/pages/datensaetze[module_datensaetze_ui],
  ../../R/pages/studies[module_studies_ui],
  ../../R/pages/stories[module_stories_ui],
  ../../R/pages/monitor[module_monitor_ui],
  ../../R/pages/explorer[module_explorer_ui],
  ../../R/pages/handlung_1[module_handlung_1_ui],
  ../../R/pages/handlung_2[module_handlung_2_ui],
  ../../R/pages/impressum[module_impressum_ui],
  ../../R/pages/datenschutz[module_datenschutz_ui],
  ../../R/pages/team[module_team_ui],
  ../../R/pages/karten[module_karten_ui],
  ../../R/pages/monitor_bildung_inhalt[module_monitor_bildung_inhalt_ui],
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, tagList, tags, HTML,
    navbarPage, icon, actionButton,
    column, div, h4, h5, br,
    uiOutput
  ],
  bsplus[use_bs_tooltip],
  shiny.router[router_ui, route],
  shinyjs[useShinyjs],
  cicerone[use_cicerone]
)


#' Missing description
#' @export

draw_ui <- function(){
  fluidPage(
    style = "padding: 0px;",
    useShinyjs(),
    use_cicerone(),

    # HEAD AND STYLE
    tags$head(
      tags$link(rel = "apple-touch-icon-precomposed", sizes = "180x180", href = "https://www.stifterverband.org/themes/custom/cake/res/favicons/apple-touch-icon.png"),
      tags$link(rel = "icon",                         sizes = "192x192", href = "https://www.stifterverband.org/themes/custom/cake/res/favicons/touch-icon-192x192.png"),
      tags$link(rel = "shortcut icon",                                   href = "https://www.stifterverband.org/themes/custom/cake/res/favicons/favicon.ico"),
      tags$link(rel = "stylesheet", type = "text/css", href = paste0("styles.css?version=", Sys.time())),
      tags$title("SV Datenportal")
    ),
    use_bs_tooltip(),
    title = NULL,

    # BODY
    draw_header(),
    fluidPage(
      style = "padding:0; background-color: #F2F2F2; display: flex;",
      id    = "main-body",
      div(
        id  = "sidebar",
        draw_sidebar()
      ),
      div(
        id  = "content-body",
        router_ui(
          route("/",                      module_home_ui()),
          route("stories",                module_stories_ui()),
          route("monitor",                module_monitor_ui()),
          route("explorer",               module_explorer_ui()),
          route("indikator",              module_indikator_ui()),
          route("suchen",                 module_suchen_ui()),
          route("vergleichen",            module_vergleichen_ui()),
          route("datensaetze",            module_datensaetze_ui()),
          route("studies",                module_studies_ui()),
          route("handlung1",              module_handlung_1_ui()),
          route("handlung2",              module_handlung_2_ui()),
          route("monitor_bildung_inhalt", module_monitor_bildung_inhalt_ui()),
          route("impressum",              module_impressum_ui()),
          route("datenschutz",            module_datenschutz_ui()),
          route("team",                   module_team_ui()),
          route("karten",                 module_karten_ui())
        )
      )
    ),
    draw_footer()
  )
}

#' Missing description
#' @noRd

navigation_bar <-
  navbarPage(
    title       = "Platzhalter",
    windowTitle = "SV dataVerse",
    id          = "navigation_bar",
    selected    = "SV Data",
    # tabPanel("SV Data",    icon = icon("chart-simple")),
    # tabPanel("Handlung 1", icon = icon("graduation-cap")),
    # tabPanel("Handlung 2", icon = icon("lightbulb")),
    fluid       = TRUE,
    collapsible = TRUE
  )

#' Missing description
#' @noRd

draw_footer <- function() {
  tagList(
    HTML('<footer class="main-footer" data-fixed="false">'),
    HTML(
      "<div class='bottom__left'>
        <a class='brand' href='/' rel='home'><img class='brand__logo' src='https://stifterverband.org/themes/custom/cake/res/logo_stifterverband.svg' alt='Logo Stifterverband'></a>
       </div>"
    ),
    HTML("</footer>")
  )
}

#' Missing description
#' @noRd

draw_sidebar <- function(){
  tagList(
    div(
      class = "sidebar_group",
      h4("Handlungsfelder", class = "sidebar_title", style = "margin-top: 40px;"),
      div(
        style = "background-color: white;",
        actionButton("sb_handlung1",  label = HTML("Bildung &<br>Kompetenzen"),   class = "sidebar_button_hf_1"),
        actionButton("sb_handlung2",  label = HTML("Forschung &<br>Innovation"),  class = "sidebar_button_hf_2"),
      )
    ),
    div(
      class = "sidebar_group",
      h4("Formate", class = "sidebar_title", style = "margin-top: 40px;"),
      div(
        style = "background-color: white;",
        actionButton("sb_stories",  label = "Stories",  class = "sidebar_button", icon = icon("newspaper")),
        actionButton("sb_monitor",  label = "Monitor",  class = "sidebar_button", icon = icon("chart-pie")),
        actionButton("sb_explorer", label = "Explorer", class = "sidebar_button", icon = icon("magnifying-glass")),
        actionButton("sb_studies",  label = "Studies",  class = "sidebar_button", icon = icon("square-poll-vertical")),
      )
    ),
    div(
      class = "sidebar_group",
      h4("Inhalt", class = "sidebar_title", style = "margin-top: 40px;"),
      uiOutput("sidebar_dynamic")
    ),
    div(
      class = "sidebar_group",
      h4("Mehr über uns", class = "sidebar_title", style = "margin-top: 40px;"),
      div(
        style = "background-color: white;",
        actionButton("sb_team",        label = "SV DATA",     class = "sidebar_button", icon = icon("user-group")),
        actionButton("sb_impressum",   label = "Impressum",   class = "sidebar_button"), #, icon = icon("circle-info")
        actionButton("sb_datenschutz", label = "Datenschutz", class = "sidebar_button") #, icon = icon("shield-halved")
        # actionButton("sb_ziviz",     label = "ZiviZ",       class = "sidebar_button", icon = icon("people-group")),
        # actionButton("sb_ki",        label = "KI",          class = "sidebar_button", icon = icon("brain")),
      )
    ),
    div(style = "margin: 80px;")
  )
}

#' Missing description
#' @noRd

draw_header <- function(){
  tagList(
    HTML("<header class='top' style = 'display: grid; grid-template-columns: 1fr auto 1fr; align-items: center; background-color: white; max-width: var(--max-width); margin: 0 auto; padding: 25px;'>"),
    div(
      class = "top__left",
      style = "display: flex;",
      actionButton(
        "button_minimize",
        label = "",
        class = "sidebar_button",
        icon = icon("bars"),
        style = "width: auto; border: 0px solid white; font-size: 24px; color: var(--blue)"
      ),
      h5(
        "SV DATENPORTAL",
        id    = "header-left-title",
        style = "font-weight: 900; font-size: 26px; margin: auto 0;"
      ),
    ),
    div(
      class = "top__middle",
      HTML("<img class='brand__logo' src='https://stifterverband.org//themes/custom/cake/res/logo_stifterverband_wide.svg' alt='Logo Stifterverband'>")
    ),
    div(
      class = "top__right"
    ),
    HTML("</header>")

  )
}

