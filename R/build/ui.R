#' Necessary Packages/Functions

box::use(
  ../../R/pages/home[module_home_ui],
  #../../R/pages/indikator[module_indikator_ui],
  ../../R/pages/suchen[module_suchen_ui],
  ../../R/pages/suchen_ergebnis[module_suche_ergebnis_ui],
  #../../R/pages/vergleichen[module_vergleichen_ui],
  ../../R/pages/analysetool[module_analysetool_ui],
  ../../R/pages/datensaetze[module_datensaetze_ui],
  ../../R/pages/studies[module_studies_ui],
  ../../R/pages/stories[module_stories_ui],
  ../../R/pages/stories_inhalt[module_stories_inhalt_ui],
  ../../R/pages/monitor[module_monitor_ui],
  ../../R/pages/explorer[module_explorer_ui],
  ../../R/pages/handlung_1[module_handlung_1_ui],
  ../../R/pages/handlung_2[module_handlung_2_ui],
  ../../R/pages/impressum[module_impressum_ui],
  ../../R/pages/datenschutz[module_datenschutz_ui],
  ../../R/pages/fdz[module_fdz_ui],
  ../../R/pages/team[module_team_ui],
  ../../R/pages/karten[module_karten_ui],
  ../../R/pages/monitor_inhalt[module_monitor_inhalt_ui],
  ../../R/utils/ui[draw_search],
  ../../R/utils/js[get_js],
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, tagList, tags, HTML,
    navbarPage, icon, actionButton,
    column, div, h4, h5, br, a,
    uiOutput
  ],
  bsplus[use_bs_tooltip],
  shiny.router[router_ui, route],
  shinyjs[useShinyjs],
  shinyWidgets[searchInput, radioGroupButtons],
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
      get_js("go_back_page"),
      tags$link(rel = "apple-touch-icon-precomposed", sizes = "180x180", href = "https://www.stifterverband.org/themes/custom/cake/res/favicons/apple-touch-icon.png"),
      tags$link(rel = "icon",                         sizes = "192x192", href = "https://www.stifterverband.org/themes/custom/cake/res/favicons/touch-icon-192x192.png"),
      tags$link(rel = "shortcut icon",                                   href = "https://www.stifterverband.org/themes/custom/cake/res/favicons/favicon.ico"),
      tags$link(rel = "stylesheet", type = "text/css", href = paste0("styles.css?version=", Sys.time())),
      tags$link(rel = "stylesheet", type = "text/css", href = "WebKit_bounding.css"),
      tags$title("SV Datenportal")
    ),
    use_bs_tooltip(),
    title = NULL,

    # BODY
    draw_header(),
    fluidPage(
      style = "padding:0; background-color: #F2F2F2; display: flex;",
      id    = "main-body",
      draw_sidebar(),
      div(
        id  = "content-body",
        router_ui(
          route("/",              module_home_ui()),
          route("stories",        module_stories_ui()),
          route("stories_inhalt", module_stories_inhalt_ui()),
          route("monitor",        module_monitor_ui()),
          route("monitor_inhalt", module_monitor_inhalt_ui()),
          route("explorer",       module_explorer_ui()),
          #route("indikator",      module_indikator_ui()),
          route("suchen",         module_suchen_ui()),
          route("suchergebnisse", module_suche_ergebnis_ui()),
          #route("vergleichen",    module_vergleichen_ui()),
          route("analysetool",    module_analysetool_ui()),
          route("datensaetze",    module_datensaetze_ui()),
          route("studies",        module_studies_ui()),
          route("handlung1",      module_handlung_1_ui()),
          route("handlung2",      module_handlung_2_ui()),
          route("impressum",      module_impressum_ui()),
          route("datenschutz",    module_datenschutz_ui()),
          route("team",           module_team_ui()),
          route("karten",         module_karten_ui()),
          route("fdz",            module_fdz_ui())
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
  div(
    id = "sidebar",
    div(
      class = "sidebar_group",
      h4("Formate", class = "sidebar_title"),
      div(
        style = "background-color: white;",
        actionButton("sb_explorer", label = "Daten Explorer", class = "sidebar_button", icon = icon("magnifying-glass")),
        uiOutput("sidebar_dynamic_explorer"),
        actionButton("sb_monitor",  label = "Monitoring",     class = "sidebar_button", icon = icon("chart-pie")),
        uiOutput("sidebar_dynamic_monitor"),
        actionButton("sb_stories",  label = "Analysen",       class = "sidebar_button", icon = icon("newspaper")),
        uiOutput("sidebar_dynamic_stories"),
        actionButton("sb_studies",  label = "Projekte",       class = "sidebar_button", icon = icon("square-poll-vertical")),
        uiOutput("sidebar_dynamic_studies")
      )
    ),
    div(
      id = "sidebar_group_filter",
      class = "sidebar_group div_hide",
      h4("Filter", class = "sidebar_title"),
      class = "radio_handlung_sidebar",
      radioGroupButtons(
        inputId      = "sb_handlung",
        label        = NULL,
        choiceValues = c("alle", "bildung", "forschung"),
        choiceNames  = c("Alle Inhalte", "Bildung &<br>Kompetenzen", "Forschung &<br>Innovation"),
        selected     = NULL,
        direction    = "vertical",
        checkIcon    = list(
          yes = tags$i(class = "fa fa-check-square"),
          no  = tags$i(class = "fa fa-square-o")
        )
      )
    ),
    div(
      class = "sidebar_group",
      h4("Weitere Inhalte", class = "sidebar_title"),
      div(
        style = "background-color: white;",
        actionButton("sb_team",        label = "SV DATA",     class = "sidebar_button", icon = icon("user-group")),
        actionButton("sb_fdz",         label = "FDZ",         class = "sidebar_button", icon = icon("right-to-bracket")),
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
    HTML("<header class='top sticky'>"),
    div(
      class = "header-panel",
      div(
        class = "header-left",
        style = "display: flex;",
        actionButton(
          "button_minimize",
          label = "",
          class = "sidebar_button",
          icon = icon("bars"),
          style = "width: auto; border: 0px solid white; font-size: 24px; color: var(--blue)"
        ),
        a(
          h5(
            "SV DATENPORTAL",
            id    = "header-left-title",
            style = "font-weight: 900; font-size: 26px; margin: auto 0;"
          ),
          class = "header_link",
          href = "/#!/"
        ),
      ),
      div(
        class = "header-middle",
        a(
          HTML("<img class='brand__logo' src='https://stifterverband.org//themes/custom/cake/res/logo_stifterverband_wide.svg' alt='Logo Stifterverband'>"),
          class = "header_link",
          href = "/#!/"
        )
      ),
      div(
        class = "header-right",
        style = "display: flex; flex-direction: row-reverse;",
        actionButton("sb_help", label = NULL, icon("circle-question")),
        div(
          class = "top-search",
          draw_search(
            inputId     = "nav_suchen",
            placeholder = "Datenbank durchsuchen ...",
            btnSearch   = icon("search"),
            btnReset    = icon("remove"),
            width       = "100%"
          )
        )
      )
    ),
    HTML("</header>"),
    div(class = "header-bottom")
  )
}
