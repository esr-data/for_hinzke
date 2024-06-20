box::use(
  ../../R/utils/routing[get_hf_param, add_param_in_url, recode_parameter],
  ../../R/utils/database[get_query, load_table_by_variable],
  ../../R/utils/js[get_js],
  ../../R/utils/monitor[get_content_monitor],
  ../../R/pkgs/svVis/create_bar_grouped[create_bar_grouped_interactive],
  ../../R/pkgs/svVis/create_bar[create_bar],
  ../../R/pkgs/svVis/create_flextable[create_flextable],
  ../../R/prepare/data_monitor_zwischenloesung[
    give_df_ganztag_vielfalt_primar,
    give_df_ganztag_vielfalt_sek_I,
    give_df_ganztag_vielfalt_gym,
    give_df_ganztag_kooperation,
    give_df_ganztag_multiprofessionell,
    give_df_ganztag_lage_arbeitsmotivation_schulleitungen,
    give_df_ganztag_lage_weiterempfehlung_schulleiterberuf,
    give_df_gerechtigkeit_trichter_gymnasiumswahrscheinlichkeit
  ],
  ggplot2[
    theme,
    element_text
  ],
  tibble[
    tibble
  ],
  reactable[
    renderReactable,
    reactable,
    reactableOutput,
    colDef
  ],
  officer[
    fp_border
  ],
  flextable[
    htmltools_value,
    flextable,
    color,
    bg,
    border_outer,
    border_inner_h,
    border_inner_v,
    autofit,
    align,
    merge_v
  ],
  shiny[
    reactive,
    span,
    actionButton,
    br,
    column,
    conditionalPanel,
    div,
    fluidPage, fluidRow,
    h1, h2, h3, h4,
    HTML,
    img,
    mainPanel,
    markdown,
    moduleServer,
    numericInput, #
    NS,
    observe, #
    observeEvent,
    p,
    radioButtons, #
    updateRadioButtons,
    reactiveVal,
    reactiveValues,
    renderUI,
    req, #
    selectInput, #
    selectizeInput,
    sidebarLayout,
    sidebarPanel,
    tagList,
    tags,
    uiOutput,
    icon,
    plotOutput,
    renderPlot
  ],
  shiny.router[get_query_param, get_page, change_page],
  shinyWidgets[
    pickerInput,       updatePickerInput,
    radioGroupButtons, updateRadioGroupButtons,
  ],
  plotly[
    plotlyOutput, renderPlotly, plotly
  ],
  magrittr[`%>%`],
  shinycssloaders[withSpinner],
  dplyr[rename, filter],
  purrr[map2],
  DT[datatable, JS, renderDataTable, dataTableOutput, renderDT],
  shinyjs[
    removeCssClass,
    addCssClass
  ]
)
#TODO irrelevanten Verknüpfungen aussortieren!

# Global Variables

MAX_VARIABLEN   <- 12
content_monitor <- get_content_monitor()
darstellungen   <- list(a = c("Zeitverlauf", "Tabelle", "Karte"))

# UI-Modul -----------------------------------------------------------------------------------------

MAX_VARIABLEN   <- 12
content_monitor <- get_content_monitor()
darstellungen   <- list(a = c("Zeitverlauf", "Tabelle", "Karte"))

# UI-Modul -----------------------------------------------------------------------------------------

module_monitor_inhalt_ui <- function(id = "monitor_inhalt", label = "m_monitor_inhalt") {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = paste0("monitor_styles.css?version=", Sys.time())
      )
    ),
    fluidPage(
      div(
        class = "panel-content",
        div(
          class = "monitor-subpage-title",
          uiOutput(ns("title")),
          div(class = "monitor-subpage-title-clipgraph")
        ),
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            fluidRow(
              class = "back_button_row",
              actionButton(ns("back_button"), label = HTML("Zurück<br>zur Übersicht"))
            ),
            fluidRow(
              width = 12,
              div(
                class = "fragen-indikatoren-ziele-box monitor-sidbar-row",
                radioGroupButtons(
                  ns("switch_fragen_indikatoren_ziele"),
                  selected = c("Fragen"),
                  choices = c("Fragen", "Ziele", "Indikatoren"),
                  justified = TRUE
                ),
                uiOutput(ns("liste_fragen_ziele_indikatoren"))
              )
            ),
            uiOutput(ns("information_box"))
          ),
          mainPanel(
            width = 9,
            uiOutput(ns("main_panel")),
            div(
              style = "margin: 0 auto; display: table; margin-bottom: 20px;",
              h3(
                style = "font-weight: 600; text-align: center; max-width: 175px; border-bottom: 3px solid var(--blue); border-radius: 3px; padding-bottom: 6px;",
                "Indikatoren"
              )
            ),
            div(
              class = "content-ask-aim-ind",
              reactableOutput(ns("content_table"))
            )
          )
        )
      )
    )
  )
}

# Server-Modul -------------------------------------------------------------------------------------

module_monitor_inhalt_server <- function(id = "monitor_inhalt") {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns

                 current <-
                   reactiveValues(
                     content   = NULL,
                     variable  = NULL,
                     parameter = list(
                       hf  = "",
                       tp  = "",
                       fr  = "",
                       ind = ""
                     )
                   )

                 observeEvent(
                   get_query_param(), {

                     if (get_page() == "monitor_inhalt"){

                       change_in_choice <- FALSE
                       skip_from_now_on <- FALSE
                       parameter        <- get_query_param()

                       # PARAMETER tp -----------------------------------------------------------------------
                       param_tp <-
                         parameter$tp |>
                         recode_parameter(
                           is_integer = FALSE,
                           value_set  = names(content_monitor)
                         )

                       if (param_tp != current$parameter$tp){
                         current$parameter$tp <- param_tp
                         if (param_tp == ""){
                           skip_from_now_on  <- TRUE
                           # current$parameter <- parameter_default
                           # change_page("monitor?hf=1")
                         } else {
                           current$content        <- content_monitor[[param_tp]]
                           output$title           <- renderUI({h1(class = "monitor-subpage-title-headline", current$content[["Titel"]])})
                           output$information_box <- renderUI({draw_information_box(current$content)})
                           output$main_panel      <- renderUI({draw_main_content(current$content)})
                         }
                       }

                       if (!skip_from_now_on){

                         # PARAMETER fr ---------------------------------------------------------------------
                         select_choices <- c("Fragen", "Ziele", "Indikatoren")
                         param_fr <-
                           parameter$fr |>
                           recode_parameter(
                             is_integer = FALSE,
                             value_set  = substr(select_choices, 1, 2)
                           )
                         if (param_fr == "") param_fr <- "Fr"
                         if (param_fr != current$parameter$fr){
                           current$parameter$fr <- param_fr
                           if (!is.null(input$switch_fragen_indikatoren_ziele)){
                             selected_choice <- select_choices[param_fr == substr(select_choices, 1, 2)]
                             if (selected_choice != input$switch_fragen_indikatoren_ziele){
                               updateRadioGroupButtons(
                                 session = session,
                                 inputId = "switch_fragen_indikatoren_ziele",
                                 selected = selected_choice
                               )
                             }
                           }

                           output$liste_fragen_ziele_indikatoren <-
                             renderUI({
                               draw_fragen_ziele_indikatoren(
                                 current$content,
                                 select_choices[substr(select_choices, 1, 2) == param_fr]
                               )
                             })
                         }

                         # PARAMETER ind Teil 1 -------------------------------------------------------------
                         param_ind <-
                           parameter$ind |>
                           recode_parameter(
                             is_integer = TRUE,
                             value_set  = 1:length(current$content$Inhalt$Titel)
                           )
                         if (param_ind == ""){
                           param_ind <- 1
                         }
                       }
                     }

                   }
                 )

                 observeEvent(
                   input$back_button, {
                     param_hf <- get_hf_param()
                     if (param_hf != "") param_hf <- sprintf("?hf=%s", param_hf)
                     change_page(paste0("monitor", param_hf))
                   }
                 )

                 observeEvent(
                   input$switch_fragen_indikatoren_ziele, {
                     if (!is.null(current$content)){
                       if (get_page() == "monitor_inhalt"){
                         current_url <- session$clientData$url_hash
                         new_url <-
                           add_param_in_url(
                             current_url  = current_url,
                             current_page = "monitor_inhalt",
                             parameter    = "fr",
                             value        = substr(input$switch_fragen_indikatoren_ziele, 1, 2),
                             old_value    = get_query_param("fr")
                           )
                         if (new_url != current_url){
                           change_page(new_url)
                         }
                       }
                     }
                   }
                 )

                 output$content_table <- renderReactable({
                   reactable(
                     tibble(section = unlist(current$content[["Inhalt"]][["Titel"]])),
                     columns = list(
                       section = colDef(name = "")
                     ),
                     details = function(index) {
                       fluidPage(
                         div(
                           class = "content-ask-aim-ind",
                           tags$ul(
                             tags$li(tags$i(class = "fa-solid fa-clipboard-question"), tags$b("Frage: "), current$content[["Inhalt"]][["Frage"]][[index]]),
                             tags$li(tags$i(class = "fa-solid fa-bullseye"), tags$b("Ziel: "), current$content[["Inhalt"]][["Ziel"]][[index]]),
                             tags$li(tags$i(class = "fa-solid fa-ruler"), tags$b("Indikator: "), current$content[["Inhalt"]][["Indikator"]][[index]])
                           )
                         ),
                         br(),
                         div(
                           draw_table_row_content(current$content$Inhalt$ID[[index]], ns)
                         )
                       )
                     }
                   )
                 })

                 # bildung ----
                 #### ganztag -----------------------------------------------------

                 #1 quantitativ

                 # output$Grafik_ganztag_quantitativ <- renderPlotly({
                 #
                 # })

                 output$Grafik_ganztag_quantitativ <- renderUI({
                   fluidRow(
                     br(),
                     br(),
                     icon(
                       style = "margin-left: 50px; color: var(--red); font-size: 50px;",
                       "screwdriver-wrench"
                     ),
                     p(
                       style = "margin-left: 50px; color: var(--red);",
                       tags$b("Hier wird noch gebaut:"),
                       "Interne Notiz - der Indikator wird gerade noch in das SV DataWarehouse eingespielt und wird bald ergänzt. Zu sehen sein wird ein interaktives Liniendiagramm, welches mit den linken Filtern bedienbar ist."
                     )
                    )
                 })

                 output$Anmerkungen_bildung_ganztag_quantitativ <- renderUI({
                   p(class = "anmerkungen",
                     "Definitionen und methodische Ergänzungen zur Statistikreihe finden sich ",
                     tags$a(href = "https://www.kmk.org/dokumentation-statistik/statistik/schulstatistik/allgemeinbildende-schulen-in-ganztagsform.html", "hier", target = "_blank"),
                     "."
                   )
                 })

                 #2 vielfalt

                 output$Tabelle_ganztag_vielfalt <- renderUI({
                   req(input$Auswahl_ganztag_vielfalt_schulform)
                   if (input$Auswahl_ganztag_vielfalt_schulform == "primar"){
                     create_flextable(give_df_ganztag_vielfalt_primar()) %>%
                       htmltools_value()
                   } else if(input$Auswahl_ganztag_vielfalt_schulform == "Sek I (ohne Gymnasium)") {
                     create_flextable(give_df_ganztag_vielfalt_sek_I()) %>%
                       htmltools_value()
                   } else {
                     create_flextable(give_df_ganztag_vielfalt_gym()) %>%
                       htmltools_value()
                   }

                 })

                 output$Anmerkungen_bildung_ganztag_vielfalt <- renderUI({
                   p(class = "anmerkungen",
                     "Definitionen und methodische Ergänzungen zur Statistikreihe finden sich ",
                     tags$a(href = "https://www.dipf.de/de/forschung/pdf-forschung/llib/bericht-ganztagsschulen-2017-2018", "hier", target = "_blank"),
                     ". Die Daten sind von 2017/2018 und damit veraltet. Der Stifterverband plant eine Schulleitungsbefragung zur Datenaktualisierung. Damit soll dann auch ein klarer Indikator
                     \"Median Anzahl Angebotsbereiche je Schule\" ausweisbar sein."
                   )
                 })

                 #3 kooperation

                 df_ganztag_kooperation <- reactive({
                   req(input$Auswahl_ganztag_kooperation_schulform)

                   df <- give_df_ganztag_kooperation()

                   if (input$Auswahl_ganztag_kooperation_schulform == "insgesamt") {
                     df <- df %>% filter(Schulform == "insgesamt")
                   } else if (input$Auswahl_ganztag_kooperation_schulform == "Grundschule") {
                     df <- df %>% filter(Schulform == "Grundschule")
                   } else if (input$Auswahl_ganztag_kooperation_schulform == "Haupt-/ Real-/ Gesamtschule") {
                     df <- df %>% filter(Schulform == "Haupt-/ Real-/ Gesamtschule")
                   } else if (input$Auswahl_ganztag_kooperation_schulform == "Gymnasium") {
                     df <- df %>% filter(Schulform == "Gymnasium")
                   } else {
                     df <- df %>% filter(Schulform == "Förder-/ Sonderschule")
                   }

                   df
                 })

                 output$Grafik_ganztag_kooperation <- renderPlotly({
                   create_bar(
                     df_ganztag_kooperation(),
                     x_var = Auspraegung,
                     y_var = value,
                     plot_type = "plotly", #TODO: Updaten auf neue Fassung von svVis
                     ylabel_text = "Prozent",
                     plot_title = "Kooperation mit außerschulischen Partnern zur Gestaltung unterrichtsbezogener Angebote",
                     plot_subtitle = "Die Schule kooperiert mit außerschulischen Lernorten/Bildungspartnern zur Gestaltung unterrichtsbezogener Angebote; Angaben in Prozent.",
                     custom_caption = "Quelle: Telekom Stiftung 2023."
                   )
                 })

                 output$Anmerkungen_bildung_ganztag_kooperation <- renderUI({
                   p(class = "anmerkungen",
                     "Methodische Ergänzungen zur Statistik finden sich ",
                     tags$a(href = "https://www.telekom-stiftung.de/sites/default/files/files/umfrage_multiprofessionalitaet_ergebnisbericht.pdf", "hier", target = "_blank"),
                     "."
                   )
                 })

                 #4 multiprofessionell

                 output$Tabelle_ganztag_multiprofessionell <- renderUI({
                   flextable(give_df_ganztag_multiprofessionell()) %>%
                     bg(bg = "#195365", part = "header") %>%
                     color(color = "white", part = "header") %>%
                     border_outer(part = "all", border = fp_border(color = "black")) %>%
                     border_inner_h(part = "all", border = fp_border(color = "black")) %>%
                     border_inner_v(part = "all", border = fp_border(color = "black")) %>%
                     merge_v(j = "Bereich") %>%
                     autofit() %>%
                     align(align = "center", part = "head") %>%
                     align(j = 2:ncol(give_df_ganztag_multiprofessionell()), align = "center", part = "body") %>%
                     htmltools_value()
                  })

                 output$Anmerkungen_bildung_ganztag_multiprofessionell <- renderUI({
                   p(class = "anmerkungen",
                     "Methodische Ergänzungen sowie weitere Statistiken zur Bewertung der Lage an Schulen finden sich ",
                     tags$a(href = "https://www.telekom-stiftung.de/sites/default/files/files/umfrage_multiprofessionalitaet_ergebnisbericht.pdf", "hier", target = "_blank"),
                     "."
                   )
                 })

                 #5 lage

                 output$Grafik_ganztag_lage <- renderPlotly({
                   req(input$Auswahl_ganztag_lage_frage)
                   if (input$Auswahl_ganztag_lage_frage == "Arbeitsmotivation der Schulleitungen") {
                       create_bar_grouped_interactive(
                         df = give_df_ganztag_lage_arbeitsmotivation_schulleitungen(), #TODO Daten in Magpie aufbereiten
                         x_var = Jahr,
                         y_var = Prozent,
                         group_var = Kategorie,
                         flipped = TRUE,
                         plot_title = "Arbeitsmotivation der Schulleiterinnen und Schulleiter",
                         plot_subtitle = "Es üben ihren Beruf derzeit alles in allem ... aus; Angaben in Prozent; N = 1.310.",
                         custom_caption = "Quelle: VBE 2023."
                       )
                   } else {
                      create_bar_grouped_interactive(
                        df = give_df_ganztag_lage_weiterempfehlung_schulleiterberuf(), #TODO Daten in Magpie aufbereiten
                        x_var = Jahr,
                        y_var = Prozent,
                        group_var = Kategorie,
                        flipped = TRUE,
                        plot_title = "Weiterempfehlungsbereitschaft des Schulleitungsberuf",
                        plot_subtitle = "Es würden den Beruf der Schulleiterin bzw. des Schulleiters weiterempfehlen; Angaben in Prozent; N = 1.310.",
                        custom_caption = "Quelle: VBE 2023."
                      )
                   }
                 })

                 output$Anmerkungen_bildung_ganztag_lage <- renderUI({
                   p(class = "anmerkungen",
                     "Methodische Ergänzungen sowie weitere Statistiken zur Bewertung der Lage an Schulen finden sich ",
                     tags$a(href = "https://deutscher-schulleitungskongress.de/wp-content/uploads/2023/11/2023-11-21_VOe-Nov_Bericht_Deutschland.pdf", "hier", target = "_blank"),
                     "."
                   )
                 })

                 #### bildungsgerechtigkeit ----

                 #1 trichter

                 output$Grafik_bildungsgerechtigkeit_gymnasium_bundeslaender <- renderPlot({
                   create_bar(
                     df = give_df_gerechtigkeit_trichter_gymnasiumswahrscheinlichkeit(),
                     x_var = Land,
                     y_var = Chancenverhaeltnis,
                     plot_title = "Ungleiche Chancen nach Bundesländern",
                     plot_subtitle = "Wahrscheinlichkeit, mit der Kinder aus benachteiligten Verhältnissen ein Gymnasium\nbesuchen gegenüber Kindern aus günstigen Verhältnissen, in Prozent",
                     custom_caption = "Quelle: Ifo Institut" ,
                     flipped = TRUE
                  ) +
                  theme(text = element_text(size = 20))
                 })

                 output$Anmerkungen_bildungsgerichtigkeit_trichter <- renderUI({
                   p(class = "anmerkungen",
                     "Methodische Ergänzungen sowie weitere Statistiken zur Bewertung der Lage an Schulen finden sich ",
                     tags$a(href = "https://deutscher-schulleitungskongress.de/wp-content/uploads/2023/11/2023-11-21_VOe-Nov_Bericht_Deutschland.pdf", "hier", target = "_blank"),
                     "."
                   )
                 })

                 #### berufsorientierung ----

                 #1 subjektiv

                 output$UI_berufsorientierung_subjektiv <- renderUI({
                   fluidRow(
                     br(),
                     br(),
                     icon(
                       style = "margin-left: 50px; color: var(--red); font-size: 50px;",
                       "screwdriver-wrench"
                     ),
                     p(
                       style = "margin-left: 50px; color: var(--red);",
                       tags$b("Hier wird noch gebaut:"),
                       "Interne Notiz - Nach letzter Diskussion mit Mathias soll der ganze Zweig Berufsorientierung rausgenommen werden. Die Indikatoren entfallen demnach - gerade der Indikator zu den Praktika ist sehr schwer zu erheben."
                     )
                   )
                 })

                 #2 praktika

                 output$UI_berufsorientierung_praktika <- renderUI({
                   fluidRow(
                     br(),
                     br(),
                     icon(
                       style = "margin-left: 50px; color: var(--red); font-size: 50px;",
                       "screwdriver-wrench"
                     ),
                     p(
                       style = "margin-left: 50px; color: var(--red);",
                       tags$b("Hier wird noch gebaut:"),
                       "Interne Notiz - Nach letzter Diskussion mit Mathias soll der ganze Zweig Berufsorientierung rausgenommen werden. Die Indikatoren entfallen demnach - gerade der Indikator zu den Praktika ist sehr schwer zu erheben."
                     )
                   )
                 })

               #---
               }

  )
}

#' Missing description
#' @noRd

draw_fragen_ziele_indikatoren <- function(content, input){
  #TODO Lösung ohne gsub
  input <- gsub("Fragen", "Frage", input)
  input <- gsub("Indikatoren", "Indikator", input)
  input <- gsub("Ziele", "Ziel", input)
  tags$ul(
    map2(
      unlist(content["ID"]),
      unlist(content$Inhalt[input]),
      ~ tags$li(
        tags$a(
          class = "link",
          href = paste0("#", .x),
          .y,
          draw_svg_arrow()
        )
      )
    )
  )
}

#' Missing description
#' @noRd

draw_svg_arrow <- function(){
  tags$svg(
    class = "link__arrow",
    width = "10",
    height = "14",
    viewBox = "0 0 10 14",
    `xmlns` = "http://www.w3.org/2000/svg",
    tags$path(
      class = "link__arrow-path",
      d = "M2.55058 14L0.854004 12.3497L6.4527 6.99767L0.854004 1.65025L2.55058 0L9.84645 6.99767L2.55058 14Z"
    )
  )
}

#' Missing description
#' @noRd

draw_information_box <- function(content){
  tagList(
    get_js("click_collapsible_header"),
    create_collapsible_panel(
      "Aktivitäten des Stifterverbandes",
      content$Aktivitaet$URL,
      content$Aktivitaet$Label,
      "aktivitaeten-box"
    ),
    create_collapsible_panel(
      "Datenbasis",
      content$Datenbasis$URL,
      content$Datenbasis$Label,
      "database-box"
    ),
    create_collapsible_panel(
      "weiterführende Links",
      content$Link$URL,
      content$Link$Label,
      "link-box"
    )
  )
}

#' Missing description
#' @noRd

draw_main_content <- function(content){
  tagList(
    div(
      class = "intro-text-monitor-subsite",
      h3(content[["Untertitel"]]),
      content[["Einfuehrungstext"]]
    ),
    div(
      class = "info-box-data",
      tags$i(class = "fa fa-info-circle"),
      markdown(readLines("md/monitor_hinweis_datenportal_projektstand.md"))
    ),
    br(),
    h4("Das Wichtigste in Kürze:"),
    div( # TODO - function/flex für Imagegenerierung anhand der gefilterten Liste
      class = "important-values-graph",
      img(
        src = "img/4er_Ganztag.svg",
        alt = "Wichtige Kennzahlen zum Ausbau der Ganztagsschule in Deutschland"
      )
    ),
    br()
  )
}

#' Missing description
#' @noRd

create_collapsible_panel <- function(title, content_links, content_texts, class_suffix) {
  fluidRow(
    class = paste0(class_suffix, " monitor-sidbar-row"),
    width = 12,
    div(
      h4(
        class = "collapsible-header",
        title,
        tags$i(class = "arrow-down")
      ),
      div(
        class = "collapsible-content",
        tags$ul(
          map2(content_links, content_texts, ~ create_link_with_svg(.x, .y))
        )
      )
    )
  )
}

#' Missing description
#' @noRd

create_link_with_svg <- function(link_id, link_text) {
  tags$li(
    tags$a(
      class = "link",
      href = paste0("#", link_id),
      link_text,
      tags$svg(
        class = "link__arrow",
        width = "10",
        height = "14",
        viewBox = "0 0 10 14",
        `xmlns` = "http://www.w3.org/2000/svg",
        tags$path(
          class = "link__arrow-path",
          d = "M2.55058 14L0.854004 12.3497L6.4527 6.99767L0.854004 1.65025L2.55058 0L9.84645 6.99767L2.55058 14Z"
        )
      )
    )
  )
}

#' Missing description
#' @noRD

draw_table_row_content <- function(indikator_ID, ns) {

  # bildung ----

  #### ganztag ----
  if (startsWith(indikator_ID, "ganztag")) {

    if(indikator_ID == "ganztag_quantitativ") {
      fluidPage(
        p(
          "Aktueller Stand: Derzeit erfüllen ", tags$b("73,2 Prozent aller allgemeinbildeneden Schulen in
          Primar- und Sekundarbereich I"), " die ", span("KMK Definition für Ganztag", id = "tooltip_KMK_Def_Ganztag"),". Dabei gibt es jedoch
          nach Schulträgerschaft, Schulform, Ganztagsform und Bundesländern große
          Unterschiede. ", tags$i("Nutzen Sie die Filter, um die Sie interessierenden Zahlen zu erhalten")
        ),
        br(),
        fluidRow(
          column(
            width = 3,
            selectInput(ns("Auswahl_ganztag_quantitativ_land"), "Land", choices = c("a", "b")),
            selectInput(ns("Auswahl_ganztag_quantitativ_schulform"), "Schulform", choices = c("a", "b")),
            selectInput(ns("Auswahl_ganztag_quantitativ_trägerschaft"), "Trägerschaft", choices = c("a", "b")),
            selectInput(ns("Auswahl_ganztag_quantitativ_ganztagsform"), "Ganztagsform", choices = c("a", "b"))
          ),
          column(
            width = 9,
            # plotlyOutput(ns("Grafik_ganztag_quantitativ"))
            uiOutput(ns("Grafik_ganztag_quantitativ"))
          )
        ),
        br(),
        p(
          "Unter Berücksichtigung der steigenden Zahl an Schülerinnen und Schülern
          gemäß KMK-Prognose, der derzeitigen Ausbaugeschwindigkeit sowie des
          bestehenden und prognostiziertem Lehrkräftemangel ist das Projekt Ausbau Ganztagsschule sowie
          die tatsächliche Erfüllung des Rechtsanspruchs Ganztagsschule im Primarbereich ab 2026 aktuell nicht wie gewünscht zu realisieren.
          Über ein Drittel der Grundschuleitungen gehen davon aus, dass eine Ganztagsbetreuung von der jeweiligen Kommune bis 2026/2027 nicht sichergestellt werden kann (",
          tags$a(
            href = "https://deutscher-schulleitungskongress.de/wp-content/uploads/2023/11/2023-11-21_VOe-Nov_Bericht_Deutschland.pdf",
            "VBE 2023, S. 26",
            target = "_blank"
          ),
          "). Der Ganztagsschulausbau muss als gesamtgesellschaftliche Aufgabe begriffen und die Koperation von Schulen
          und zivilgesellschaftlichen Akteuren an dieser Stelle weiter vorangebracht werden."
        ),
        p(class = "anmerkungen", "Anmerkungen:"),
        uiOutput(ns("Anmerkungen_bildung_ganztag_quantitativ"))
      )
    } else if (indikator_ID == "ganztag_vielfalt"){
      fluidPage(
        p("Das Bildungsangebot in den Ganztagsschulen sollte möglichst vielfältig sein und sollte im Idealfall die folgenden Angebotsbereiche umfassen:"),
        tags$ul(
          tags$li("- lernunterstützende Angebote,"),
          tags$li("- MINT-Angebote,"),
          tags$li("- sprachliche und geisteswissenschaftliche Angebote,"),
          tags$li("- musisch-kulturelle, lebenspraktische und berufsorientierende Angebote sowie"),
          tags$li("- Angebote zu Freizeit, Bewegung, Gesundheit und sozialem Lernen.")
        ),
        br(),
        p(
          "Die nachfolgende Tabelle zeigt den Anteil der Schulen mit entsprechendem Angebot im Ganztag in Prozent. Die Daten stammen aus der ",
          tags$a("Studie zur Entwicklung von Ganztagsschulen (StEG)", href = "https://www.dipf.de/de/forschung/pdf-forschung/llib/bericht-ganztagsschulen-2017-2018", target = "_blank"),
          "."
        ),
        br(),
        fluidRow(
          column(
            width = 3,
            radioButtons(ns("Auswahl_ganztag_vielfalt_schulform"), "Schulform", choices = c("primar", "Sek I (ohne Gymnasium)", "Gymnasium"))
          ),
          column(
            width = 9,
            uiOutput(ns("Tabelle_ganztag_vielfalt"))
          )
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_bildung_ganztag_vielfalt"))
      )
    } else if (indikator_ID == "ganztag_kooperation"){
      fluidPage(
        p("Zur Gestaltung unterrichtsbezogener Angebote kooperieren gemäß Schulleitungsbefragung aus dem Jahr 2023
          der Telekom Stiftung ", tags$b("86 Prozent"), " der allgemeinbildenden öffentlichen Schulen
          mit außerschulischen Lernorten und/oder Bildungspartnern. Schulen kooperieren vor allem mit
          Sportvereinen, Bibliotheken, Musikschulen sowie Einrichtungen der Kinder- und Jugendarbeit."),
        p("Die Kooperation findet dabei im allgemeinen (70 Prozent) auf Basis eines gemeinsam formulierten
          Pädagogischen Konzept statt."),
        br(),
        fluidRow(
          column(
            width = 2,
            radioButtons(
              ns("Auswahl_ganztag_kooperation_schulform"),
              "Schulform",
              choices = c(
                "insgesamt",
                "Grundschule",
                "Haupt-/ Real-/ Gesamtschule",
                "Gymnasium",
                "Förder-/ Sonderschule"
              ),
              selected = "insgesamt"
            )
          ),
          column(
            width = 10,
            plotlyOutput(ns("Grafik_ganztag_kooperation"))
          ),
          p("Anmerkungen:", class = "anmerkungen"),
          uiOutput(ns("Anmerkungen_bildung_ganztag_kooperation"))
        )
      )
    } else if (indikator_ID == "ganztag_multiprofessionell") {
      fluidPage(
        p(
          "Bei der Umsetzung von Ganztag können und sollten multiprofessionelle Teams an Schulen eingesetzt werden. ",
          tags$b("Oftmals arbeiten an Schulen - neben Gebäudemanagement und Sekretariat - jedoch ausschließlich Lehrkräfte."),
          " Unterstützungsstrukturen etwa durch IT-, und Verwaltungsfachkräfte und/oder Sozial-, Medien-, Kulturpädagoginnen
          und -pädagogen sowie Projektmanagement etc. fehlen.",
        ),
        br(),
        p(" Konkrete Zahlen liefert zu diesem Befund eine Schulleitungsbefragung
          zu Multiprofessionalität an allgemeinbildenden Schulen von der Telekom Stiftung, dargestellt in der folgenden Tabelle - Angaben in Prozent:"),
        br(),
        fluidRow(
          column(
            width = 12,
            uiOutput(ns("Tabelle_ganztag_multiprofessionell"))
          )
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_bildung_ganztag_multiprofessionell"))
      )
    } else {
      fluidPage(
        p(
          "Wie bewerten die Schulleitungen ganz allgemein die Lage an den Schulen? Flapsig: Wie ist die Stimmung?
          Dieser Frage nährt sich die jährliche Schulleitungsbefragung des Verband Bildung und Erziehung (VBE)
          u. a. mit den Fragen danach, inwieweit die Schulleitungen gerne Ihren Berauf ausüben und ob sie eine Weiterempfehlung für den Beruf abgeben."
        ),
        fluidRow(
          column(
            width = 3,
            radioButtons(ns("Auswahl_ganztag_lage_frage"), "Auswahl:", choices = c("Arbeitsmotivation der Schulleitungen", "Weiterempfehlung Beruf Schulleitung"))
          ),
          column(
            width = 9,
            plotlyOutput(ns("Grafik_ganztag_lage"))
          )
        ),
        p(
          "Die Ergebnisse: 83 Prozent der Schulleitungen üben ihren Job (sehr) gerne aus - vor der Covid 19-Pandemie waren es noch 96 Prozent.
          Auch die positive Weiterempfehlung des Berufs ist nicht mehr wie vor der Pandemie gegeben, zuletzt haben knapp die Hälfte der Schulleitungen eine Wahrscheinliche oder klare Weiterempfehlung für Ihren Beruf ausgesprochen."
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_bildung_ganztag_lage"))
      )
    }

  #### bildungsgerechtigkeit ----
  } else if (indikator_ID == "bildungsgerechtigkeit_trichter") {
    fluidPage(
      p(
        "Der Bildungstrichter des Stifterverbandes ist ein analytisches Instrument, das die
        Bildungschancen von jungen Menschen mit Akademikereltern und ohne Akademikereltern in
        Deutschland von der Schule bis zum Doktorgrad abbildet. Das Instrument baut auf den Arbeiten
        des ", tags$a("DZHWs", href = "https://www.dzhw.eu/pdf/pub_brief/dzhw_brief_03_2018.pdf", target = "_blank"),
        " auf und zeigt deutlich wie ungerecht Bildungschancen in Deutschland verteilt sind. ", tags$b("Beginnen von 100 Grundschulkindern aus einem Akademikerhaushalt 79 ein Studium.
        Sind es bei Grundschulkindern ohne akademischen Hintergrund gerade einmal 27."), " Diese ungleichheit zieht sich durch die Bildungsstufen bis zur Promotion.
        In den letzten Jahren wurde leicht geringere Differenzen beobachtet - der weite Weg zu mehr Chancengleichheit bleibt jedoch."
      ),
      br(),
      div(
        style = "display: flex; justify-content: center; align-items: center;",
        tags$img(
          src = "img/Bildungstrichter.png",
            height = "550px",
            width = "723px",
            alt = "Der Bildungstrichter des Stifterverbaandes zeigt auf, wie über die verschiedenen Bildungsstufen hinweg Nichtakademikerkinder schlechtere Bildungschancen haben als Akademikerkinder."
        )
      ),
      br(),
      p(
        "Hinsichtlich des Übergangs Grundschule zum Gymnasium liefert zudem eine ",
        tags$a("ifo-Studie", href = "https://www.ifo.de/DocDL/sd-2024-05-ungleiche-bildungschancen-woessmann-etal-.pdf", targe = "_blank"),
        " Auswertungen - auch auf Bundeslandsebene. Aufgrund methodischer Unterschiede lassen sich die Daten ", tags$b("nicht"), "direkt bei dem Bildungstrichter einsortieren,
        bieten aber nochmals eine weitere Perspektive. Die Grafik zeigt die Wahrscheinlichkeit, mit der Kinder aus benachteiligten Verhältnissen ein Gymnasium besuchen gegenüber Kindern aus günstigen Verhältnissen:"
      ),
      br(),
      plotOutput(ns("Grafik_bildungsgerechtigkeit_gymnasium_bundeslaender"), height = "1000px", width = "800px"),
      p("Anmerkungen:", class = "anmerkungen"),
      uiOutput(ns("Anmerkungen_bildungsgerichtigkeit_trichter"))
    )

  #### berufsorientierung ----
  } else if (startsWith(indikator_ID, "berufsorientierung")) {
    if (indikator_ID == "berufsorientierung_subjektiv") {
      fluidPage(
        uiOutput(ns("UI_berufsorientierung_subjektiv"))
      )
    } else {
      fluidPage(
        uiOutput(ns("UI_berufsorientierung_praktika"))
      )
    }

  # MINT ----

  #### Minternational

  } else if (startsWith(indikator_ID, "minternational")){
    if (indikator_ID == "minternational_studierende") {
      fluidPage(
        p("")
      )
    }
  }
}
