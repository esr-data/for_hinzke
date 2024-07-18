box::use(
  ../../R/utils/routing[get_hf_param, add_param_in_url, recode_parameter],
  ../../R/utils/database[get_query, load_table_by_variable, capture_tbl],
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
    renderPlot,
    tabsetPanel,
    tabPanel,
    htmlOutput
  ],
  shiny.router[get_query_param, get_page, change_page],
  shinyWidgets[
    pickerInput,       updatePickerInput,
    radioGroupButtons, updateRadioGroupButtons,
    sliderTextInput, pickerOptions
  ],
  plotly[
    plotlyOutput, renderPlotly, plotly
  ],
  magrittr[`%>%`],
  shinycssloaders[withSpinner],
  dplyr[rename, filter, select, case_when, across, mutate,
        arrange, show_query],
  purrr[map2],
  DT[datatable, JS, renderDataTable, dataTableOutput, renderDT],
  shinyjs[
    removeCssClass,
    addCssClass
  ],
  highcharter[renderHighchart, hchart, hcaes, hc_tooltip, hc_size, 
              hc_yAxis, hc_xAxis, hc_plotOptions, hc_colors, hc_title, 
              hc_chart, hc_legend,  highchartOutput],
  tidyr[pivot_longer, pivot_wider],
  stringr[str_ends, str_detect],
  forcats[fct_rev],
  stats[na.omit],
  utils[capture.output]
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
                 
               #### minternational ----
                 
                 #1 übersicht
                   output$slider_minternational_jahr <- renderUI({
                     sliderTextInput(
                       inputId = ns("date_studium_studienzahl_ausl"),
                       label = "Jahr",
                       choices = 2012:2022,
                       selected = 2022
                     )
                   })
                 
                   output$pickerInput_minternational_region<- renderUI({
                     pickerInput(
                       inputId = ns("region_minternational_übersicht"),
                       label = "Region",
                       choices = c("Deutschland",
                                   "Westdeutschland (o. Berlin)",
                                   "Ostdeutschland (inkl. Berlin)",
                                   "Baden-Württemberg",
                                   "Bayern",
                                   "Berlin",
                                   "Brandenburg",
                                   "Bremen",
                                   "Hamburg",
                                   "Hessen",
                                   "Mecklenburg-Vorpommern",
                                   "Niedersachsen",
                                   "Nordrhein-Westfalen",
                                   "Rheinland-Pfalz",
                                   "Saarland",
                                   "Sachsen",
                                   "Sachsen-Anhalt",
                                   "Schleswig-Holstein",
                                   "Thüringen"),
                       selected = "Deutschland"
                     )
                   })
                 
                 r <- reactiveValues()
                 
                 observeEvent(input$region_minternational_übersicht, {
                   r$region_minternational_übersicht <- input$region_minternational_übersicht
                 })
                 
                 observeEvent(input$date_studium_studienzahl_ausl, {
                   r$date_studium_studienzahl_ausl <- input$date_studium_studienzahl_ausl
                 })
                 
                 observeEvent(input$status_ausl, {
                   r$status_ausl <- input$status_ausl
                 })
                 
                 observeEvent(input$abs_zahlen_studium_studienzahl_ausl, {
                   r$abs_zahlen_studium_studienzahl_ausl <- input$abs_zahlen_studium_studienzahl_ausl
                 })
                 
                 observeEvent(input$ebene_ausl, {
                   r$ebene_ausl <- input$ebene_ausl
                 })
                 
                output$plot_minternational_uebersicht <- renderHighchart({
                
                  bl_select <- r$region_minternational_übersicht
                   year_select <- r$date_studium_studienzahl_ausl
                  absolut_selector <- r$abs_zahlen_studium_studienzahl_ausl
                  status_select <- r$status_ausl
                  betr_ebene <- r$ebene_ausl
                  
                
                  df_query <- capture.output(capture_tbl("studierende_detailliert") %>%
                    filter(indikator %in% c("internationale Studienanfänger:innen (1. Hochschulsemester)",
                                                   "internationale Studierende",
                                                   "Studienanfänger:innen (1. Hochschulsemester)",
                                                   "Studierende"),
                                  geschlecht == "Gesamt",
                                 # region == input$region_minternational_übersicht,
                                  region == bl_select,
                                  jahr == year_select )%>%
                    select(-mint_select,- fachbereich)%>%
                    show_query())
                  
                   df_query <- paste(df_query[-1], collapse = " ")
                   df <- get_query(df_query)
                  
                    df <- df %>% pivot_wider(names_from=indikator, values_from = wert) %>%
                    mutate("deutsche Studierende" =`Studierende` - `internationale Studierende`,
                                  "deutsche Studienanfänger:innen (1. Hochschulsemester)"=`Studienanfänger:innen (1. Hochschulsemester)`-
                                    `internationale Studienanfänger:innen (1. Hochschulsemester)`)%>%
                    mutate("deutsche Studierende_p" =`deutsche Studierende`/`Studierende`,
                                  "internationale Studierende_p"= `internationale Studierende`/`Studierende`,
                                  "deutsche Studienanfänger:innen (1. Hochschulsemester)_p" =`deutsche Studienanfänger:innen (1. Hochschulsemester)`/`Studienanfänger:innen (1. Hochschulsemester)`,
                                  "internationale Studienanfänger:innen (1. Hochschulsemester)_p"=`internationale Studienanfänger:innen (1. Hochschulsemester)`/`Studienanfänger:innen (1. Hochschulsemester)`)%>%
                    select(-c(Studierende, `Studienanfänger:innen (1. Hochschulsemester)` ))%>%
                    pivot_longer(c(7:ncol(.)), names_to="indikator", values_to="wert")%>%
                    mutate(selector=case_when(str_ends(.$indikator, "_p")~"Relativ",
                                                            T~"Asolut"))%>%
                    mutate(selector=case_when(str_ends(.$indikator, "_p") ~ "In Prozent",
                                                            T ~ "Anzahl"))%>%
                    mutate(ausl_detect=case_when(str_detect(.$indikator, "international")~"International",
                                                               T~ "Deutsch"))%>%
                    filter(!fach %in% c("Weitere ingenieurwissenschaftliche Fächer",
                                               "Weitere naturwissenschaftliche und mathematische Fächer",
                                               "Außerhalb der Studienbereichsgliederung/Sonstige Fächer"))
                  
                  
                  df$indikator <- gsub("_p", "", df$indikator)
                  
                  df$indikator <- gsub("deutsche ", "", df$indikator)
                  
                  df$indikator <- gsub("internationale ", "", df$indikator)
                  
                  #df$fach <- gsub("Nicht_MINT", "Nicht MINT", df$fach)
                  
                  
                  df$ausl_detect  <- factor(df$ausl_detect, levels=c("Deutsch", "International"))
                  
                  df <- df %>%
                    filter(indikator == status_select)
                  
                  df_fachbereich <- df %>%
                    filter(fach %in% c("Geisteswissenschaften",
                                              "Mathematik, Naturwissenschaften",
                                              "Rechts-, Wirtschafts- und Sozialwissenschaften",
                                              "Humanmedizin/Gesundheitswissenschaften",
                                              "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                                              "Sport",
                                              "Kunst, Kunstwissenschaft",
                                              "Alle Fächer",
                                              "Alle MINT-Fächer",
                                              "Alle Nicht MINT-Fächer",
                                              "Ingenieurwissenschaften (inkl. Informatik)"))
                  
                  
                  df_faecher <- df %>%
                    filter(!fach %in% c("Geisteswissenschaften",
                                               "Rechts-, Wirtschafts- und Sozialwissenschaften",
                                               "Humanmedizin/Gesundheitswissenschaften",
                                               "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                                               "Sport",
                                               "Kunst, Kunstwissenschaft",
                                               "Ingenieurwissenschaften ohne Informatik",
                                               "Alle Nicht MINT-Fächer",
                                               "Alle MINT-Fächer",
                                               "Alle Fächer",
                                               "Mathematik, Naturwissenschaften",
                                               "Ingenieurwissenschaften (inkl. Informatik)"))
                  df_faecher$fach <- as.factor(df_faecher$fach)
                  
                  #Faktor für Höhe des Grafens berechnen
                  ebene <- c("Fachbereiche", "MINT-Fächer")
                  höhe <- c(8, 11)
                  plt.add <- data.frame(ebene, höhe)
                  
                  # NA aus fach entfernen für BULAs mit weniger Studienfachgruppen
                  df_fachbereich <-na.omit(df_fachbereich)
                  df_fachbereich <- df_fachbereich %>%
                    arrange(wert)
                  df_faecher <- na.omit(df_faecher)
                  df_faecher <- df_faecher %>%
                    arrange(wert)
                  
                  
                  # Vorbereitung Überschrift
                  help <- "Studierender"
                  help <- ifelse(grepl("anfänger", status_select), "Studienanfänger:innen", help)
                  
                  help2 <- "Studierenden"
                  help2 <- ifelse(grepl("anfänger", status_select), "Studienanfänger:innen", help2)
                  
                  
                  if(betr_ebene=="Fachbereiche"){
                    
                    if(absolut_selector=="In Prozent"){
                      
                      df_fachbereich <- df_fachbereich %>%
                        filter(selector == "In Prozent")%>%
                        mutate(wert = round(wert*100, 1))
                      
                      df_fachbereich$display_rel <- prettyNum(df_fachbereich$wert, big.mark = ".", decimal.mark = ",")
    
                      
                      out <- hchart(df_fachbereich, 'bar', hcaes(y = wert, x = fach, group = ausl_detect))%>%
                        hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anteil: {point.display_rel} %")%>%
                        hc_size(height = 60*plt.add$höhe[plt.add$ebene == betr_ebene])%>%
                        hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
                        hc_xAxis(title = list(text = ""), categories = c("Alle Fächer",
                                                                                      "Alle MINT-Fächer",
                                                                                      "Alle Nicht MINT-Fächer",
                                                                                      "",
                                                                                      "Ingenieurwissenschaften (inkl. Informatik)",
                                                                                      "Kunst, Kunstwissenschaft",
                                                                                      "Mathematik, Naturwissenschaften",
                                                                                      "Geisteswissenschaften",
                                                                                      "Rechts-, Wirtschafts- und Sozialwissenschaften",
                                                                                      "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                                                                                      "Humanmedizin/Gesundheitswissenschaften",
                                                                                      "Sport")) %>%
                        hc_plotOptions(bar = list(stacking = "percent")) %>%
                        hc_colors(c("#ADA58B", "#195365")) %>%
                        hc_title(text = paste0("Anteil internationaler ", help, " an allen ", help2, " in ", bl_select,  " (",year_select, ")" ),
                                              margin = 45,
                                              align = "center",
                                              style = list(color = "black", useHTML = TRUE, fontFamily = "Arial", fontSize = "18px")) %>%
                        hc_chart(
                          style = list(fontFamily = "Arial", fontSize = "16px")
                        ) %>%
                        hc_legend(enabled = TRUE, reversed = T)
                        
                      
                      
                    } else if(absolut_selector =="Anzahl"){
                      
                      hcoptslang <- getOption("highcharter.lang")
                      hcoptslang$thousandsSep <- "."
                      options(highcharter.lang = hcoptslang)
                      
                      df_fachbereich$display_abs <- prettyNum(df_fachbereich$wert, big.mark = ".", decimal.mark = ",")
                      
                      df_fachbereich <- df_fachbereich %>%
                        filter(selector == "Anzahl")
                      
                      
                      
                      
                      out <- hchart(df_fachbereich, 'bar', hcaes(y = wert, x = fach, group = ausl_detect))%>%
                        hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anzahl: {point.display_abs}")%>%
                        hc_size(height = 60*plt.add$höhe[plt.add$ebene == betr_ebene])%>%
                        hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}")) %>%
                        hc_xAxis(title = list(text = ""), categories = c("Alle Fächer",
                                                                                      "Alle MINT-Fächer",
                                                                                      "Alle Nicht MINT-Fächer",
                                                                                      "",
                                                                                      "Ingenieurwissenschaften (inkl. Informatik)",
                                                                                      "Rechts-, Wirtschafts- und Sozialwissenschaften",
                                                                                      "Mathematik, Naturwissenschaften",
                                                                                      "Geisteswissenschaften",
                                                                                      "Kunst, Kunstwissenschaft",
                                                                                      "Humanmedizin/Gesundheitswissenschaften",
                                                                                      "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                                                                                      "Sport")) %>%
                        #hc_plotOptions(bar = list(stacking = "percent")) %>%
                        hc_colors(c("#ADA58B", "#195365")) %>%
                        hc_title(text = paste0("Anzahl internationaler ", help, " in ", bl_select,  " (",year_select, ")" ),
                                              margin = 45,
                                              align = "center",
                                              style = list(color = "black", useHTML = TRUE, fontFamily = "Arial", fontSize = "18px")) %>%
                        hc_chart(
                          style = list(fontFamily = "Arial", fontSize = "16px")
                        ) %>%
                        hc_legend(enabled = TRUE, reversed = T)
                      
                    }} else if(betr_ebene == "MINT-Fächer"){
                      
                      if(absolut_selector=="In Prozent"){
                        
                        
                        df_faecher <- df_faecher %>%
                          filter(selector == "In Prozent")%>%
                          mutate(wert = round(wert*100, 1)) %>%
                          arrange(wert)
                        
                        df_faecher$display_rel <- prettyNum(df_faecher$wert, big.mark = ".", decimal.mark = ",")
                        
                        out <- hchart(df_faecher, 'bar', hcaes(y = wert, x = fach, group = ausl_detect))%>%
                          hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anteil: {point.display_rel} %")%>%
                          hc_size(height = 60*plt.add$höhe[plt.add$ebene == betr_ebene])%>%
                          hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
                          hc_xAxis(title = list(text = "")
                          ) %>%
                          hc_plotOptions(bar = list(stacking = "percent"))%>%
                          hc_colors(c("#ADA58B", "#195365")) %>%
                          
                          hc_title(text = paste0("Anteil internationaler ", help, " an allen ", help2, " in ", bl_select,  " (",year_select, ")" ),
                                                margin = 45,
                                                align = "center",
                                                style = list(color = "black", useHTML = TRUE, fontFamily = "Arial", fontSize = "18px")) %>%
                          hc_chart(
                            style = list(fontFamily = "Arial", fontSize = "16px")
                          ) %>%
                          hc_legend(enabled = TRUE, reversed = T) 
                        
                        
                        
                      } else if(absolut_selector =="Anzahl"){
                        
                        hcoptslang <- getOption("highcharter.lang")
                        hcoptslang$thousandsSep <- "."
                        options(highcharter.lang = hcoptslang)
                        
                        df_faecher <- df_faecher %>%
                          filter(selector == "Anzahl") %>%
                          arrange(wert)
                        
                        df_faecher$display_abs <- prettyNum(df_faecher$wert, big.mark = ".", decimal.mark = ",")
                        
                        df_faecher <- df_faecher[order(-df_faecher$wert),]
                        
                        out <- hchart(df_faecher, 'bar', hcaes(y = wert, x = fach, group = ausl_detect))%>%
                          hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anzahl: {point.display_abs}")%>%
                          hc_size(height = 60*plt.add$höhe[plt.add$ebene == betr_ebene])%>%
                          hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}")) %>%
                          hc_xAxis(title = list(text = "")) %>%
                          #hc_plotOptions(bar = list(stacking = "percent")) %>%
                          hc_colors(c("#ADA58B", "#195365")) %>%
                          hc_title(text = paste0("Anzahl internationaler ", help, " in ", bl_select,  " (",year_select, ")" ),
                                                margin = 45,
                                                align = "center",
                                                style = list(color = "black", useHTML = TRUE, fontFamily = "Arial", fontSize = "18px")) %>%
                          hc_chart(
                            style = list(fontFamily = "Arial", fontSize = "16px")
                          ) %>%
                          hc_legend(enabled = TRUE, reversed = T)
                      }}
                  out
                  
                  })
                
                #2 Zeitverlauf
                
                output$pickerInput_minternational_zeitverlauf<- renderUI({
                  fluidRow(
                    column(width = 11,
                    pickerInput(
                      inputId = ns("states_studium_studienzahl_ausl_zeit"),
                      label = "Region",
                      choices = c("Deutschland",
                                  "Baden-Württemberg",
                                  "Bayern",
                                  "Berlin",
                                  "Brandenburg",
                                  "Bremen",
                                  "Hamburg",
                                  "Hessen",
                                  "Mecklenburg-Vorpommern",
                                  "Niedersachsen",
                                  "Nordrhein-Westfalen",
                                  "Rheinland-Pfalz",
                                  "Saarland",
                                  "Sachsen",
                                  "Sachsen-Anhalt",
                                  "Schleswig-Holstein",
                                  "Thüringen",
                                  "Westdeutschland (o. Berlin)",
                                  "Ostdeutschland (inkl. Berlin)"),
                      selected = "Deutschland"
                    ),
                    
                    #Conditonal Panel, dass keine leeren Plots kommen
                    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Deutschland' |
                                                   input.states_studium_studienzahl_ausl_zeit == 'Baden-Württemberg' |
                                                   input.states_studium_studienzahl_ausl_zeit == 'Bayern' |
                                                   input.states_studium_studienzahl_ausl_zeit == 'Berlin' |
                                                   input.states_studium_studienzahl_ausl_zeit == 'Hamburg' |
                                                   input.states_studium_studienzahl_ausl_zeit == 'Hessen' |
                                                   input.states_studium_studienzahl_ausl_zeit == 'Nordrhein-Westfalen' |
                                                  input.states_studium_studienzahl_ausl_zeit == 'Rheinland-Pfalz' |
                                                  input.states_studium_studienzahl_ausl_zeit == 'Sachsen' |
                                                  input.states_studium_studienzahl_ausl_zeit == 'Westdeutschland (o. Berlin)' |
                                                  input.states_studium_studienzahl_ausl_zeit == 'Ostdeutschland (inkl. Berlin)'",
                                     ns = ns,
                                     shinyWidgets::pickerInput(
                                       inputId = ns("fach1_studium_studienzahl_ausl_zeit"),
                                       label = "Fachbereich",
                                       choices = studi_det_ui_faecher(spezif_r=c('Deutschland','Baden-Württemberg','Bayern','Berlin',
                                                                                 'Hamburg', 'Hessen','Nordrhein-Westfalen', 'Rheinland-Pfalz',
                                                                                 'Sachsen', 'Westdeutschland (o. Berlin)', 'Ostdeutschland (inkl. Berlin)' )),
                                       selected = "Alle MINT-Fächer",
                                       multiple = FALSE
                                     )),
                    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Brandenburg'",
                                     ns = ns,
                                     shinyWidgets::pickerInput(
                                       inputId = ns("fach2_studium_studienzahl_ausl_zeit"),
                                       label = "Fachbereich",
                                       choices = studi_det_ui_faecher(spezif_r='Brandenburg'),
                                       selected = "Alle MINT-Fächer",
                                       multiple = FALSE
                                     )),
                    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Bremen'",
                                     ns = ns,
                                     shinyWidgets::pickerInput(
                                       inputId = ns("fach3_studium_studienzahl_ausl_zeit"),
                                       label = "Fachbereich",
                                       choices = studi_det_ui_faecher(spezif_r='Bremen'),
                                       selected = "Alle MINT-Fächer",
                                       multiple = FALSE
                                     )),
                    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Mecklenburg-Vorpommern'",
                                     ns = ns,
                                     shinyWidgets::pickerInput(
                                       inputId = ns("fach4_studium_studienzahl_ausl_zeit"),
                                       label = "Fachbereich",
                                       choices = studi_det_ui_faecher(spezif_r='Mecklenburg-Vorpommern'),
                                       selected = "Alle MINT-Fächer",
                                       multiple = FALSE
                                     )),
                    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Niedersachsen'",
                                     ns = ns,
                                     shinyWidgets::pickerInput(
                                       inputId = ns("fach5_studium_studienzahl_ausl_zeit"),
                                       label = "Fachbereich",
                                       choices = studi_det_ui_faecher(spezif_r='Niedersachsen'),
                                       selected = "Alle MINT-Fächer",
                                       multiple = FALSE
                                     )),
                    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Saarland'",
                                     ns = ns,
                                     shinyWidgets::pickerInput(
                                       inputId = ns("fach6_studium_studienzahl_ausl_zeit"),
                                       label = "Fachbereich",
                                       choices = studi_det_ui_faecher(spezif_r='Saarland'),
                                       selected = "Alle MINT-Fächer",
                                       multiple = FALSE
                                     )),
                    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Sachsen-Anhalt'",
                                     ns = ns,
                                     shinyWidgets::pickerInput(
                                       inputId = ns("fach7_studium_studienzahl_ausl_zeit"),
                                       label = "Fachbereich",
                                       choices = studi_det_ui_faecher(spezif_r='Sachsen-Anhalt'),
                                       selected = "Alle MINT-Fächer",
                                       multiple = FALSE
                                     )),
                    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Schleswig-Holstein'",
                                     ns = ns,
                                     shinyWidgets::pickerInput(
                                       inputId = ns("fach8_studium_studienzahl_ausl_zeit"),
                                       label = "Fachbereich",
                                       choices = studi_det_ui_faecher(spezif_r='Schleswig-Holstein'),
                                       selected = "Alle MINT-Fächer",
                                       multiple = FALSE
                                     )),
                    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Thüringen'",
                                     ns = ns,
                                     shinyWidgets::pickerInput(
                                       inputId = ns("fach9_studium_studienzahl_ausl_zeit"),
                                       label = "Fachbereich",
                                       choices =studi_det_ui_faecher(spezif_r='Thüringen'),
                                       selected = "Alle MINT-Fächer",
                                       multiple = FALSE
                                     ))
                    )
                  ) 
                   
                })
                    
                observeEvent(input$states_studium_studienzahl_ausl_zeit, {
                  r$states_studium_studienzahl_ausl_zeit <- input$states_studium_studienzahl_ausl_zeit
                })
                
                observeEvent(input$status_ausl_zeit, {
                  r$status_ausl_zeit <- input$status_ausl_zeit
                })
                
                observeEvent(input$fach1_studium_studienzahl_ausl_zeit, {
                  r$fach1_studium_studienzahl_ausl_zeit <- input$fach1_studium_studienzahl_ausl_zeit
                })
                
                observeEvent(input$fach2_studium_studienzahl_ausl_zeit, {
                  r$fach2_studium_studienzahl_ausl_zeit <- input$fach2_studium_studienzahl_ausl_zeit
                })
                observeEvent(input$fach3_studium_studienzahl_ausl_zeit, {
                  r$fach3_studium_studienzahl_ausl_zeit <- input$fach3_studium_studienzahl_ausl_zeit
                })
                
                observeEvent(input$fach4_studium_studienzahl_ausl_zeit, {
                  r$fach4_studium_studienzahl_ausl_zeit <- input$fach4_studium_studienzahl_ausl_zeit
                })
                observeEvent(input$fach5_studium_studienzahl_ausl_zeit, {
                  r$fach5_studium_studienzahl_ausl_zeit <- input$fach5_studium_studienzahl_ausl_zeit
                })
                
                observeEvent(input$fach6_studium_studienzahl_ausl_zeit, {
                  r$fach6_studium_studienzahl_ausl_zeit <- input$fach6_studium_studienzahl_ausl_zeit
                })
                observeEvent(input$fach7_studium_studienzahl_ausl_zeit, {
                  r$fach7_studium_studienzahl_ausl_zeit <- input$fach7_studium_studienzahl_ausl_zeit
                })
                
                observeEvent(input$fach8_studium_studienzahl_ausl_zeit, {
                  r$fach8_studium_studienzahl_ausl_zeit <- input$fach8_studium_studienzahl_ausl_zeit
                })
                
                observeEvent(input$fach9_studium_studienzahl_ausl_zeit, {
                  r$fach9_studium_studienzahl_ausl_zeit <- input$fach9_studium_studienzahl_ausl_zeit
                })
                
                observeEvent(input$abs_zahlen_studium_studienzahl_ausl_zeit, {
                  r$abs_zahlen_studium_studienzahl_ausl_zeit <- input$abs_zahlen_studium_studienzahl_ausl_zeit
                })
                
                output$plot_minternational_zeitverlauf <- renderHighchart({
           
                  bl_select <- r$states_studium_studienzahl_ausl_zeit
                  absolut_selector <- r$abs_zahlen_studium_studienzahl_ausl_zeit
                  status_select <- r$status_ausl_zeit
                  
                  if(bl_select %in% c("Deutschland",
                                      "Baden-Württemberg",
                                      "Bayern",
                                      "Berlin",
                                      "Hamburg",
                                      "Hessen",
                                      "Nordrhein-Westfalen",
                                      "Rheinland-Pfalz",
                                      "Sachsen",
                                      "Westdeutschland (o. Berlin)",
                                      "Ostdeutschland (inkl. Berlin)")) {
                    fach_select <- r$fach1_studium_studienzahl_ausl_zeit
                  }
                  else {
                    if(bl_select == "Brandenburg")fach_select <- r$fach2_studium_studienzahl_ausl_zeit
                    if(bl_select == "Bremen")fach_select <- r$fach3_studium_studienzahl_ausl_zeit
                    if(bl_select == "Mecklenburg-Vorpommern")fach_select <- r$fach4_studium_studienzahl_ausl_zeit
                    if(bl_select == "Niedersachsen")fach_select <- r$fach5_studium_studienzahl_ausl_zeit
                    if(bl_select == "Saarland")fach_select <- r$fach6_studium_studienzahl_ausl_zeit
                    if(bl_select == "Sachsen-Anhalt")fach_select <- r$fach7_studium_studienzahl_ausl_zeit
                    if(bl_select == "Schleswig-Holstein")fach_select <- r$fach8_studium_studienzahl_ausl_zeit
                    if(bl_select == "Thüringen")fach_select <- r$fach9_studium_studienzahl_ausl_zeit
                  }
                  
                  #bl_select <- "Deutschland"
                  # absolut_selector <- "Anzahl"
                  # status_select <- "Studierende"
                  # fach_select <- "Alle MINT-Fächer"
                   
                
                  df_query <- capture.output(capture_tbl("studierende_detailliert") %>%
                    filter(indikator %in% c("internationale Studienanfänger:innen (1. Hochschulsemester)",
                                                   "internationale Studierende",
                                                   "Studienanfänger:innen (1. Hochschulsemester)",
                                                   "Studierende"),
                                  geschlecht == "Gesamt")%>%
                    select(-mint_select,- fachbereich)%>%
                    show_query())
                
                df_query <- paste(df_query[-1], collapse = " ")
                df <- get_query(df_query)
                
                df <- df %>% 
                   pivot_wider(names_from=indikator, values_from = wert)%>%
                    mutate("deutsche Studierende" =`Studierende`-`internationale Studierende`,
                                  "deutsche Studienanfänger:innen (1. Hochschulsemester)"=`Studienanfänger:innen (1. Hochschulsemester)`-
                                    `internationale Studienanfänger:innen (1. Hochschulsemester)`)%>%
                    mutate("deutsche Studierende_p" =`deutsche Studierende`/Studierende,
                                  "internationale Studierende_p"= `internationale Studierende`/`Studierende`,
                                  "deutsche Studienanfänger:innen (1. Hochschulsemester)_p" =`deutsche Studienanfänger:innen (1. Hochschulsemester)`/`Studienanfänger:innen (1. Hochschulsemester)`,
                                  "internationale Studienanfänger:innen (1. Hochschulsemester)_p"=`internationale Studienanfänger:innen (1. Hochschulsemester)`/`Studienanfänger:innen (1. Hochschulsemester)`)%>%
                    select(-c(`Studierende`, `Studienanfänger:innen (1. Hochschulsemester)` ))%>%
                    #  filter(geschlecht=="Gesamt")%>%
                    pivot_longer(c(7:ncol(.)), names_to="indikator", values_to="wert")%>%
                    mutate(selector=case_when(str_ends(.$indikator, "_p")~"Relativ",
                                                            T~"Absolut"))%>%
                    mutate(selector=case_when(str_ends(.$indikator, "_p") ~ "In Prozent",
                                                            T ~ "Anzahl"))%>%
                    mutate(ausl_detect=case_when(str_detect(.$indikator, "international")~"international",
                                                               T~ "deutsch"))
                  
                  df$indikator <- gsub("_p", "", df$indikator)
                  
                  df$indikator <- gsub("deutsche ", "", df$indikator)
                  
                  df$indikator <- gsub("internationale ", "", df$indikator)
                  
                  #df$fach <- gsub("Nicht_MINT", "Nicht MINT", df$fach)
                  
                  
                  df$ausl_detect  <- factor(df$ausl_detect, levels=c("deutsch", "international"))
                  
                  df <- df %>%
                    filter(region == bl_select,
                                  fach ==fach_select,
                                  indikator==status_select)
                  
                  
                  # Vorbereitung Überschrift
                  help <- "Studierender"
                  help <- ifelse(grepl("anfänger", status_select), "Studienanfänger:innen", help)
                  
                  help2 <- "Studierenden"
                  help2 <- ifelse(grepl("anfänger", status_select), "Studienanfänger:innen", help2)
                  
                  fach_help <- fach_select
                  fach_help <- ifelse(fach_help == "Alle MINT-Fächer", "MINT", fach_help)
                  
                  # Plot
                  
                  if(absolut_selector=="In Prozent"){
                    
                    df <- df %>%
                      filter(selector == absolut_selector)%>%
                      mutate(across(wert, ~round(.*100, 1)))
                    
                    df$display_rel <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
                    
                    
                    
                    
                    hchart(df, 'column', hcaes(y = wert, x = jahr, group = ausl_detect))%>%
                      hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anteil: {point.display_rel} %")%>%
                      # hc_size(height = 1000)%>%
                      hc_yAxis(title = list(text = "")
                                            , labels = list(format = "{value} %")
                      ) %>%
                      hc_xAxis(title = list(text = "")) %>%
                      hc_plotOptions(column = list(stacking = "percent")) %>%
                     # hc_plotOptions(column = list(pointWidth = 70))%>%
                      hc_colors(c("#ADA58B", "#195365")) %>%
                      hc_yAxis(max = 40)%>%
                      hc_title(text = paste0("Anteil internationaler ", help, " an allen ", help2, " in ", fach_help , " in ", bl_select ),
                                            align = "center",
                                            style = list(color = "black", useHTML = TRUE, fontFamily = "Arial", fontSize = "18px")) %>%
                      hc_chart(
                        style = list(fontFamily = "Arial", fontSize = "16px")
                      ) %>%
                     hc_legend(enabled = TRUE, reversed = T) 
                    # %>%
                    #   hc_exporting(enabled = FALSE,
                    #                             buttons = list(contextButton = list(
                    #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                    #                               onclick = JS("function () {
                    #                                         this.exportChart({ type: 'image/png' }); }"),
                    #                               align = 'right',
                    #                               verticalAlign = 'bottom',
                    #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
                    
                    
                  } else if(absolut_selector=="Anzahl"){
                    
                    df <- df %>%
                      filter(selector == absolut_selector)
                    
                    hcoptslang <- getOption("highcharter.lang")
                    hcoptslang$thousandsSep <- "."
                    options(highcharter.lang = hcoptslang)
                    
                    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
                    
                    
                    
                    
                    hchart(df, 'column', hcaes(y = wert, x = jahr, group = ausl_detect))%>%
                      hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anzahl: {point.display_abs}")%>%
                      hc_yAxis(title = list(text = "")
                                            , labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "BrandonTextWeb-Regular")
                      ) %>%
                      hc_xAxis(title = list(text = "")) %>%
                      hc_colors(c("#ADA58B", "#195365")) %>%
                      hc_title(text =  paste0("Anzahl internationaler ", help, " in ", fach_help, " in ", bl_select),
                                            margin = 45,
                                            align = "center",
                                            style = list(color = "black", useHTML = TRUE, fontFamily = "Arial", fontSize = "18px")) %>%
                      hc_chart(
                        style = list(fontFamily = "Arial", fontSize = "16px")
                      ) %>%
                      hc_legend(enabled = TRUE, reversed = T) 
                    # %>%
                      # hc_exporting(enabled = FALSE,
                      #                           buttons = list(contextButton = list(
                      #                             symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                      #                             onclick = JS("function () {
                      #                                       this.exportChart({ type: 'image/png' }); }"),
                      #                             align = 'right',
                      #                             verticalAlign = 'bottom',
                      #                             theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
                  }
                  
                  
                })
                  
                 
                 
               #### mint lehramtstudium ----
                
                output$slider_input_fs_mint_jahr <- renderUI({
                  
                  sliderTextInput(
                    inputId = ns("mint_lehramt_uebersicht_time"),
                    label = "Jahr",
                    choices = 2013:2022,
                    selected = 2022
                  )
                })
                 
                 observeEvent(input$mint_lehramt_uebersicht_time, {
                   r$mint_lehramt_uebersicht_time <- input$mint_lehramt_uebersicht_time
                 })
                 
                 #1 übersicht
                
                 output$plot_lehrkraft_mint <- renderHighchart({
                  
                    timerange <- r$mint_lehramt_uebersicht_time
                   #timerange <- 2022
                  # timerange <- r$date_kurse_einstieg_comparison
                   
                   # filter dataset based on UI inputs
                   df_query <- capture.output(capture_tbl("studierende") %>%
                    filter(jahr == timerange,
                                   geschlecht == "Gesamt",
                                   region== "Deutschland")%>%
                     show_query())
                 
                 df_query <- paste(df_query[-1], collapse = " ")
                 df <- get_query(df_query)
                 
                   df <- df %>%
                     pivot_wider(names_from=fachbereich, values_from = wert)%>%
                     #dplyr::rename("MINT (gesamt)" = MINT)%>%
                    select( -region, -Ingenieurwissenschaften,- `Mathematik, Naturwissenschaften`)
                   
                   # Calculating props
                   
                   df_props <- df %>%
                     dplyr::mutate(dplyr::across(c("MINT (Gesamt)", "Nicht MINT"), ~round(./Alle * 100,1)))%>%
                     dplyr::select(-Alle)%>%
                     tidyr::pivot_longer(c("MINT (Gesamt)", "Nicht MINT"), values_to="prop", names_to = "proportion")
                   
                   # joining props and wert
                   df <- df%>%
                     dplyr::select(-Alle )%>%
                     tidyr::pivot_longer(c("MINT (Gesamt)", "Nicht MINT"), values_to="wert", names_to = "proportion")%>%
                     dplyr::left_join(df_props)
                   
                   
                   #Trennpunkte für lange Zahlen ergänzen
                   
                   df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
                   df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
                   
  
                   df$indikator <-factor(df$indikator,levels= c("Studierende",
                                                                "Studierende (Fachhochschulen)",
                                                                "Studierende (Lehramt, Universität)",
                                                                "Studierende (Universität)",
                                                                "Studienanfänger:innen (1.Fachsemester)",
                                                                "Studienanfänger:innen (1.Hochschulsemester)",
                                                                "Studienanfänger:innen (Fachhochschulen, 1.Fachsemester)",
                                                                "Studienanfänger:innen (Fachhochschulen, 1.Hochschulsemester)",
                                                                "Studienanfänger:innen (Lehramt, Universität, 1.Fachsemester)",
                                                                "Studienanfänger:innen (Lehramt, Universität, 1.Hochschulsemester)",
                                                                "Studienanfänger:innen (Universität, 1.Fachsemester)",
                                                                "Studienanfänger:innen (Universität, 1.Hochschulsemester)"
                   )
                   )
                   df <- within(df, proportion <- factor(proportion, levels=c("Nicht MINT", "MINT (Gesamt)")))
                   
                   hchart(df, 'bar', hcaes(y = prop, x = indikator, group = forcats::fct_rev(proportion)))%>%
                     hc_tooltip(pointFormat = "Fachbereich: {point.proportion} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}") %>%
                     hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  F) %>%
                     hc_xAxis(title = list(text = "")) %>%
                     hc_plotOptions(bar = list(stacking = "percent")) %>%
                     hc_colors(c("#195365", "#ADA58B")) %>%
                     hc_title(text = paste0("Anteil von Studierenden in MINT an allen Studierenden", "(", timerange, ")"),
                                           margin = 45,
                                           align = "center",
                                           style = list(color = "black", useHTML = TRUE, fontFamily = "Arial", fontSize = "18px")) %>%
                     hc_chart(
                       style = list(fontFamily = "Arial", fontSize = "16px")
                     ) %>%
                     hc_legend(enabled = TRUE, reversed = F)
                   # %>%
                   #   hc_exporting(enabled = FALSE,
                   #                             buttons = list(contextButton = list(
                   #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                   #                               onclick = JS("function () {
                   #                                            this.exportChart({ type: 'image/png' }); }"),
                   #                               align = 'right',
                   #                               verticalAlign = 'bottom',
                   #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
                   
                   })
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

studi_det_ui_faecher <-function(spezif_i, spezif_r){
  
  if(missing(spezif_i) & missing(spezif_r)){
    
    df_query <- capture.output(capture_tbl("studierende_detailliert") %>%
      filter(mint_select == "MINT"  | fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer")) %>%
      select(fach)%>%
      show_query())

    df_query <- paste(df_query[-1], collapse = " ")
    df <- get_query(df_query)
    
    df <- df %>%
      unique()%>%
      as.vector()%>%
      unlist()%>%
      unname()
    
    df <- sort(df)
    
  } else if (missing(spezif_i)){
    
    df_query <- capture.output(capture_tbl("studierende_detailliert") %>%
      filter(mint_select == "MINT"  | fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer"))%>%
      filter(region %in%  spezif_r) %>%
      show_query())

    df_query <- paste(df_query[-1], collapse = " ")
    df <- get_query(df_query)
    
    df <- df %>%select(fach)%>%
      unique()%>%
      as.vector()%>%
      unlist()%>%
      unname()
    
    df <- sort(df)
    
  } else if(missing(spezif_r)){
    
    df_query <- capture.output(capture_tbl("studierende_detailliert") %>%
      filter(mint_select == "MINT"  | fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer"))%>%
      filter(indikator %in%  spezif_i) %>%
      show_query())

      df_query <- paste(df_query[-1], collapse = " ")
      df <- get_query(df_query)
      
    df <- df %>%select(fach)%>%
      unique()%>%
      as.vector()%>%
      unlist()%>%
      unname()
    
    df <- sort(df)
    
  }
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

  #### Minternational ----

  } else if (startsWith(indikator_ID, "minternational")){
    if (indikator_ID == "minternational_studierende") {
      fluidPage(
        div(class = "small-font",
        tabsetPanel(
          type = "tabs",
          tabPanel("Anteil von internationalen Studierenden nach Fächern",
                   fluidRow(
                     column(
                    width = 3,
                    br(),
                    uiOutput(ns("slider_minternational_jahr")),
                    uiOutput(ns("pickerInput_minternational_region")),
                    
                    radioButtons(
                      inputId = ns("status_ausl"),
                      label = "Studierendengruppe",
                      choices = c("Studierende",
                                  "Studienanfänger:innen (1. Hochschulsemester)"
                      ),
                      selected = "Studierende"
                    ),
                    # p("Betrachtung:"),
                    radioButtons(
                      inputId = ns("abs_zahlen_studium_studienzahl_ausl"),
                      label = "Betrachtungart",
                      choices = c("In Prozent", "Anzahl"),
                      selected = "Anzahl"
                      #justified = TRUE,
                      # checkIcon = list(yes = icon("ok",
                      #                             lib = "glyphicon"))
                    ),
                   # p("Betrachtungsebene:"),
                    radioButtons(
                      inputId = ns("ebene_ausl"),
                      label = "Betrachtungsebene",
                      choices = c("MINT-Fächer",
                                  "Fachbereiche"
                      ),
                      selected = "Fachbereiche"
                    )

          ),
           column(
             width = 9,
             br(),
             withSpinner(highchartOutput(ns("plot_minternational_uebersicht"), height = "600px"))
           )
         )
          )
         ,
          tabPanel("Zeitverlauf Zahlen von internationalen Studierenden",
                   fluidRow(
                     column(
                       width = 3,
                       br(),
                       uiOutput(ns("pickerInput_minternational_zeitverlauf")),
                     #   p("Status der Studierenden:"),
                       radioButtons(
                         inputId = ns("status_ausl_zeit"),
                         label = "Studierendengruppen",
                         choices = c("Studierende",
                                     "Studienanfänger:innen (1. Hochschulsemester)"
                         ),
                         selected = "Studierende"
                       ),
                     #   p("Betrachtung:"),
                       radioButtons(
                         inputId = ns("abs_zahlen_studium_studienzahl_ausl_zeit"),
                         label = "Betrachtungsart",
                         choices = c("In Prozent", "Anzahl"),
                         selected = "Anzahl"
                         # justified = TRUE,
                         # checkIcon = list(yes = icon("ok",
                         #                             lib = "glyphicon"))

                       )
                     ),
                     column(
                       width = 9,
                       br(),
                       withSpinner(highchartOutput(ns("plot_minternational_zeitverlauf")))
                     )
                   )
          )
          )
        )
      )
    }
    
  # Lehrkräfte FS ----

   #### Lehramt-Studis MINT ----
  
  } else if(startsWith(indikator_ID, "fs_")){
    if (indikator_ID == "fs_mint") {
      fluidPage(
        # Auswahlmöglichkeit Jahr
        column(
          width = 3,
          uiOutput(ns("slider_input_fs_mint_jahr"))
        ),
       column(
          width = 12,
          br(),
          withSpinner(highchartOutput(ns("plot_lehrkraft_mint"))),

         # p("Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt.")
        )
      )
    }
  }
 }
