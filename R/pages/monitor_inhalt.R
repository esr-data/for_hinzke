box::use(
  ../../R/utils/routing[get_hf_param, add_param_in_url, recode_parameter],
  ../../R/utils/database[get_query, load_table_by_variable, capture_tbl],
  ../../R/utils/js[get_js],
  ../../R/utils/monitor[get_content_monitor],
  ../../R/pkgs/svVis/create_bar_grouped[create_bar_grouped],
  ../../R/pkgs/svVis/create_bar_grouped[create_bar_grouped_interactive],
  ../../R/pkgs/svVis/create_bar[create_bar],
  ../../R/pkgs/svVis/create_lineplot[create_lineplot],
  ../../R/pkgs/svVis/create_flextable[create_flextable],
  ../../R/pkgs/svVis/create_choropleth_map_germany[create_choropleth_map_germany],
  ../../R/pkgs/wrangling/get_data[get_data],
  ../../prepare/data_monitor_zwischenloesung[
    give_df_ganztag_vielfalt_primar,
    give_df_ganztag_vielfalt_sek_I,
    give_df_ganztag_vielfalt_gym,
    give_df_bildung_ganztag_kooperation,
    give_df_ganztag_multiprofessionell,
    give_df_ganztag_lage_arbeitsmotivation_schulleitungen,
    give_df_ganztag_lage_weiterempfehlung_schulleiterberuf,
    give_df_gerechtigkeit_trichter_gymnasiumswahrscheinlichkeit,
    give_df_minternational_wimis,
    give_df_teilhabe_mint_stundentafeln,
    give_df_teilhabe_mint_lehrkraefte,
    give_df_teilhabe_mint_pflichtfach_inf,
    give_df_lehrkraefte_fs_medienkompetenz
  ],
  ggplot2[
    theme,
    element_text
  ],
  utils[
    capture.output
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
    htmlOutput,
    checkboxGroupInput
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
  dplyr[rename, filter, select, tbl, collect, case_when, across, mutate,
        arrange, show_query, group_by],
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
  DBI[dbConnect],
  duckdb[duckdb]
)
#TODO irrelevanten Verknüpfungen aussortieren!

con <- dbConnect(duckdb(), "data/magpie.db", read_only = TRUE)

# Global Variables -------------------------------------------------------------

MAX_VARIABLEN   <- 12
content_monitor <- get_content_monitor()
darstellungen   <- list(a = c("Zeitverlauf", "Tabelle", "Karte"))

# UI-Modul ---------------------------------------------------------------------

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
  moduleServer(
    id,
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
             parameter <- get_query_param()

             #### PARAMETER tp -------------------------------------------------

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

             #### PARAMETER fr -------------------------------------------------

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

               #### PARAMETER ind Teil 1 ---------------------------------------

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

       # Back Button und Switch Fragen/Indikatoren/Ziele -----------------------

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

       # Reactable Indikatoren -------------------------------------------------

       observeEvent(
         current$content, {
           if (get_page() == "monitor_inhalt"){
             if (!is.null(current$content)){
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
             }
           }
         }
       )

       # OUTPUTS & REACTIVES --------------------------------------------------

       ## bildung ----
       #### ganztag -----------------------------------------------------

       ##### 1 quantitativ ----

       output$Grafik_bildung_ganztag_quantitativ <- renderUI({
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

       ##### 2 vielfalt ----

       output$Tabelle_bildung_ganztag_vielfalt <- renderUI({
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

       #####3 kooperation ------------------------------------------------------

       df_bildung_ganztag_kooperation <- reactive({
         req(input$Auswahl_ganztag_kooperation_schulform)

         df <- give_df_bildung_ganztag_kooperation()

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

       output$Grafik_bildung_ganztag_kooperation <- renderPlotly({
         create_bar(
           df_bildung_ganztag_kooperation(),
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

       #####4 multiprofessionell -----------------------------------------------

       output$Tabelle_bildung_ganztag_multiprofessionell <- renderUI({
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

       #####5 lage -------------------------------------------------------------

       output$Grafik_bildung_ganztag_lage <- renderPlotly({
         req(input$Auswahl_bildung_ganztag_lage_frage)
         if (input$Auswahl_bildung_ganztag_lage_frage == "Arbeitsmotivation der Schulleitungen") {
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

       ######1 trichter---------------------------------------------------------

       output$Grafik_bildung_bildungsgerechtigkeit_gymnasium_bundeslaender <- renderPlot({
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

       output$Anmerkungen_bildung_bildungsgerichtigkeit_trichter <- renderUI({
         p(class = "anmerkungen",
           "Methodische Ergänzungen sowie weitere Statistiken zur Bewertung der Bildungsgerechtigkeit finden sich ",
           tags$a(href = "https://www.stifterverband.org/medien/vom_arbeiterkind_zum_doktor", "hier", target = "_blank"),
           "."
         )
       })

       # #### berufsorientierung -------------------------------------------------
       #
       # #####1 subjektiv---------------------------------------------------------
       #
       # output$UI_berufsorientierung_subjektiv <- renderUI({
       #   fluidRow(
       #     br(),
       #     br(),
       #     icon(
       #       style = "margin-left: 50px; color: var(--red); font-size: 50px;",
       #       "screwdriver-wrench"
       #     ),
       #     p(
       #       style = "margin-left: 50px; color: var(--red);",
       #       tags$b("Hier wird noch gebaut:"),
       #       "Interne Notiz - Nach letzter Diskussion mit Mathias soll der ganze Zweig Berufsorientierung rausgenommen werden. Die Indikatoren entfallen demnach - gerade der Indikator zu den Praktika ist sehr schwer zu erheben."
       #     )
       #   )
       # })
       #
       # #####2 praktika --------------------------------------------------------
       #
       # output$UI_berufsorientierung_praktika <- renderUI({
       #   fluidRow(
       #     br(),
       #     br(),
       #     icon(
       #       style = "margin-left: 50px; color: var(--red); font-size: 50px;",
       #       "screwdriver-wrench"
       #     ),
       #     p(
       #       style = "margin-left: 50px; color: var(--red);",
       #       tags$b("Hier wird noch gebaut:"),
       #       "Interne Notiz - Nach letzter Diskussion mit Mathias soll der ganze Zweig Berufsorientierung rausgenommen werden. Die Indikatoren entfallen demnach - gerade der Indikator zu den Praktika ist sehr schwer zu erheben."
       #     )
       #   )
       # })

       # mint ----

       #### minternational ----

       #####1 studierende ----------

       #1.1 übersicht
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


         df <- tbl(con, from = "studierende_detailliert") %>%
           filter(indikator %in% c("internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "internationale Studierende",
                                   "Studienanfänger:innen (1. Hochschulsemester)",
                                   "Studierende"),
                  geschlecht == "Gesamt",
                  # region == input$region_minternational_übersicht,
                  region == bl_select,
                  jahr == year_select )%>%
           select(-mint_select,- fachbereich)%>%
           collect() %>%
           pivot_wider(names_from=indikator, values_from = wert)%>%
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

       #1.2 Zeitverlauf

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


         df <- tbl(con, from = "studierende_detailliert") %>%
           filter(indikator %in% c("internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "internationale Studierende",
                                   "Studienanfänger:innen (1. Hochschulsemester)",
                                   "Studierende"),
                  geschlecht == "Gesamt")%>%
           select(-mint_select,- fachbereich)%>%
           collect() %>%

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

       output$Anmerkungen_minternational_studis <- renderUI({
         p(class = "anmerkungen",
           "Methodische Ergänzungen sowie weitere Statistiken zu internationalen MINT-Studierenden finden Sie ",
           tags$a(href = "https://mint-vernetzt.shinyapps.io/datalab/", "hier", target = "_blank"),
           "."
         )
       })

       #####2 wimis -------

       output$Grafik_minternational_wimis <- renderPlotly({
         req(input$Auswahl_minternational_faechergruppe_wimis_in)
         create_lineplot(
           dplyr::filter(give_df_minternational_wimis(), Fachbereich %in% input$Auswahl_minternational_faechergruppe_wimis_in),
           jahr,
           value,
           group_color = Fachbereich,
           plot_title = "Anteil ausländisches wissenschaftliches und künsterlisches Personal an Hochschulen nach Fachbereichen",
           plot_subtitle = "Anteile in Prozent.",
           custom_caption = "Quelle: Destatis, eigene Berechnungen.",
           plot_type = "plotly"
         )
       })

       output$Anmerkungen_minternational_wimis <- renderUI({
         p(class = "anmerkungen",
           "Methodische Ergänzungen sowie weitere Statistiken zu MINT-Personal finden Sie ",
           tags$a(href = "https://www.hsi-monitor.de/themen/unterthemen/wissenschaftliches-und-kuenstlerisches-personal-einleitung/", "hier", target = "_blank"),
           "."
         )
       })

       #### teilhabe MINT ----

       #####1 studentafeln -----

       output$Grafik_mint_teilhabe_stundentafeln <- renderPlotly({ #TODO das ist viel zu langsam und sieht nicht gut aus, hie rbrauchen wir in svVis eine Überarbeitung! Auch wenn da schon ewig viel Arbeit in diese Karten riengeflossen ist, wieso ist dass denn immer alles so schwer :(
         create_choropleth_map_germany(
           give_df_teilhabe_mint_stundentafeln(),
           anteil_wochenstunden_mint,
           plot_title = "Anteil Wochenstunden im MINT Bereich an allen Wochenstunden an Gymnasien (G8/G9)",
           plot_subtitle = "Angaben in Prozent.",
           custom_caption = "Quelle: Informatik Monitor 2023.",
           interactive = TRUE
         )
       })

       output$Anmerkungen_mint_teilhabe_studentafeln <- renderUI({
         p(class = "anmerkungen",
           "Methodische Ergänzungen sowie weitere Statistiken zu Stundentafeln finden Sie ",
           tags$a(href = "https://informatik-monitor.de/2023-24/laendervergleich", "hier", target = "_blank"),
           "."
         )
       })

       #####2 lehrkreafte informatik -----

       output$Tabelle_mint_teilhabe_lehrkraefte <- renderUI({
         create_flextable(give_df_teilhabe_mint_lehrkraefte()) %>%
           htmltools_value()
       })

       output$Anmerkungen_mint_teilhabe_lehrkraefte <- renderUI({
         p(class = "anmerkungen",
           "Methodische Ergänzungen sowie weitere Statistiken zu Stundentafeln finden Sie ",
           tags$a(href = "https://informatik-monitor.de/2023-24/laendervergleich", "hier", target = "_blank"),
           "."
         )
       })

       #####3 oberstufe mint -----

       #UI Balkendiagramm
       output$menu_mint_oberstufe <- renderUI({
         tagList(
           sliderTextInput(
             inputId = ns("datum_mint_oberstufe"),
             label = "Jahr:",
             choices = 2013:2022,
             selected = 2022
           ),
           pickerInput(
             inputId = ns("region_mint_oberstufe"),
             label = "Region:",
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
                         "Ostdeutschland (inkl. Berlin)"
             ),
             multiple = FALSE,
             selected = c("Deutschland")
           )
         )

       })

       observeEvent(input$datum_mint_oberstufe, {
         r$datum_mint_oberstufe <- input$datum_mint_oberstufe
       })

       observeEvent(input$region_mint_oberstufe, {
         r$region_mint_oberstufe <- input$region_mint_oberstufe
       })

       #UI Zeitverlauf

       output$menu_mint_oberstufe_verlauf <- renderUI({
         tagList(
           sliderTextInput(
             inputId = ns("datum_mint_oberstufe_verlauf"),
             label = "Jahr:",
             choices = 2013:2022,
             selected = c(2016, 2022)
           ),
           pickerInput(
             inputId = ns("region_mint_oberstufe_verlauf"),
             label = "Region:",
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
                         "Ostdeutschland (inkl. Berlin)"
             ),
             multiple = FALSE,
             selected = c("Deutschland")
           ),
           radioButtons(
             label = "Betrachtung",
             inputId = ns("abs_rel_mint_oberstufe_verlauf"),
             choices = c("In Prozent", "Anzahl")
           )
         )

       })

       observeEvent(input$datum_mint_oberstufe_verlauf, {
         r$datum_mint_oberstufe_verlauf <- input$datum_mint_oberstufe_verlauf
       })

       observeEvent(input$region_mint_oberstufe_verlauf, {
         r$region_mint_oberstufe_verlauf <- input$region_mint_oberstufe_verlauf
       })

       observeEvent(input$abs_rel_mint_oberstufe_verlauf, {
         r$abs_rel_mint_oberstufe_verlauf <- input$abs_rel_mint_oberstufe_verlauf
       })



       #Output Balkendiagramm

       output$plot_mint_oberstufe <- renderHighchart({

         timerange <- r$datum_mint_oberstufe
         regio <- r$region_mint_oberstufe

         df_query <- capture.output(capture_tbl("kurse") %>%
                                      filter(jahr == timerange,
                                             region == regio,
                                             anzeige_geschlecht == "Gesamt") %>%
                                      select(-region, -jahr, - bereich) %>%
                                      show_query())

         df_query <- paste(df_query[-1], collapse = " ")
         df <- get_query(df_query)

         # calculate proportions
         df <- df %>%
           group_by(indikator) %>%
           mutate(sum_wert = sum(wert, na.rm = TRUE))

         df <- df %>% group_by(indikator, fachbereich) %>%
           mutate(proportion = wert/sum_wert *100)


         #Trennpunkte für lange Zahlen ergänzen
         df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

         #als Faktor für Darstellung
         df$fachbereich <- as.factor(df$fachbereich)
         df$fachbereich <- factor(df$fachbereich, levels = c("MINT", "andere Fächer"))

         hchart(df, 'bar', hcaes(y = round(proportion), x = indikator, group = forcats::fct_rev(fachbereich))) %>%
           hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
           hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
           hc_xAxis(title = list(text = "")) %>%
           hc_plotOptions(bar = list(stacking = "percent")) %>%
           hc_colors(c( "#ADA58B", "#195365")) %>%
           hc_title(text = paste0("Anteil von MINT-Belegungen an allen Belegungen in ", regio, " (", timerange,")"),
                    margin = 45,
                    align = "center",
                    style = list(color = "black", useHTML = TRUE, fontFamily = "Arial", fontSize = "18px")) %>%
           hc_chart(
             style = list(fontFamily = "Arial", fontSize = "16px")
           ) %>%
           hc_legend(enabled = TRUE, reversed = TRUE)

       })

       # Output Zeitverlauf

       output$plot_mint_oberstufe_verlauf <- renderHighchart({

         timerange <- r$datum_mint_oberstufe_verlauf
         t <- timerange[1]:timerange[2]
         regio <- r$region_mint_oberstufe_verlauf
         abs_rel <- r$abs_rel_mint_oberstufe_verlauf

         df_query <- capture.output(capture_tbl("kurse") %>%
                                      filter(
                                        region == regio,
                                        anzeige_geschlecht == "Gesamt",
                                        fachbereich %in% c("MINT", "andere Fächer")) %>%
                                      select(-region, - bereich) %>%
                                      show_query())

         df_query <- paste(df_query[-1], collapse = " ")
         df <- get_query(df_query)
         df <- df %>% filter(jahr %in% t)

         # calculate proportions
         df$wert_new <- df$wert
         df <- df %>%
           dplyr::group_by(jahr, indikator) %>%
           dplyr::mutate(sum_wert = sum(wert_new))


         # calcualte proportions
         df <- df %>% dplyr::group_by(jahr, indikator, fachbereich, wert_new) %>%
           dplyr::summarize(wert = wert_new/sum_wert)%>%
           dplyr::rename(Absolut = wert_new, Relativ=wert)%>%
           tidyr::pivot_longer(c(Absolut, Relativ), names_to = "selector", values_to = "wert")%>%
           dplyr::filter(fachbereich == "MINT")%>%
           dplyr::mutate(selector=dplyr::case_when(
             selector == "Relativ" ~ "In Prozent",
             selector == "Absolut" ~ "Anzahl"
           ))

         if(abs_rel  == "In Prozent"){

           df <- df %>%
             dplyr::filter(selector == "In Prozent")

           df$wert <- df$wert * 100

           # order years for plot
           df <- df[with(df, order(jahr, decreasing = FALSE)), ]



           # plot
           highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(wert), group = indikator)) %>%
             highcharter::hc_tooltip(pointFormat = "Anteil: {point.indikator} <br> Wert: {point.y} %") %>%
             highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
             highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
             #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
             highcharter::hc_title(text = paste0("Anteil von MINT-Belegungen an allen Belegungen"),
                                   margin = 45,
                                   align = "center",
                                   style = list(color = "black", useHTML = TRUE, fontFamily = "Arial", fontSize = "18px")) %>%
             highcharter::hc_colors(c("#ADA58B", "#195365","#E73f0C")) %>%
             highcharter::hc_chart(
               style = list(fontFamily = "Arial", fontSize = "16px")
             )

         } else if (abs_rel =="Anzahl") {

           hcoptslang <- getOption("highcharter.lang")
           hcoptslang$thousandsSep <- "."
           options(highcharter.lang = hcoptslang)

           df <- df %>%
             dplyr::filter(selector == "Anzahl")

           # order years for plot
           df <- df[with(df, order(jahr, decreasing = FALSE)), ]

           highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(wert), group = indikator)) %>%
             highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
             highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
             highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
             #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
             highcharter::hc_title(text = paste0("Anzahl an MINT-Belegungen in der Schule"),
                                   margin = 45,
                                   align = "center",
                                   style = list(color = "black", useHTML = TRUE, fontFamily = "Arial", fontSize = "18px")) %>%
             highcharter::hc_colors(c("#ADA58B", "#195365","#E73f0C")) %>%
             highcharter::hc_chart(
               style = list(fontFamily = "Arial", fontSize = "16px")
             )



         }

       })

       #####4 oberstufe mint frauen -----

       #UI Balkendiagramm
       output$menu_mint_oberstufe_frauen <- renderUI({
         tagList(
           sliderTextInput(
             inputId = ns("datum_mint_oberstufe_frauen"),
             label = "Jahr:",
             choices = 2013:2022,
             selected = 2022
           )
           ,
           pickerInput(
             inputId = ns("region_mint_oberstufe_frauen"),
             label = "Region:",
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
                         "Ostdeutschland (inkl. Berlin)"
             ),
             multiple = FALSE,
             selected = c("Deutschland")
           )
         )

       })

       observeEvent(input$datum_mint_oberstufe_frauen, {
         r$datum_mint_oberstufe_frauen <- input$datum_mint_oberstufe_frauen
       })

       observeEvent(input$region_mint_oberstufe_frauen, {
         r$region_mint_oberstufe_frauen <- input$region_mint_oberstufe_frauen
       })

       #UI Zeitverlauf

       output$menu_mint_oberstufe_frauen_verlauf <- renderUI({
         tagList(
           sliderTextInput(
             inputId = ns("datum_mint_oberstufe_frauen_verlauf"),
             label = "Jahr:",
             choices = 2013:2022,
             selected = c(2016, 2022)
           )
           ,
           pickerInput(
             inputId = ns("region_mint_oberstufe_frauen_verlauf"),
             label = "Region:",
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
                         "Ostdeutschland (inkl. Berlin)"
             ),
             multiple = FALSE,
             selected = c("Deutschland")
           ),
           radioButtons(
             label = "Betrachtung",
             inputId = ns("abs_rel_mint_oberstufe_frauen_verlauf"),
             choices = c("In Prozent", "Anzahl")
           )

         )

       })

       observeEvent(input$datum_mint_oberstufe_frauen_verlauf, {
         r$datum_mint_oberstufe_frauen_verlauf <- input$datum_mint_oberstufe_frauen_verlauf
       })

       observeEvent(input$region_mint_oberstufe_frauen_verlauf, {
         r$region_mint_oberstufe_frauen_verlauf <- input$region_mint_oberstufe_frauen_verlauf
       })

       observeEvent(input$abs_rel_mint_oberstufe_frauen_verlauf, {
         r$abs_rel_mint_oberstufe_frauen_verlauf <- input$abs_rel_mint_oberstufe_frauen_verlauf
       })

       #Output Balkendiagramm

       output$plot_mint_oberstufe_frauen <- renderHighchart({

         timerange <- r$datum_mint_oberstufe_frauen
         regio <- r$region_mint_oberstufe_frauen


         df_query <- capture.output(capture_tbl("kurse") %>%
                                      filter(jahr == timerange,
                                             region == regio) %>%
                                      select(-region, -jahr, - bereich) %>%
                                      show_query())

         df_query <- paste(df_query[-1], collapse = " ")
         df <- get_query(df_query)

         df <-  df %>% filter(anzeige_geschlecht != "Gesamt") %>%
           group_by(fachbereich, indikator) %>%
           mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

         df <- df %>% filter(anzeige_geschlecht == "Frauen")

         #gegenwert Berechnen
         df_m <- df %>% group_by(fachbereich, indikator) %>%
           mutate(wert = props - wert)

         df_m$anzeige_geschlecht <- "Männer"

         df <- rbind(df, df_m)

         # calcualte proportions
         df <- df %>% group_by(indikator, fachbereich, anzeige_geschlecht) %>%
           mutate(proportion = wert/props *100)

         df$fachbereich <- factor(df$fachbereich, levels = c("MINT","andere Fächer"))

         df$indikator <- ifelse(df$indikator == "Grundkurse" & df$fachbereich == "MINT", "Grundkurse MINT-Fächer", df$indikator)
         df$indikator <- ifelse(df$indikator == "Grundkurse" & df$fachbereich == "andere Fächer", "Grundkurse andere Fächer", df$indikator)
         df$indikator <- ifelse(df$indikator == "Leistungskurse" & df$fachbereich == "MINT", "Leistungskurse MINT-Fächer", df$indikator)
         df$indikator <- ifelse(df$indikator == "Leistungskurse" & df$fachbereich == "andere Fächer", "Leistungskurse andere Fächer", df$indikator)
         df$indikator <- ifelse(df$indikator == "Oberstufenbelegungen" & df$fachbereich == "MINT", "Oberstufenbelegungen MINT-Fächer", df$indikator)
         df$indikator <- ifelse(df$indikator == "Oberstufenbelegungen" & df$fachbereich == "andere Fächer", "Oberstufenbelegungen andere Fächer", df$indikator)

         df$anzeige_geschlecht[df$anzeige_geschlecht == "Frauen"] <- "Mädchen"
         df$anzeige_geschlecht[df$anzeige_geschlecht == "Männer"] <- "Jungen"

         #Trennpunkte für lange Zahlen ergänzen
         df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

         # plot
         hchart(df, 'bar', hcaes( x = indikator, y=round(proportion), group = anzeige_geschlecht)) %>%
           hc_tooltip(pointFormat = "{point.anzeige_geschlecht}-Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
           hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  TRUE) %>%
           hc_xAxis(title = list(text = ""), categories=c("Grundkurse MINT-Fächer",
                                                          "Grundkurse andere Fächer",
                                                          "Leistungskurse MINT-Fächer",
                                                          "Leistungskurse andere Fächer",
                                                          "Oberstufenbelegungen MINT-Fächer",
                                                          "Oberstufenbelegungen andere Fächer")) %>%
           hc_plotOptions(bar = list(stacking = "percent")) %>%
           hc_colors(c( "#ADA58B", "#195365")) %>%
           hc_title(text = paste0("Anteil von Mädchen in MINT- und anderen Fächern ", "(", timerange, ")"),
                    margin = 25,
                    align = "center",
                    style = list(color = "black", useHTML = TRUE, fontFamily = "Arial", fontSize = "18px")) %>%
           hc_chart(
             style = list(fontFamily = "Arial", fontSize = "16px")
           ) %>%
           hc_legend(enabled = TRUE, reversed = TRUE)

       })

       # Output Zeitverlauf
       output$plot_mint_oberstufe_frauen_verlauf <- renderHighchart({

         timerange <- r$datum_mint_oberstufe_frauen_verlauf
         t <- timerange[1]:timerange[2]
         regio <- r$region_mint_oberstufe_frauen_verlauf
         abs_rel <- r$abs_rel_mint_oberstufe_frauen_verlauf


         df_query <- capture.output(capture_tbl("kurse") %>%
                                      filter(
                                        region == regio,
                                        fachbereich =="MINT") %>%
                                      select(-region, - bereich) %>%
                                      show_query())

         df_query <- paste(df_query[-1], collapse = " ")
         df <- get_query(df_query)
         df <- df %>% filter(jahr %in% t)

         df <-  df %>% filter(anzeige_geschlecht != "Gesamt") %>%
           group_by(fachbereich, indikator, jahr) %>%
           mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

         df <- df %>% filter(anzeige_geschlecht == "Frauen")

         # calcualte proportions
         df <- df %>% group_by(indikator, fachbereich, anzeige_geschlecht, jahr) %>%
           mutate(proportion = wert/props)

         df$proportion <- round(df$proportion*100,0)

         df$anzeige_geschlecht[df$anzeige_geschlecht == "Frauen"] <- "Mädchen"

         #Trennpunkte für lange Zahlen ergänzen
         df$wert_anzeige <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

         if(abs_rel == "Anzahl"){

           # order years for plot
           df <- df[with(df, order(jahr, decreasing = FALSE)), ]

           hcoptslang <- getOption("highcharter.lang")
           hcoptslang$thousandsSep <- "."
           options(highcharter.lang = hcoptslang)

           # plot
           highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group = indikator)) %>%
             highcharter::hc_tooltip(pointFormat = "Anzahl: {point.indikator} <br> Wert: {point.wert_anzeige}") %>%
             highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
             highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
             #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
             highcharter::hc_title(text = paste0("Anzahl von Mädchen in MINT-Oberstufenkursen"),
                                   margin = 45,
                                   align = "center",
                                   style = list(color = "black", useHTML = TRUE, fontFamily = "Arial", fontSize = "18px")) %>%
             highcharter::hc_colors(c("#ADA58B", "#195365","#E73f0C")) %>%
             highcharter::hc_chart(
               style = list(fontFamily = "Arial", fontSize = "16px")
             )

         } else if (abs_rel =="In Prozent") {

           # order years for plot
           df <- df[with(df, order(jahr, decreasing = FALSE)), ]

           highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = proportion, group = indikator)) %>%
             highcharter::hc_tooltip(pointFormat = "Anzahl: {point.indikator} <br> Wert: {point.y}%") %>%
             highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
             highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
             #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
             highcharter::hc_title(text = paste0("Mädchenanteil in MINT-Oberstufenkursen"),
                                   margin = 45,
                                   align = "center",
                                   style = list(color = "black", useHTML = TRUE, fontFamily = "Arial", fontSize = "18px")) %>%
             highcharter::hc_colors(c("#ADA58B", "#195365","#E73f0C")) %>%
             highcharter::hc_chart(
               style = list(fontFamily = "Arial", fontSize = "16px")
             )



         }

       })

       #####5 pflichtfach informatik -----

       output$Grafik_mint_teilhabe_pflichtfach_inf <- renderPlotly({ #TODO das ist viel zu langsam und sieht nicht gut aus, hie rbrauchen wir in svVis eine Überarbeitung! Auch wenn da schon ewig viel Arbeit in diese Karten riengeflossen ist, wieso ist dass denn immer alles so schwer :(
         create_choropleth_map_germany(
           give_df_teilhabe_mint_pflichtfach_inf(),
           kategorie,
           plot_title = "Ausmaß des Pflichtfaches Informatik in den Bundesländern",
           plot_subtitle = "Anhand der Wochenstundenzahlen gemäß Lehrplänen.",
           custom_caption = "Quelle: Informatik Monitor 2023.",
           interactive = TRUE
         )
       })

       output$Anmerkungen_mint_teilhabe_pflichtfach_inf <- renderUI({
         p(class = "anmerkungen",
           "Methodische Ergänzungen sowie weitere Statistiken zum Thema Pflichtfach Informatik finden Sie ",
           tags$a(href = "https://informatik-monitor.de/2023-24/", "hier", target = "_blank"),
           "."
         )
       })

       # lehrkraefte ----

       #### lehrkraefte future skills ----

       #####1 medienkompetenz ----

       output$Grafik_fs_digital_lehramt <- renderPlotly({
         create_bar_grouped(
           give_df_lehrkraefte_fs_medienkompetenz(),
           Schulform,
           Wert,
           group_var = Jahr,
           interactive = TRUE,
           flipped = TRUE,
           mode = "dodge",
           plot_title = "Anteil der Hochschulen mit für alle Lehramtsstudierenden verpflichtenden Angeboten zum Erwerb von Medienkompetenz",
           plot_subtitle = "Angaben in Prozent",
           custom_caption = "Quelle: Monitor Lehrkräfte."
         )
       })

       output$Anmerkungen_fs_digital_lehramt <- renderUI({
         p(class = "anmerkungen",
           "Methodische Ergänzungen sowie weitere Statistiken zum Thema digitale Kompetenzen/Medienkompetenz im Lehramt finden Sie ",
           tags$a(href = "https://www.monitor-lehrkraeftebildung.de/schwerpunkte/digitalisierung/vergleichsdaten-2017-2022/", "hier", target = "_blank"),
           "."
         )
       })

       #####2 lehramt-mint -------

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
         df <- dplyr::tbl(con, from = "studierende") %>%
           dplyr::filter(jahr == timerange,
                         geschlecht == "Gesamt",
                         region== "Deutschland")%>%
           dplyr::collect()
         df <- df %>%
           tidyr::pivot_wider(names_from=fachbereich, values_from = wert)%>%
           #dplyr::rename("MINT (gesamt)" = MINT)%>%
           dplyr::select( -region, -Ingenieurwissenschaften,- `Mathematik, Naturwissenschaften`)

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

       output$Anmerkungen_lehrkraefte_fs_mint <- renderUI({
         p(class = "anmerkungen",
           "Methodische Ergänzungen sowie weitere Statistiken zum Thema MINT_Lehramt finden Sie ",
           tags$a(href = "https://mint-vernetzt.shinyapps.io/datalab/", "hier", target = "_blank"),
           "."
         )
       })

    }




  )
}

# DETAILS ROWS IN REACTABLE ----------------------------------------------------

#' Missing description
#' @noRD

draw_table_row_content <- function(indikator_ID, ns) {

  # bildung ----


  if (startsWith(indikator_ID, "ganztag")) {

    #### ganztag ----

    if(indikator_ID == "ganztag_quantitativ") {

      #####1 quantiitativ ----

      fluidPage(
        p(
          "Der Ausbau der Ganztagsschule ist wichtig, weil er eine bessere
          Vereinbarkeit von Beruf und Familie ermöglicht und gleichzeitig
          Chancengleichheit durch zusätzliche Bildungs- und Betreuungsangebote
          fördert. Zudem unterstützt er eine ganzheitliche Entwicklung der
          Schülerinnen und Schüler durch mehr Zeit für individuelle Förderung,
          soziale Interaktion und außerschulische Aktivitäten. Derzeit erfüllen
          ", tags$b("73,2 Prozent aller allgemeinbildeneden Schulen in
          Primar- und Sekundarbereich I"), " die ", span("KMK Definition für
          Ganztag", id = "tooltip_KMK_Def_Ganztag"),". Dabei gibt es jedoch nach
          Schulträgerschaft, Schulform, Ganztagsform und Bundesländern große
          Unterschiede. ", tags$i("Nutzen Sie die Filter, um die Sie
          interessierenden Zahlen zu erhalten.")
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
            uiOutput(ns("Grafik_bildung_ganztag_quantitativ"))
          )
        ),
        br(),
        p(
          "Unter Berücksichtigung der steigenden Zahl an Schülerinnen und Schülern
          gemäß KMK-Prognose, der derzeitigen Ausbaugeschwindigkeit sowie des
          bestehenden und prognostiziertem Lehrkräftemangel ist das Projekt
          Ausbau Ganztagsschule sowie die tatsächliche Erfüllung des
          Rechtsanspruchs Ganztagsschule im Primarbereich ab 2026 aktuell nicht
          wie gewünscht zu realisieren. Über ein Drittel der Grundschuleitungen
          gehen davon aus, dass eine Ganztagsbetreuung von der jeweiligen
          Kommune bis 2026/2027 nicht sichergestellt werden kann (",
          tags$a(href = "https://deutscher-schulleitungskongress.de/wp-content/uploads/2023/11/2023-11-21_VOe-Nov_Bericht_Deutschland.pdf",
          "VBE 2023, S. 26", target = "_blank"), ")."
        ),
        p(class = "anmerkungen", "Anmerkungen:"),
        uiOutput(ns("Anmerkungen_bildung_ganztag_quantitativ"))
      )

    } else if (indikator_ID == "ganztag_vielfalt"){

      #####2 vielfalt ----

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
            uiOutput(ns("Tabelle_bildung_ganztag_vielfalt"))
          )
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_bildung_ganztag_vielfalt"))
      )

    } else if (indikator_ID == "ganztag_kooperation"){

      #####3 kooperation ----

      fluidPage(
        p(
          "Schulen sollten im Ganztag mit außerschulischen Lernorten und
          Bildungspartnern zusammenarbeiten, um den Schülern ein breites
          Spektrum an Lernmöglichkeiten und realen Erfahrungen zu bieten. Diese
          Kooperationen können innovative Bildungsangebote schaffen, die den
          Unterricht bereichern und den Schülerinnen und Schülern praxisnahe
          Einblicke in verschiedene Berufsfelder und gesellschaftliche Bereiche
          ermöglichen.
          Zur Gestaltung unterrichtsbezogener Angebote kooperieren gemäß
          Schulleitungsbefragung aus dem Jahr 2023 der Telekom Stiftung ",
          tags$b("86 Prozent"), " der allgemeinbildenden öffentlichen Schulen
          mit außerschulischen Lernorten und/oder Bildungspartnern. Schulen
          kooperieren vor allem mit Sportvereinen, Bibliotheken, Musikschulen
          sowie Einrichtungen der Kinder- und Jugendarbeit."),
        p(
          "Die Kooperation findet dabei im allgemeinen (70 Prozent) auf Basis
          eines gemeinsam formulierten
          Pädagogischen Konzept statt.
        "),
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
            plotlyOutput(ns("Grafik_bildung_ganztag_kooperation"))
          ),
          p("Anmerkungen:", class = "anmerkungen"),
          uiOutput(ns("Anmerkungen_bildung_ganztag_kooperation"))
        )
      )

    } else if (indikator_ID == "ganztag_multiprofessionell") {

      #####4 multiprofessionell -----

      fluidPage(
        p(
          "Bei der Umsetzung von Ganztag können und sollten multiprofessionelle
          Teams an Schulen eingesetzt werden. Multiprofessionelle Teams sind
          wichtig, weil sie durch ihre unterschiedlichen Fachkenntnisse und
          Perspektiven eine ganzheitliche Betreuung und Förderung der
          Schülerinnen und Schüler ermöglichen sowie die Lehrkräfte entlasten
          können. ", tags$b("Oftmals arbeiten an Schulen - neben
          Gebäudemanagement und Sekretariat - jedoch ausschließlich Lehrkräfte."
          ), " Unterstützungsstrukturen etwa durch IT-, und Verwaltungsfachkräfte und/oder Sozial-, Medien-, Kulturpädagoginnen
          und -pädagogen sowie Projektmanagement etc. fehlen.",
        ),
        br(),
        p(" Konkrete Zahlen liefert zu diesem Befund eine Schulleitungsbefragung
          zu Multiprofessionalität an allgemeinbildenden Schulen von der
          Telekom Stiftung, dargestellt in der folgenden Tabelle - Angaben in
          Prozent:"
        ),
        br(),
        fluidRow(
          column(
            width = 12,
            uiOutput(ns("Tabelle_bildung_ganztag_multiprofessionell"))
          )
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_bildung_ganztag_multiprofessionell"))
      )

    } else if (indikator_ID == "ganztag_lage"){

      #####5 lage ----

      fluidPage(
        p(
          "Wie bewerten die Schulleitungen ganz allgemein die Lage an den
          Schulen? Flapsig: Wie ist die Stimmung? Dieser Frage nährt sich die
          jährliche Schulleitungsbefragung des Verband Bildung und Erziehung
          (VBE) u. a. mit den Fragen danach, inwieweit die Schulleitungen gerne
          Ihren Berauf ausüben und ob sie eine Weiterempfehlung für den Beruf
          abgeben."
        ),
        p(
          "Die Ergebnisse: 83 Prozent der Schulleitungen üben ihren Job (sehr)
          gerne aus - vor der Covid 19-Pandemie waren es noch 96 Prozent.
          Auch die positive Weiterempfehlung des Berufs ist nicht mehr wie vor
          der Pandemie gegeben, zuletzt haben knapp die Hälfte der
          Schulleitungen eine wahrscheinliche oder klare Weiterempfehlung für
          Ihren Beruf ausgesprochen."
        ),
        fluidRow(
          column(
            width = 3,
            radioButtons(ns("Auswahl_bildung_ganztag_lage_frage"), "Auswahl:", choices = c("Arbeitsmotivation der Schulleitungen", "Weiterempfehlung Beruf Schulleitung"))
          ),
          column(
            width = 9,
            plotlyOutput(ns("Grafik_bildung_ganztag_lage"))
          )
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_bildung_ganztag_lage"))
      )
    }

  #### bildungsgerechtigkeit ----

  #####1 trichter ----

  } else if (indikator_ID == "bildungsgerechtigkeit_trichter") {
    fluidPage(
      p(
        "Der Bildungstrichter des Stifterverbandes ist ein analytisches
        Instrument, das die Bildungschancen von jungen Menschen mit
        Akademikereltern und ohne Akademikereltern in Deutschland von der
        Schule bis zum Doktorgrad abbildet. Das Instrument baut auf den Arbeiten
        des ", tags$a("DZHWs", href = "https://www.dzhw.eu/pdf/pub_brief/dzhw_brief_03_2018.pdf",
        target = "_blank"), " auf und zeigt deutlich wie ungerecht
        Bildungschancen in Deutschland verteilt sind. ", tags$b("Beginnen von
        100 Grundschulkindern aus einem Akademikerhaushalt 79 ein Studium. Sind
        es bei Grundschulkindern ohne akademischen Hintergrund gerade einmal 27.
        "), " Diese Ungleichheit zieht sich durch die Bildungsstufen bis zur
        Promotion. In den letzten Jahren wurde leicht geringere Differenzen
        beobachtet - der weite Weg zu mehr Chancengleichheit bleibt jedoch."
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
        "Hinsichtlich des Übergangs Grundschule zum Gymnasium liefert zudem eine
        ", tags$a("ifo-Studie", href = "https://www.ifo.de/DocDL/sd-2024-05-ungleiche-bildungschancen-woessmann-etal-.pdf", targe = "_blank"),
        " Auswertungen - auch auf Bundeslandsebene. Aufgrund methodischer
        Unterschiede lassen sich die Daten ", tags$b("nicht"), "direkt bei dem
        Bildungstrichter einsortieren, bieten aber nochmals eine weitere
        Perspektive. Die Grafik zeigt die Wahrscheinlichkeit, mit der Kinder aus
        benachteiligten Verhältnissen ein Gymnasium besuchen gegenüber Kindern
        aus günstigen Verhältnissen:"
      ),
      br(),
      plotOutput(ns("Grafik_bildung_bildungsgerechtigkeit_gymnasium_bundeslaender"), height = "1000px", width = "800px"),
      p("Anmerkungen:", class = "anmerkungen"),
      uiOutput(ns("Anmerkungen_bildung_bildungsgerichtigkeit_trichter"))
    )

  } else if (startsWith(indikator_ID, "berufsorientierung")) {

    #### berufsorientierung ----

    if (indikator_ID == "berufsorientierung_subjektiv") {

      #####1 subjektiv ----

      fluidPage(
        uiOutput(ns("UI_berufsorientierung_subjektiv"))
      )

    } else {

      #####2 praktika ----

      fluidPage(
        uiOutput(ns("UI_berufsorientierung_praktika"))
      )
    }

# mint ----

  } else if (startsWith(indikator_ID, "minternational")){

  ##### minternational ----

    if (indikator_ID == "minternational_studierende") {

      #####1 studierende ----

      fluidPage(
        p("Es ist wichtig, dass Deutschland viele ausländische Studierende im Bereich MINT (Mathematik, Informatik, Naturwissenschaften und Technik) anzieht:"),
        tags$ul(
          tags$li("Fachkräftemangel - Deutschland hat z. T. einen Mangel an Fachkräften in MINT-Berufen. Ausländische Studierende können diese Lücke schließen, indem sie nach ihrem Studium in Deutschland bleiben und arbeiten."),
          tags$li("Internationale Wettbewerbsfähigkeit - Eine vielfältige Studentenschaft fördert den Austausch von Ideen und Innovationen, was die Wettbewerbsfähigkeit Deutschlands stärkt."),
          tags$li("Kultureller Austausch und Integration - Ausländische Studierende bereichern die kulturelle Vielfalt an Hochschulen und fördern die interkulturelle Kompetenz."),
          tags$li("Wirtschaftliche Vorteile - Sie tragen durch Konsumausgaben und Studiengebühren zur deutschen Wirtschaft bei und können langfristig als hochqualifizierte Arbeitskräfte zur wirtschaftlichen Stabilität beitragen.")
        ),
        p(
          tags$b("Im Jahr 2022 waren  195.814 internationale Studierende in
          MINT-Fächern an deutschen Hochschulen eingeschrieben, was 18,2 Prozent
          aller MINT-Studierenden ausmacht. Der Anteil ausländischer
          Studierender ist in MINT-Fächern deutlich höher als in anderen
          Fachbereichen."),
        ),
        br(),
        br(),
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
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_minternational_studis"))
      )
    } else {

      #####2 wimis ----

      fluidPage(
        p(
          "Die Gewinnung internationaler Wissenschaftler im Bereich MINT
          (Mathematik, Informatik, Naturwissenschaften und Technik) ist für
          Deutschland von großer Bedeutung, denn internationale Wissenschaftler
          bringen wertvolles Wissen und Expertise mit, die zur Weiterentwicklung
          von Forschung und Technologie in Deutschland beitragen. Ihre
          unterschiedlichen Hintergründe und Erfahrungen fördern den
          wissenschaftlichen Fortschritt und die Innovation, stärken
          internationale Kooperationen und Vernetzung.", tags$b("Im Jahr 2023
          arbeiteten 63.078 internationale Wissenschaftlerinnen und
          Wissenschaftler in MINT-Fächern an deutschen Hochschulen, was 18,1
          Prozent des gesamten wissenschaftlichen und künsterlischen Personals
          in MINT-Fächern ausmacht."), "Dieser Anteil ist deutlich höher als in
          anderen Fachbereichen. Der Unterschied bei den Anteilen zwischen dem
          MINT-Bereich und dem Nicht-MINT-Bereich ist seit 2008 von 2,6
          Prozentpunkten auf 5,8 Prozentpunkte gestiegen."
        ),
        br(),
        fluidRow(
          column(
            width = 3,
            checkboxGroupInput(
              inputId = ns("Auswahl_minternational_faechergruppe_wimis_in"),
              label = "Fachbereich",
              choices = c(
                "Wiss./künstl. Personal insgesamt (ohne zentrale Einrichtungen)",
                "MINT",
                "Alle Nicht MINT-Fächer" #,
                # "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin", Rein oder nicht?
                # "Geisteswissenschaften",
                # "Humanmedizin/Gesundheitswissenschaften",
                # "Ingenieurwissenschaften",
                # "Kunst, Kunstwissenschaft",
                # "Mathematik, Naturwissenschaften",
                # "Rechts-, Wirtschafts- und Sozialwissenschaften",
                # "Sport",
                # "Wiss./künstl. Personal insgesamt (inkl. zentrale Einrichtungen)"
              ),
              selected = c(
                "Wiss./künstl. Personal insgesamt (ohne zentrale Einrichtungen)",
                "MINT",
                "Alle Nicht MINT-Fächer"
              )
            )
          ),
          column(
            width = 9,
            plotlyOutput(ns("Grafik_minternational_wimis"))
          )
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_minternational_wimis"))
      )
    }

  } else if(startsWith(indikator_ID, "teilhabe")) {

    ##### teilhabe an MINT -----------------------------------------------------

    if (indikator_ID == "teilhabe_stundentafeln") {

      #####1 studentafeln ----

      fluidPage(
        p(
          "MINT-Fächer (Mathematik, Informatik, Naturwissenschaften und Technik)
          sind in der Schule von entscheidender Bedeutung, da sie die Grundlage
          für die technologische und wissenschaftliche Entwicklung unserer
          Gesellschaft bilden. Sie fördern kritisches Denken,
          Problemlösungsfähigkeiten und kreatives Innovieren. Aufgrund der
          unterschiedlichen Schulsysteme zwischen den Bundesländern sind
          Vergleiche zum Umfang des MINT-Unterrichts schwierig aufzustellen.
          Für die Schulform, die es neben der Grundschule in jedem Bundesland
          gibt, das Gymnasien, konnten die Studentafeln ausgewertet werden. Das
          Ergebnis: ", tags$b("Im Schnitt machen Mathematik, der
          Naturwissenschaftliche Bereich und Informatik 26,1 Prozent der Stunden
          an staatlichen Gymnasien aus."), " In Sachsen sind es 28,5 Prozent, in
          Hamburg sind es 21,8 Prozent."
        ),
        br(),
        fluidRow(
          plotlyOutput(ns("Grafik_mint_teilhabe_stundentafeln"))
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_mint_teilhabe_studentafeln"))
      )

    } else if(indikator_ID == "teilhabe_lehrkraefte"){

      #####2 lehrkraefte informatik----

      fluidPage(
        p(
          "Für einen guten MINT-Unterricht braucht es engagierte Lehrkräfte in
          diesem Bereich. Leider weisen die öffentlich verfügbaren Statistiken
          der Kultusministerkonferenz (KMK) die Lehrkäfte nicht nach Fächern der
          Lehrbefähigung aus, sodass wir - mit Ausnahme einzelner Landesstudien
          (bspw. Klaus Klemm zu NRW #TODO Link) keinen Indikator zu den
          MINT-Lehrkräften aufstellen können. Für das Fach Informatik haben
          Gesellschaft für Informatik, Stifterverband und Heinz Nixdorf Stiftung
          jedoch entsprechende Daten gesammelt und festgestellt, dass ", tags$b(
          "etwa 2 Prozent der Lehrkräfte Informatiklehrkräfte sind"),". Für
          einen Informatikunterricht von6 Wochenstunden wären voraussichtlich 6
          bis 7 Prozent nötig."
        ),
        br(),
        fluidRow(
          uiOutput(ns("Tabelle_mint_teilhabe_lehrkraefte"))
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_mint_teilhabe_lehrkraefte"))
      )

    } else if (indikator_ID == "teilhabe_oberstufe") {

      #####3 oberstufe mint ----

      fluidPage(
        p(
          "Das Belegen von MINT-Fächern (Mathematik, Informatik,
          Naturwissenschaften und Technik) in den Oberstufen-Leistungskursen ist
          entscheidend, es fördert den interdisziplinären Austausch im späteren
          Berufsleben. Auch Schüler, die sich später für nicht-MINT-Berufe
          entscheiden, profitieren von einem tieferen Verständnis der
          MINT-Themen. Dies erleichtert die Kommunikation und Zusammenarbeit
          zwischen MINT-Experten und Fachleuten aus anderen Bereichen, was für
          den Fortschritt in zahlreichen Industrien und Wissenschaften
          unerlässlich ist. So tragen MINT-Leistungskurse zur Förderung einer
          wissensbasierten und innovationsfreudigen Gesellschaft bei. In den
          Bundesländern ist unterschiedlich stark vorgegeben inwieweit
          MINT-Leistungskurse zu belegen sind. "
        ),
        p(
          tags$b("Derzeit sind ein Drittel aller Leistungskursbelegungen dem
          MINT-Bereich zuzuordnen, eine seit 2016 leicht fallender Tendenz seit
          2016."), " Die hinterlegten Daten stammen von der KMK."),
        div(class = "small-font",
            tabsetPanel(
              type = "tabs",
              tabPanel("Balkendiagramm",

                       column(
                         width = 3,
                         uiOutput(ns("menu_mint_oberstufe"))
                       ),
                       column(
                         width = 9,
                         br(),
                         withSpinner(highchartOutput(ns("plot_mint_oberstufe")))
                       )
              ),
              tabPanel("Zeitverlauf",
                       column(
                         width = 3,
                         uiOutput(ns("menu_mint_oberstufe_verlauf"))
                       ),
                       column(
                         width = 9,
                         br(),
                         withSpinner(highchartOutput(ns("plot_mint_oberstufe_verlauf")))
                       )

              )
            )
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_mint_teilhabe_oberstufe"))
      )


    } else if (indikator_ID == "teilhabe_oberstufen_frauen") {

      #####4 oberstufe mint frauen----

      fluidPage(
        p(
          "Es ist Ziel, dass mehr Frauen MINT-Leistungskurse wählen, um die
          Geschlechtervielfalt in diesen Bereichen zu erhöhen und damit ein
          ausgewogeneres und innovativeres Arbeitsumfeld zu schaffen. Frauen
          sind in MINT-Berufen noch immer unterrepräsentiert, was zu einem
          Verlust von Potenzial und unterschiedlichen Perspektiven führt. Aus
          den Daten zur Oberstufenbelegung der KMK zeigt sich, dass im
          aktuellsten Berichtsjahr 2022 ", tags$b("etwa 47 Prozent aller
          Leistungskursbelegungen im MINT-Bereich von Frauen sind, in anderen
          Fächern liegt der Anteil bei 57 Prozent.")
        ),
        div(class = "small-font",
            tabsetPanel(
              type = "tabs",
              tabPanel("Balkendiagramm",

                       column(
                         width = 3,
                         uiOutput(ns("menu_mint_oberstufe_frauen"))
                       ),
                       column(
                         width = 9,
                         br(),
                         withSpinner(highchartOutput(ns("plot_mint_oberstufe_frauen")))
                       )
              ),
              tabPanel("Zeitverlauf",

                       column(
                         width = 3,
                         uiOutput(ns("menu_mint_oberstufe_frauen_verlauf"))
                       ),
                       column(
                         width = 9,
                         br(),
                         withSpinner(highchartOutput(ns("plot_mint_oberstufe_frauen_verlauf")))
                       )
              ),
            )
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_mint_teilhabe_oberstufe_frauen"))
      )

    } else if (indikator_ID == "teilhabe_informatik") {

      #####5 pflichtfach informatik----

      fluidPage(
        p(
          "Ein Pflichtfach Informatik in der Schule ist von großer Bedeutung, da
          die digitale Kompetenz in der heutigen Welt unerlässlich ist. In
          nahezu allen Berufsfeldern sind Kenntnisse in Informatik und der
          Umgang mit digitalen Technologien erforderlich. Ein Pflichtfach würde
          sicherstellen, dass alle Schülerinnen und Schüler grundlegende
          Fähigkeiten in Programmierung, Datenverarbeitung und Cybersicherheit
          erwerben, was ihre beruflichen Chancen erheblich verbessert. Durch ein
          verpflichtendes Informatikfach werden Schülerinnen und Schüler nicht
          nur zu kompetenten Nutzerinnen und Nutzern, sondern auch zu kreativen
          Gestalterinnen und Gestaltern der digitalen Zukunft. Gefordert werden
          vom Stifterverband 6 Wochenstunden Informatik.", tags$b("Dies ist
          bisher bisher nur in 2 der 16 Bundesländer Realität.")
        ),
        br(),
        fluidRow(
          plotlyOutput(ns("Grafik_mint_teilhabe_pflichtfach_inf"))
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_mint_teilhabe_pflichtfach_inf"))
      )
    }

    #lehrkraefte ---------------------------------------------------------------

  } else if(startsWith(indikator_ID, "fs")){

    ###### future skills ----

    if(indikator_ID == "fs_digital_lehramt"){

      ######1 medienkompetenz ----

      fluidPage(
        p(
          "Der Erwerb von Medienkompetenz sollte in der Ausbildung von
          Lehrkräften verpflichtend sein, damit sie Schülerinnen und Schüler
          effektiv und verantwortungsbewusst im Umgang mit digitalen Medien
          anleiten können. In den Jahren 2017 und 2022 wurden die Hochschulen im
          Rahmen der Erhebungen zum Monitor Lehrkräftebildung danach gefragt,
          ob es für Lehramtsstudierende verpflichtend im Curriculum verankerte
          Angebote zum Erwerb von „Medienkompetenz in einer digitalen Welt“ gibt
          . Hierbei wurde nicht nach dem Ort der curricularen Verankerung
          gefragt. Verpflichtende Angebote können also in den
          Bildungswissenschaften und/oder Fachdidaktiken und/oder
          Fachwissenschaften verankert sein. Seit dem Jahr 2017 sind hier viele
          Verbesserungen eingetreten, bei jeder Schulform gab es einen
          deutlichen Anstieg, i. d. R. von etwa 15 Prozent auf jeweils 50 bis
          63,6 Prozent. Dieser Wert sollte sich jedoch weiter erhöhen.
          "
        ),
        fluidRow(
          plotlyOutput(ns("Grafik_fs_digital_lehramt"))
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_fs_digital_lehramt"))
      )

    } else if (indikator_ID == "fs_mint") {

      ######2 lehramt-mint -----

      fluidPage(
        p(
          "Die Entwicklung der MINT-Lehramtsausbildung (Mathematik, Informatik,
          Naturwissenschaften, Technik) ist von zentraler Bedeutung für die
          Bildungslandschaft und die technologische Zukunftsfähigkeit einer
          Gesellschaft. Länderspezifische Analysen zeigen, dass gerade im
          MINT-Bereich in den kommenden Jahren Lehrkräfte fehlen werden (vgl. u.
          a. Klemm 20XX).", tags$b("Der Anteil an MINT-Studierenden unter allen
          Lehramtsstudierenden verbleibt jedoch relativ konstant über die Jahre
          bei 23,3 bis 24,3 Prozent.")
        ),
        fluidRow(
          #Auswahlmöglichkeit Jahr
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
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_lehrkraefte_fs_mint"))
      )
    }
  } else if(startsWith(indikator_ID, "bedingungen")) {

    ###### bedingungen ----

    if(indikator_ID == "bedingungen_praxis"){

      #####1 praxisbezug -----

      fluidPage(
        p(
          "Der Praxisbezug im Lehramt und das Praxissemester sind entscheidend,
          um angehenden Lehrkräften die Möglichkeit zu geben, theoretisches
          Wissen in realen Unterrichtssituationen anzuwenden und wertvolle
          Erfahrungen zu sammeln. Durch praktische Einsätze können zukünftige
          Lehrer ihre pädagogischen Fähigkeiten entwickeln, Unterrichtsmethoden
          erproben und ein besseres Verständnis für den Schulalltag und die
          Bedürfnisse der Schüler gewinnen. Diese praxisnahen Phasen tragen
          wesentlich dazu bei, die Qualität der Lehrerausbildung zu erhöhen und
          die Berufsbereitschaft der Absolventen zu verbessern. Es gibt
          mittlerweile in allen Bundesländern die Vorgaben zu Praxisphasen. ",
          tags$b("Das Praxissmester ist aktuelle in 7 von 16 Bundesländern
          etabliert.")
        ),
        fluidRow(
          column(
            width = 3,
            renderUI(ns("Auswahlmoeglichkeiten_bedingungen_praxis"))
          ),
          column(
            width = 9,
            renderPlotly(ns("Grafik_leahramt_bedingungen_praxis"))
          )
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_lehrkraefte_bedingungen_praxis"))
      )

    } else if (indikator_ID == "bedingungen_experimentierklauseln") {

      #####2 experimentierklausel -----

      fluidPage(
        p(
          "Das Hinzufügen einer Experimentierklausel zu den Rechtsgrundlagen für
          die Lehrkräftebildung in den Bundesländern ist von entscheidender
          Bedeutung, um innovative und flexible Ansätze in der
          Lehrkräfteausbildung zu fördern. Solche Klauseln ermöglichen es
          Bildungseinrichtungen und Lehramtsstudiengängen, neue pädagogische
          Konzepte, Lehrmethoden und Ausbildungsformate unter realen Bedingungen
          zu erproben, was letztlich zu einer höheren Qualität der
          Lehrkräfteausbildung und damit auch des gesamten Bildungssystems
          beiträgt. Derzeit haben jedoch lediglich vier Bundesländer (Bayern,
          Berlin, Brandenburg und Mecklenburg-Vorpommern) entsprechende
          Klauseln hinterlegt und auch dort bleiben diese hinsichtlich der
          Konkretisierung und Verbindlichkeit hinter den Experimentierklauseln
          in den vergleichbaren Ärztlichen Approbationsordnungen zurück.
          "
        ),
        fluidRow(
          column(
            width = 12,
            plotlyOutput(ns("Grafik_leahramt_bedingungen_experimentierklauseln"))
          )
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_lehrkraefte_bedingungen_experimentierklauseln"))
      )


    } else if (indikator_ID == "bedingungen_universitaetsschulen") {

      #####3 universitaetsschulen -----

      fluidPage(
        p(
          "Die praktische Ausbildung angehender Medizinerinnen und Mediziner
          erfolgt im Wesentlichen an Universitätskliniken, die für eine
          Hochleistungsmedizin stehen, beziehungsweise an Akademischen
          Lehrkrankenhäusern, die dafür bestimmte Qualitätsstandards erfüllen
          müssen. Analog dazu sollten gemäß des Masterplans Lehrkräfte auch für
          angehende Lehrkräfte Universitätsschulen errichtet werden, die
          pädagogische und didaktische Innovationen entwickeln und dafür sorgen,
          dass wissenschaftliche Erkenntnisse möglichst schnell in der Praxis
          ankommen. Universitätsschulen bieten der Wissenschaft umgekehrt ein
          „Reallabor“ für die Forschung. Beispiele hierfür sind die Laborschule
          Bielefeld, die Universitätsschule Dresden oder die Heliosschule in
          Köln. Bislang gibt es jedoch lediglich XX solcher Schulen, bei XXX
          Universitäten mit Lehramtsausbildung."
        ),
        fluidRow(
          column(
            width = 12,
            plotlyOutput(ns("Grafik_leahramt_bedingungen_universitaetsschulen"))
          )
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_lehrkraefte_bedingungen_universitaetsschulen"))
      )

    }
  } else if (startsWith(indikator_ID, "flexibilisierung")){

    ###### flexibilisierung ----

    if(indikator_ID == "flexibilisierung_haws"){

      #####1 haws -----

      fluidPage(
        p(
          "Analog zu den Lehramtsstudiengängen für Kunst, Musik und Sport
          sollten die HAWs eigenständige Lehramtsstudiengänge für die
          beruflichen Fachrichtungen einrichten können. An den HAWs studieren
          häufiger als an den Universitäten und Pädagogischen Hochschulen
          Personen mit Berufsschulabschluss, die mit dieser Schulform vertraut
          sind und vor ihrem Studium eine einschlägige Berufsausbildung
          absolviert haben. Bei den Lehramtsstudiengängen für die beruflichen
          Fachrichtungen gibt es  zwar an etlichen Standorten Kooperationen
          zwischen Universitäten und HAWs, so erhalten Personen, die an einer
          HAW einen Abschluss erworben haben, die Möglichkeit, unter bestimmten
          Voraussetzungen ein lehramtsbezogenes Masterstudium zu beginnen. Die
          HAWs sind aber bisher nicht systematisch in die Lehrkräftebildung für
          die beruflichen Schulen einbezogen, und sie sind, anders als die
          Kunst und Musikhochschulen, keine gleichberechtigten Partner der
          Universitäten und Pädagogischen Hochschulen. Noch ist in ",
          tags$b("keinem Bundesland"), " eine derartige Flexibilisierung der
          Lehramtsausbildung gesetzlich umgesetzt"
        ),
        fluidRow(
          plotlyOutput("Grafik_leahramt_flexibilisieurng_haws")
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_lehrkraefte_flexibilisieurng_haws"))
      )

    } else if(indikator_ID == "flexibilisierung_einfach_bachelor"){

      #####2 einfach-bachelor -----


      fluidPage(
        p(
          "Trotz aller länderspezifischen Unterschiede im Detail gibt es
          bundesweit im Prinzip nur einen Weg in den Schuldienst: Lehrkraft kann
          nur werden, wer ein grundständiges Lehramtsstudium mit mindestens zwei
          Unterrichtsfächern sowie den anschließenden Vorbereitungsdienst
          absolviert hat. Ausnahmen von dieser Regel werden bisher nur gemacht,
          wenn der Lehrkräftebedarf mit ordnungsgemäß qualifizierten
          Bewerberinnen und Bewerbern nicht gedeckt werden kann. Personen, die
          sich nicht zu Studienbeginn für ein lehramtsbezogenes Studium
          entscheiden können oder wollen, gehen dadurch dem Pool potenzieller
          Lehrkräfte tendenziell verloren."
        ),
        p(
          "Die Einführung eines Ein-Fach-Studiums als zusätzliche Alternative
          würde die Durchlässigkeit zwischen fach- und lehramtsbezogenen
          Studiengängen erhöhen und die Anerkennung internationaler
          Lehramtsabschlüsse erleichtern.", tags$b("Derzeit ist dies jedoch in
          keinem Bundesland umgesetzt.")
        ),
        fluidRow(
          plotlyOutput(Grafik_leahramt_flexibilisieurng_einfach_bachelor)

        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_lehrkraefte_flexibilisieurng_einfach_bachelor"))
      )

    } else if(indikator_ID == "flexibilisierung_trichter") {

      #####3 trichter -----


      fluidPage(
        p(
          "Wie viele Lehrmatsstudierende werden später auch Lehrkraft? Mit
          dieser Frage beschäftigt sich der Lehrkräftetrichter des
          Stifterverbandes. Er schafft eine erste Grundlage, indem er in einem
          Querschnitt Schwund-Tendenzen sichtbar macht und diese einordnet. Um
          dem Lehrkräftemangel begegnen zu können, müssen die großen
          Schwundquoten am Anfang des Studiums, aber auch in den späteren Phasen
          der Lehramtsausbildung – insbesondere im MINT-Bereich – reduziert
          werden. Des Weiteren müssen Zugänge und Wechsel zum Lehramt
          flexibilisiert werden und die Ausbildungsqualität in allen drei Phasen
          verbessert werden. Der Stifterverband appelliert deswegen an die
          zuständigen Ministerinnen und Minister, zügig die Lehrkräftebildung
          für neue Zielgruppen zu öffnen und geeignete Maßnahmen zu treffen, um
          (angehende) Lehrkräfte in der Ausbildung und im Beruf zu halten. ",
          tags$b("Aktuell zeigt der Querschnitt aus 5 Jahren, dass jährlich etwa
          52.500 Studierende ein Lehramtsstudium beginnen und jährlich etwa
          28.300 Menschen das Referendariat beenden.")
        ),
        fluidRow(
          uiOutput(ns("UI_lehrkraefte_flexibilisierung_trichter"))
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_lehrkraefte_flexibilisieurng_trichter"))
      )

    }

#fs ---------------------------------------------------------------

  } else if(startsWith(indikator_ID, "ki_")) {

    ###### ki ----

    if (indikator_ID == "ki_studiengänge") {

      #####1 ki-studiengaenge -----

      fluidPage(
        p(
          "Auf Data Science und KI spezialisierte Studiengänge bringen
          Fachkräfte hervor, die in der Lage sind, komplexe Datenanalysen
          durchzuführen und innovative KI-Lösungen zu entwickeln. Diese
          Fähigkeiten sind in der modernen Wirtschaft und Forschung
          unverzichtbar, um Wettbewerbsvorteile zu erzielen und technologische
          Fortschritte zu ermöglichen. Darüber hinaus tragen solche Studiengänge
          dazu bei, ethische und verantwortungsvolle Anwendung von
          KI-Technologien zu fördern. Der Anteil an derartigen Studiengängen ist
          massiv gestiegen in den letzten Jahren, zuletzt konnten knapp 400
          derartige Studienangebote im Hochschulkompass der
          Hochschul-Rektoren-Konferenz ausgemacht werden - im Jahr 2017 waren es
          noch 132 gewesen.
          "
        ),
        fluidRow(
          plotlyOutput(ns("Grafik_fs_ki_studiengaenge"))
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_fs_ki_studiengaenge"))
      )

    } else if(indikator_ID == "ki_veranstaltungen") {

      #####2 ki-veranstaltungen -----

      fluidPage(
        p(
          "Der Anteil an Veranstaltungen zum Thema Künstliche Intelligenz (KI)
          im Gesamtveranstaltungsangebot einer Hochschule sollte dem wachsenden
          Bedarf an Fachkenntnissen in diesem Bereich gerecht werden. Angesichts
          der rasanten technologischen Entwicklungen und der zunehmenden
          Bedeutung von KI in verschiedenen Industriezweigen ist es essenziell,
          dass Studierende umfassend und praxisnah auf diese Herausforderungen
          vorbereitet werden. In der Datenbank Higher Education Explorer (HEX)
          führt der Stifterverband aktuell die Vorlesungsverzeichnisse der
          Hochschulen zu einem Forschungsdatensatz zusammen. Es finden sich
          aktuell 21 Universitäten in der Datenbank, bei denen etwa ein Viertel
          aller Studierender eingeschrieben ist. In dieser Gruppe machen
          Veranstaltungen mit KI-Bezug etwa X,X Prozent der Kurse aus insgesamt
          aus. Der Anteil ist in den vergangenen Jahren stark gestiegen.
          "
        ),
        fluidRow(
          plotlyOutput(ns("Grafik_fs_ki_veranstaltungen"))
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_fs_ki_veranstaltungen"))
      )

    } else if(indikator_ID == "ki_einschaetzung") {

      #####3 ki-einschaetzung -----

      fluidPage(
        p(
          "Die Selbsteinschätzung im Umgang mit Künstlicher Intelligenz (KI) ist
          ein Indikator, der das Vertrauen und die Kompetenz einer Person im
          Umgang mit KI-Technologien reflektiert. Sie umfasst die Fähigkeit,
          die Funktionsweise von KI zu verstehen, die Auswirkungen auf
          Arbeitsprozesse zu bewerten und ethische sowie technische
          Herausforderungen zu erkennen und zu adressieren. Eine hohe
          Selbsteinschätzung zeigt an, dass die Person sich sicher fühlt,
          KI-Anwendungen zu nutzen, kritisch zu hinterfragen und in bestehende
          Arbeitsabläufe zu integrieren. Selbsteinschätzungen weichen häufig und
          im relevanten Maße von den tatsächlichen Fähigkeiten ab, bieten jedoch
          dennoch einen ersten Anhaltspunkt. Derzeit empfinden sich ", tags$b("
          etwa XX Prozent der Bevölkerung kompetent im Umgang mit KI."),
          " Die Daten stammen von XXX."
        ),
        fluidRow(
          plotlyOutput(ns("Grafik_fs_ki_einschaetzung"))
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_fs_ki_einschaetzung"))
      )

    }


  } else if(startsWith(indikator_ID, "verankern_")) {

    ###### verankern ----

    if (indikator_ID == "verankern_tech_skills") {

      #####1 tech skills -----

      fluidPage(
        p(
          "Die Vermittlung von sogenannten Future Skills, d. h. Fähigkeiten und
          Kompetenzen, die an Bedeutung gewinnen, in der Hochschullehre wird als
          ein wichter Baustein gesehen, damit mehr Menschen die vor uns allen
          liegenden Transformationen gut gestaltet werden können. Im Rahmen des
          Forschungsprojektes Higher Education Explorer (HEX) vom Stifterverband
          wurden die Vorlesungsverzeichnisse von derzeit 22 Universitäten
          homogenisiert zu einem Datensatz. Gestützt durch KI wurden Kurse
          klassifiziert, die expliziet technologische Future Skills vermitteln.
          Der Anteil an Veranstaltungen, auf den dies zutraf stieg in der Gruppe
          dieser 22 Universitäten vergangenen Jahren deutlich an von X,X Prozent
          im Studienjahr 2017 auf X,X Prozent im Studienjahr 2023. Für eine
          genauere Beschreibung wird die Datenbank zum HEX derzeit ausgebaut."
        ),
        fluidRow(
          plotlyOutput(ns("Grafik_fs_verankern_tech_skills"))
        ),
        p("Anmerkungen:", class = "anmerkungen"),
        uiOutput(ns("Anmerkungen_fs_verankern_tech_skills"))
      )
    }
  }
}

# HELPER FUNCTIONS -----

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

    df <- tbl(con, from = "studierende_detailliert") %>%
      filter(mint_select == "MINT"  | fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer")) %>%
      select(fach)%>%
      collect()

    df <- df %>%
      unique()%>%
      as.vector()%>%
      unlist()%>%
      unname()

    df <- sort(df)

  } else if (missing(spezif_i)){

    df <- tbl(con, from = "studierende_detailliert") %>%
      filter(mint_select == "MINT"  | fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer"))%>%
      filter(region %in%  spezif_r) %>%
      collect()

    df <- df %>%select(fach)%>%
      unique()%>%
      as.vector()%>%
      unlist()%>%
      unname()

    df <- sort(df)

  } else if(missing(spezif_r)){

    df <- tbl(con, from = "studierende_detailliert") %>%
      filter(mint_select == "MINT"  | fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer"))%>%
      filter(indikator %in%  spezif_i) %>%
      collect()

    df <- df %>%select(fach)%>%
      unique()%>%
      as.vector()%>%
      unlist()%>%
      unname()

    df <- sort(df)

  }
}

