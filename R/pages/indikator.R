box::use(
  ../../R/utils/database[get_query, get_sql],
  ../../R/utils/ui[
    draw_zurueck_button, draw_save_and_share_buttons,
    add_info, add_tooltip,
    get_picker_options,
    draw_progress, get_progress, get_waiter
  ],
  ../../R/utils/charts[produce_plot, count_possible_plots],
  ../../R/utils/reactable[get_reactable_lang, get_reactable_theme],
  ../../R/pkgs/wrangling/get_data[get_data],
  shiny[
    NS, moduleServer, observeEvent,
    HTML, tagList, div, h2, h3, p, icon, tags,
    reactiveVal, reactiveValues, reactiveValuesToList,
    fluidPage, actionButton, uiOutput, renderUI,
    span
  ],
  shinyWidgets[pickerInput, updatePickerInput, sliderTextInput],
  shiny.router[get_query_param, change_page, get_page],
  shinycssloaders[withSpinner],
  reactable[reactable, reactableOutput, renderReactable, colDef, reactableTheme, reactableLang],
  sortable[bucket_list, add_rank_list],
  plotly[plotlyOutput, renderPlotly],
  utils[URLencode, URLdecode],
  urltools[param_get, param_set],
  yaml[read_yaml],
  shinyjs[removeCssClass, addCssClass, runjs]
)

# Globals:
URL_PATH             <- "indikator"
LABEL_TABSET_TABELLE <- "Tabelle"
LABEL_TABSET_GRAFIK  <- "Abbildung"

#' @export
get_globals <- function(){
  list(
    url_path             = URL_PATH,
    label_tabset_tabelle = LABEL_TABSET_TABELLE,
    label_tabset_grafik  = LABEL_TABSET_GRAFIK
  )
}

#' @export
module_ui <- function(id = URL_PATH, label = paste0(URL_PATH, "_m")) {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",

      # 1. Kopf ________________________________________________________________________________________________________
      div(
        id = ns("kopf"),
        style = "display: flex; flex-direction: row; justify-content: space-between;",
        div(
          id    = ns("triangle"),
          class = "keine_handlung_triangle",
          style = "width: 200px; height: auto; min-width: 200px; min-height: 200px;"
        ),
        uiOutput(
          style = "text-align: center; margin: auto 0; width: 100%; padding-right: 40%; padding-left: 40px;",
          ns("ueberschrift")
        ),
        div(draw_zurueck_button())
      ),

      #2. Menü__________________________________________________________________________________________________________
      div(
        id = ns("menu"),
        style = "padding: 10px; display: flex; margin: 0; min-height: 500px;",
        div(
          class = "content-box",
          style = "width: 100%; padding-top: 20px; position: relative",
          div(
            style = "display: flex; flex-direction: row; flex-wrap: wrap; padding: 16px 20px 1px 20px; align-items: center;",
            pickerInput(
              inputId  = ns("select_tag"),
              label    = "Filter für Themen:",
              choices  = c(""),
              options  = get_picker_options(`actions-box` = TRUE),
              multiple = TRUE
            ) |>
              add_tooltip("<h4>Variablen filtern</h4><p>Mit der Auswahl von Tags wird die <b>Auswahl</b> der Variablen <b>gefiltert</b>. Hierdurch kann man die Auswahl der Variablen übersichtlicher gestalten. Wenn kein Tag aus gewählt ist, werden alle Variablen in der Auswahl angezeigt.</p>"),
            actionButton(
              ns("reset"),
              icon  = icon("repeat", class = "fa-solid"),
              label = "",
              class = "button_icon",
              style = "margin: auto 0px 14px 0px;"
            ) |>
              add_tooltip("<h4>Zurücksetzen</h4><p>Mit dem Button setzen Sie alle Ihre Einstellungen auf dieser Seite zurück.</p>"),
            add_info(write_hilfe(), style = "margin: auto 0px 14px 0px;")
          ),
          div(
            style = "padding: 1px 20px 1px 20px; display: flex; flex-direction: row; flex-wrap: wrap; align-items: center;",
            div(
              style = "display: flex; flex-direction: column; margin-right: 40px",
              pickerInput(
                inputId  = ns("select_variable"),
                label    = "Variablen:",
                choices  = c(""),
                options  = get_picker_options(`max-options` = 1),
                multiple = TRUE
              )
            ),
            div(
              id = ns("variable2"),
              style = "margin-right: 40px; display: flex; flex-direction: column; align-items: flex-start;",
              div(
                style = "display: flex; align-items: center; margin-top: -6px;",
                tags$label("Vergleichen mit:", style = "margin: 0; margin-right: 10px;", class = "control-label"),
                actionButton(
                  ns("button_variable2_plus"),
                  icon  = icon("square-plus", class = "fa-solid"),
                  label = "",
                  class = c("button_icon", "no_display")
                ) |>
                  add_tooltip("<h4>Vergleichen einblenden</h4><p>Sollen Variablen für den Vergleich zur Verfügung gestellt werden?</p>"),
                actionButton(
                  ns("button_variable2_minus"),
                  icon  = icon("square-minus", class = "fa-solid"),
                  label = "",
                  class = c("button_icon", "no_display")
                ) |>
                  add_tooltip("<h4>Vergleich ausblenden</h4><p>Sollen die Vergleichsvariablen wieder ausgeblendet werden?</p>")
              ),
              div(
                id = ns("variable2_select_div"),
                pickerInput(
                  inputId  = ns("select_variable2"),
                  label    = NULL,
                  choices  = c(),
                  selected = c(),
                  options  = get_picker_options(`actions-box` = TRUE),
                  multiple = TRUE
                )
              )
            ),
            uiOutput(ns("select_zeit"), style = "margin-right: 40px")
          ),
          uiOutput(ns("select_gruppe"), style = "max-width: 650px; width: 80%;"),
          uiOutput(ns("select_filter"), style = "max-width: 250px; width: 100%; margin-left: 20px; margin-top: 25px; display: flex; flex-direction: row;"),
          div(draw_balken(), style = "position: absolute; right: 10px; top: 150px;")
        )
      ),
      div(
        style = "padding: 0 20px;",
        draw_progress(ns)
      ),
      #3. Inhalt__________________________________________________________________________________________________________
      div(
        id    = ns("ergebnis_box"),
        style = "margin-top: 24px; min-height: 1000px; min-height = 600px",
        class = c("tabset-menu", "no_display"),
        div(
          style = "display: flex; flex-direction: row; justify-content: space-between; border-bottom: 3px solid var(--mint);",
          div(
            style = "margin-top: auto;",
            render_buttons(ns = ns)
          ),
          draw_save_and_share_buttons(ns)
        ),
        uiOutput(ns("ergebnisse"))
      ),
      draw_hilfe(ns)
    )
  )
}

#' @export
module_server <- function(id = URL_PATH){
  moduleServer(
    id,
    function(input, output, session) {

      # - Alle reaktiven Elemente:
      ns <- session$ns
      parameter <-
        reactiveValues(
          hf        = "start",
          tags      = c(),
          variable  = "",
          variable2 = c(),
          zeit      = c(),
          gruppe    = c(),
          filter    = "",
          tab       = 0
        )
      filter_aenderung <- reactiveVal(1)

      waiter   <- get_waiter(ns, "ergebnis_box")
      progress <- get_progress(ns)

      # - Das zentrale Event bezieht sich auf die URL-Parameter;
      #   auf dieser Basis werden alle Anpassungen vorgenommen

      observeEvent(
        get_query_param(), {

          if (get_page() %in% URL_PATH){

            progress$set(0)
            waiter$show()

            current_url <- session$clientData$url_hash
            aenderung   <- c()
            start       <- parameter$hf %in% "start"

            ##### 1. Schritt: Parameter an aktuelle Auswahl auf Basis der URL-Parameter ############
            #####             anpassen!                                                 ############

            # PARAMETER HF - Update des Handlungsfeldes / der Themenauswahl ________________________

            parameter_hf <-
              get_query_param("hf") |>
              recode_parameter_int(default = 0)

            if (parameter_hf != parameter$hf){
              parameter_hf        <- check_parameter_hf(parameter_hf)
              parameter$hf        <- parameter_hf
              output$ueberschrift <- render_head(parameter_hf)
              aenderung           <- c(aenderung, "hf")
            }
            if (length(aenderung) > 0) progress$set(1)

            # PARAMETER tag - Update der Tags ______________________________________________________

            parameter_tag <-
              "tag" |>
              get_query_param() |>
              recode_parameter_int(vec = TRUE)

            if (diverge_two_vectors(parameter_tag, parameter$tag) | "hf" %in% aenderung){

              tags           <- get_tags_by_hfs(hf = parameter_hf)
              parameter_tag  <- parameter_tag[parameter_tag %in% tags$id]
              parameter$tag  <- parameter_tag
              aenderung      <- c(aenderung, "tag")

              if ("hf" %in% aenderung){
                updatePickerInput(
                  session  = session,
                  inputId  = "select_tag",
                  choices  = sort(tags$beschr),
                  selected = tags$beschr[match(parameter_tag, tags$id)]
                )
              }
              rm(tags)
            }
            if (length(aenderung) > 0) progress$set(2)

            # PARAMETER variable - Update der Variablen ____________________________________________

            parameter_variable <-
              "variable" |>
              get_query_param() |>
              recode_parameter_int()

            if (parameter_variable != parameter$variable | "tag" %in% aenderung){

              parameter_variable <- check_parameter_variable(parameter_variable)

              if (length(parameter_tag) < 0){
                variables <- get_variables(get_tags_by_hfs(hf = parameter_hf)$id)
              } else {
                variables <- get_variables(parameter_tag)
              }

              if (!(parameter_variable %in% variables$id)){
                parameter_variable <- ""
                selected           <- c()
              } else {
                selected           <- variables$beschr[match(parameter_variable, variables$id)]
              }

              if (parameter_variable %in% ""){
                removeCssClass("hilfe", "no_display")
              } else {
                addCssClass("hilfe", "no_display")
              }

              if ("tag" %in% aenderung | parameter_variable %in% ""){
                updatePickerInput(
                  session  = session,
                  inputId  = "select_variable",
                  choices  = sort(variables$beschr),
                  selected = selected
                )
              }

              parameter$variable <- parameter_variable
              aenderung          <- c(aenderung, "variable")
              rm(selected, variables)
            }
            if (length(aenderung) > 0) progress$set(3)

            # PARAMETER variable2 - Update der Variablen zum Vergleich _____________________________

            parameter_variable2 <-
              get_query_param("variable2") |>
              recode_parameter_int(TRUE)

            if (diverge_two_vectors(parameter_variable2, parameter$variable2) | "variable" %in% aenderung){

              if (parameter_variable %in% ""){
                addCssClass("variable2", "no_display")
                addCssClass("variable2_select_div", "no_display")
                addCssClass("button_variable2_minus", "no_display")
                parameter_variable2 <- c()
                choices             <- ""
                selected            <- c()
              } else {
                removeCssClass("variable2", "no_display")
                variable2           <- get_possible_variable2(parameter_variable, parameter_hf, parameter_tag) #TODO zu langsam
                parameter_variable2 <- parameter_variable2[parameter_variable2 %in% variable2$id]
                choices             <- variable2$beschr
                selected            <- variable2$beschr[variable2$id %in% parameter_variable2]
                rm(variable2)
              }

              if ("variable" %in% aenderung & length(parameter_variable2) == 0){
                if (start){
                  removeCssClass("button_variable2_plus", "no_display")
                  addCssClass("variable2_select_div", "no_display")
                }
                updatePickerInput(session = session, inputId = "select_variable2", selected = "", choices = choices)
              } else if (start) {
                removeCssClass("button_variable2_minus", "no_display")
                updatePickerInput(session = session, inputId = "select_variable2", selected = selected, choices = choices)
              }

              parameter$variable2 <- parameter_variable2
              aenderung           <- c(aenderung, "variable2")
              rm(choices, selected)
            }
            if (length(aenderung) > 0) progress$set(4)

            # PARAMETER zeit - Update des Zeit-Sliders _____________________________________________

            parameter_zeit <-
              get_query_param("zeit") |>
              recode_parameter_int(vec = TRUE)

            if (diverge_two_vectors(parameter_zeit, parameter$zeit) | "variable" %in% aenderung){

              if (parameter_variable %in% ""){
                parameter_zeit     <- c()
                output$select_zeit <- renderUI({HTML("")})
              } else {
                zeit <- get_zeit_by_variable_id(parameter_variable)
                if (!all(parameter_zeit %in% zeit$min:zeit$max) | !(length(parameter_zeit) %in% 1:2)){
                  parameter_zeit <- c(zeit$min, zeit$max)
                }
                output$select_zeit <- render_zeit_slider(zeit, parameter_zeit, ns)
              }

              parameter$zeit <- parameter_zeit
              aenderung      <- c(aenderung, "zeit")
            }
            if (length(aenderung) > 0) progress$set(5)

            # PARAMETER gruppe - Update der Gruppenauswahl / "Eimer" _______________________________

            parameter_gruppe <-
              get_query_param("gruppe") |>
              recode_parameter_int(vec = TRUE)

            if (diverge_two_vectors(parameter_gruppe, parameter$gruppe) | "variable" %in% aenderung){

              if (parameter_variable %in% ""){
                parameter_gruppe     <- c()
                output$select_gruppe <- renderUI({HTML("")})
              } else {
                gruppen              <- get_gruppe_by_variable_id(parameter_variable)
                parameter_gruppe     <- parameter_gruppe[parameter_gruppe %in% gruppen$id]
                output$select_gruppe <- render_eimer(gruppen$beschr, gruppen$beschr[gruppen$id %in% parameter_gruppe], ns)
              }

              parameter$gruppe <- parameter_gruppe
              aenderung          <- c(aenderung, "gruppe")
            }

            # PARAMETER der Filter _________________________________________________________________

            parameter_filter <-
              get_query_param("filter") |>
              recode_parameter_int(vec = TRUE)

            if (!is.null(parameter_filter)){
              if (all(parameter_filter %in% "")){
                parameter_filter <- c()
              }
            }

            if (diverge_two_vectors(parameter_filter, parameter$filter) | "gruppe" %in% aenderung){
              if (length(parameter_filter) > 0){
                parameter_filter <- check_filter(parameter_filter, parameter_gruppe)
                if ("gruppe" %in% aenderung){
                  output$select_filter <-
                    render_filter(
                      parameter_variable,
                      parameter_gruppe,
                      parameter_filter,
                      ns
                    )
                }
              } else if (length(parameter_gruppe) > 0 & parameter_variable != "" & "gruppe" %in% aenderung){
                output$select_filter <-
                  render_filter(
                    parameter_variable,
                    parameter_gruppe,
                    NULL,
                    ns
                  )
              }

              parameter$filter <- parameter_filter
              aenderung <- c(aenderung, "filter")
            }
            if (length(aenderung) > 0) progress$set(6)

            # PARAMETER der Tabsets ________________________________________________________________

            parameter_tab <-
              get_query_param("tab") |>
              recode_parameter_int(vec = TRUE, default = 1)

            ##### 2. Schritt: Daten werden ausgelesen auf Basis der URL-Parameter und   ############
            #####             weiterführend die Tabelle und Abbildungen erstellt!       ############

            if (length(aenderung) > 0 & any(c("variable", "variable2", "gruppe", "filter", "zeit") %in% aenderung)){
              if (!(parameter_variable %in% "")){

                variable <-
                  c(parameter_variable, parameter_variable2) |>
                  recode_parameter() |>
                  get_value_by_id("variable")

                group <-
                  parameter_gruppe |>
                  recode_parameter() |>
                  get_value_by_id("reichweite_typ")

                filter <-
                  parameter_filter |>
                  recode_parameter() |>
                  get_value_by_id("reichweite")

                time_period <-
                  parameter_zeit |>
                  recode_parameter()


                #daten <- suppressWarnings(try(get_data(variable, group, filter, (time_period[1]):(time_period[2]))))
                daten <- suppressWarnings(try(get_data(c(parameter_variable, parameter_variable2), parameter_gruppe, parameter_filter, time_period)))

                if (!(class(daten) %in% "try-error") & nrow(daten) > 1){
                  
                  anzahl_abbildung <- count_possible_plots(daten)
                 

                  if (parameter_tab %in% 1 | (parameter_tab - 1) > anzahl_abbildung){
                    #output$ergebnisse <- renderUI(vergleichen_draw_reactable(daten$df))
                    output$ergebnisse <- renderUI(vergleichen_draw_reactable(daten))

                    parameter_tab     <- 1
                    parameter$tab     <- 1
                  } else {
                    
                    if (is.na(anzahl_abbildung)){
                      abbildungen <- list()
                    }else{
                      abbildungen <- suppressWarnings(try(produce_plot(daten, 
                                                                       chart_options_rules_dir = "chart_options_rules",
                                                                       tab = parameter_tab - 1)))
                    }
                   
                    output$ergebnisse <- renderUI(abbildungen)
                  }

                  aenderung <- aenderung[aenderung != "tab"]
                } else {
                  anzahl_abbildung <- 0
                  abbildungen <- list()
                }
          
                for (i in 1:6){
                  if (i > (anzahl_abbildung + 1)){
                    addCssClass(paste0("tab", i), "no_display")
                  } else {
                    removeCssClass(paste0("tab", i), "no_display")
                  }
                }

              }
            }

            if (parameter_tab != parameter$tab | any(c("variable", "zeit", "filter", "gruppe") %in% aenderung) | start){
              parameter$tab <- parameter_tab
              for (i in 1:6){
                if (i %in% parameter_tab){
                  addCssClass(paste0("tab", i), "aktiv")
                } else {
                  removeCssClass(paste0("tab", i), "aktiv")
                }
              }
              aenderung <- c(aenderung, "tab")
            }
            if (length(aenderung) > 0) progress$set(7)

            if (length(aenderung) > 0 & "tab" %in% aenderung & !(parameter_variable %in% "")){

              variable <-
                c(parameter_variable, parameter_variable2) |>
                recode_parameter() |>
                get_value_by_id("variable")

              group <-
                parameter_gruppe |>
                recode_parameter() |>
                get_value_by_id("reichweite_typ")

              filter <-
                parameter_filter |>
                recode_parameter() |>
                get_value_by_id("reichweite")

              time_period <-
                parameter_zeit |>
                recode_parameter()

            #daten <- suppressWarnings(try(get_data(variable, group, filter, (time_period[1]):(time_period[2]))))
              daten <- suppressWarnings(try(get_data(c(parameter_variable, parameter_variable2), parameter_gruppe, parameter_filter, time_period)))
daten_versuch_2 <<- daten
              if (parameter_tab %in% 1){
                output$ergebnisse <- renderUI(vergleichen_draw_reactable(daten))
              } else {
                
               anzahl_abbildung <- count_possible_plots(daten)


                if ((parameter_tab - 1) %in% 1:anzahl_abbildung){
                  abbildungen <- suppressWarnings(try(produce_plot(daten,#daten$df, 
                                                                   chart_options_rules_dir = "chart_options_rules",
                                                                   tab = parameter_tab - 1)))
                   
                  output$ergebnisse <- renderUI(abbildungen) #hier direkt abbildung entsprechend Tab nur laden

                } else {
                  parameter_tab <- 1
                  parameter$tab <- 1
                  output$ergebnisse <- renderUI(vergleichen_draw_reactable(daten))
                }

              }
            }
            if (length(aenderung) > 0) progress$set(9)

            if ("variable" %in% aenderung | start){
              if (parameter_variable %in% ""){
                addCssClass("ergebnis_box", "no_display")
              } else {
                removeCssClass("ergebnis_box", "no_display")
              }
            }

            if (("tab" %in% aenderung | start) & parameter_variable != ""){
              runjs('document.getElementById("indikator-ergebnis_box").scrollIntoView();')
            }

            ##### 3. Schritt: URL auf aktuelle Parameter-Set anpassen                    ###########
            #####             Notwendig, damit Wiederholungen reduziert werden           ###########

            new_url <-
              current_url |>
              param_set(key = "hf",        value = parameter$hf) |>
              param_set(key = "tag",       value = paste(parameter$tag, collapse = ",")) |>
              param_set(key = "variable",  value = parameter$variable) |>
              param_set(key = "variable2", value = paste(parameter$variable2, collapse = ",")) |>
              param_set(key = "zeit",      value = paste(parameter$zeit, collapse = ",")) |>
              param_set(key = "gruppe",    value = paste(parameter$gruppe, collapse = ",")) |>
              param_set(key = "filter",    value = paste(parameter$filter, collapse = ","))

            if (new_url != current_url){
              change_page(new_url)
            }

            if (length(aenderung) > 0) progress$set(10)
            waiter$hide()
            progress$close()
          }
        },
        ignoreNULL = FALSE
      ) #____________________________________________________________ Ende des grossen ObserveEvents


      # ObserveEvents, der den Input liefert, um die URL-Parameter anzupassen:

      observeEvent(
        input$select_tag, {
          if (get_page() %in% URL_PATH){
            current_url <- session$clientData$url_hash

            new_url <-
              param_set(
                urls  = current_url,
                key   = "tag",
                value = process_input_for_parameter(input$select_tag, "tag", FALSE)
              )

            if (new_url != current_url){
              change_page(new_url)
            }
          }
        },
        ignoreNULL = FALSE
      )


      observeEvent(
        input$select_variable, {
          if (get_page() %in% URL_PATH){
            current_url <- session$clientData$url_hash
            new_url <-
              param_set(
                urls = current_url,
                key = "variable",
                value = process_input_for_parameter(input$select_variable, "variable", TRUE)
              )

            if (new_url != current_url){
              change_page(new_url)
            }
          }
        },
        ignoreNULL = FALSE
      )


      observeEvent(
        input$select_variable2, {
          if (get_page() %in% URL_PATH){
            current_url <- session$clientData$url_hash
            new_url <-
              param_set(
                urls = current_url,
                key = "variable2",
                value = process_input_for_parameter(input$select_variable2, "variable", FALSE)
              )

            if (new_url != current_url){
              change_page(new_url)
            }
          }
        },
        ignoreNULL = FALSE
      )

      observeEvent(
        input$zeit, {
          if (get_page() %in% URL_PATH){
            current_url <- session$clientData$url_hash
            new_url <-
              param_set(
                urls  = current_url,
                key   = "zeit",
                value = paste(input$zeit, collapse = ",")
              )

            if (new_url != current_url){
              change_page(new_url)
            }
          }
        },
        ignoreNULL = FALSE
      )

      observeEvent(
        input$eimer_unterscheiden, {
          if (get_page() %in% URL_PATH){
            current_url <- session$clientData$url_hash
            new_url     <-
              param_set(
                urls  = current_url,
                key   = "gruppe",
                value = process_input_for_parameter(input$eimer_unterscheiden, "reichweite_typ", FALSE)
              )
            if (new_url != current_url){
              change_page(new_url)
            }
          }
        },
        ignoreNULL = FALSE
      )

      observeEvent(
        input$select_tab, {
          if (get_page() %in% URL_PATH){
            current_url <- session$clientData$url_hash
            input_tab   <- input$select_tab
            if (is.null(input_tab)) input_tab <- 1
            new_url     <- param_set(urls = current_url, key   = "tab", value = input_tab)
            if (new_url != current_url){
              change_page(new_url)
            }
          }
        },
        ignoreNULL = FALSE
      )

      observeEvent(
        input$button_variable2_plus, {
          if (get_page() %in% URL_PATH){
            removeCssClass("variable2_select_div", "no_display")
            removeCssClass("button_variable2_minus", "no_display")
            addCssClass("button_variable2_plus", "no_display")
          }

        }
      )

      observeEvent(
        input$button_variable2_minus, {
          if (get_page() %in% URL_PATH){
            addCssClass("variable2_select_div", "no_display")
            addCssClass("button_variable2_minus", "no_display")
            removeCssClass("button_variable2_plus", "no_display")
            session$clientData$url_hash |>
              param_set(key = "variable2", value = "na") |>
              change_page()
          }
        }
      )

      observeEvent(
        input$reset, {
          if (get_page() %in% URL_PATH){
            session$clientData$url_hash |>
              param_set(key = "hf",        value = "") |>
              param_set(key = "tag",       value = "") |>
              param_set(key = "variable",  value = "") |>
              param_set(key = "variable2", value = "") |>
              param_set(key = "zeit",      value = "") |>
              param_set(key = "gruppe",    value = "") |>
              param_set(key = "filter",    value = "") |>
              change_page()
          }
        }
      )

      for (i in write_explorer_filter_observer(unlist(get_query("SELECT id FROM reichweite_typ")[,1]))){
        eval(parse(text = i))
      }

      observeEvent(
        filter_aenderung(), {

          if (get_page() %in% URL_PATH){
            current_url <- session$clientData$url_hash

            filter_inputs <-
              input |>
              reactiveValuesToList() |>
              names() |>
              grep(pattern = "^filter", value = TRUE) |>
              {\(.) .[!grepl("_open", .)]}()

            if (length(filter_inputs) > 0){
              filter_inputs <-
                filter_inputs |>
                lapply(\(.) input[[.]]) |>
                unlist()
            } else {
              filter <- NA
            }

            new_url     <-
              param_set(
                urls  = current_url,
                key   = "filter",
                value = process_input_for_parameter(filter_inputs, "reichweite", FALSE)
              )

            if (new_url != current_url){
              change_page(new_url)
            }
          }

        }
      )

    }
  )
} # Ende der Server-Funktion____________________________________________________________________________________________

#' Hilfsfunktion Update Link #TODO in UI schieben
recode_parameter_int <- function(x, vec = FALSE, default = ""){
  if (is.null(x)) return(default)
  if (vec) x <- strsplit(x, ",")[[1]]
  x <- suppressWarnings(as.integer(x))
  if (any(is.na(x))) return(default)
  return(sort(x))
}

#' Hilfsfunktion zieht mögliche Variablen
get_variables <- function(parameter_tag = c()){

  query <-
    "SELECT DISTINCT tl.reihe_id as id, variable.beschr
     FROM tag_link tl
     LEFT JOIN variable ON tl.reihe_id = variable.id
     WHERE tl.tabelle_id = (SELECT id FROM tabelle WHERE bez = 'variable')"
  if (length(parameter_tag) > 0){
    query <-
      paste(
        query,
        sprintf(
          "AND tl.tag_id IN (%s)",
          paste(
            parameter_tag,
            collapse = ","
          )
        )
      )
  }

  output <-
    query |>
    get_query()

  return(output)
}

get_tags_by_hfs <- function(hf){

  hf_zuordnung <- read_yaml("yml/explorer/handlungsfeld_zuordnung.yml")
  tags <- get_sql("indikator_get_tag_by_variable", TRUE)

  if(hf == 1){
    tags <- tags[tags$beschr %in% hf_zuordnung$handlungsfeld1, ]
  } else if(hf == 2){
    tags <- tags[tags$beschr %in% hf_zuordnung$handlungsfeld2, ]
  }
  return(tags)
}

render_eimer <- function(reichweiten = list(), selected = NULL, ns){

  if (is.null(reichweiten))    return(HTML(""))
  if (length(reichweiten) < 1) return(HTML(""))

  if (!is.null(selected)){
    reichweiten <- reichweiten[!(reichweiten %in% selected)]
  }

  bucket_list(
    header      = NULL,
    group_name  = "eimer_reichweiten",
    orientation = "horizontal",
    add_rank_list(
      text      = "Auswahl",
      labels    = reichweiten,
      input_id  = ns("eimer_ignorieren")
    ),
    add_rank_list(
      text      = "Gewählt",
      labels    = selected,
      input_id  = ns("eimer_unterscheiden")
    )
  ) |>
    renderUI()
}

#' @noRd
vergleichen_draw_reactable <- function(daten){

  if (is.null(daten))  return(reactable(data.frame(keine = "daten")))
  if (nrow(daten) < 1) return(reactable(data.frame(keine = "daten")))

  div(
    style = "margin-top: 20px",
    reactable(
      daten[, !names(daten) %in% c("meta_info_einheiten", "meta_info_zeit", "meta_info_quellen")],
      highlight       = TRUE,
      filterable      = FALSE,
      borderless      = TRUE,
      fullWidth       = TRUE,
      defaultPageSize = 25,
      theme           = get_reactable_theme(),
      language        = get_reactable_lang(),
      rowClass        = "small-font",
      details = function(index){
        datum <- daten[index,]
        div(
          class = "reactable-details",
          h3(paste0("Details zu der Variable '", datum$`Variable/n`, "'")),
          div(class = "reactable-details-label", "Beschreibung"),
          div(class = "reactable-details", p("kommt noch...")),
     #     div(class = "reactable-details-label", "Verfügbare Einheiten"),
      #    div(class = "reactable-details", p(datum$meta_info_einheiten)),
      #    div(class = "reactable-details-label", "Verfügbare Jahre"), # TODO: dynamisch für versch. Zeiteinheiten!
      #    div(class = "reactable-details", p(datum$meta_info_zeit)),
     #     div(class = "reactable-details-label", "Quellenangabe"),
    #      div(class = "reactable-details", p(datum$meta_info_quellen)) # wieder aufnehmen, wenn get_data umstrukturiert ist

        )
      }

    ),
    div("Quelle: Under Construction")
  )

}

#' @noRd
write_explorer_filter_observer <- function(filter_ids){
  write_code <- function(.){
    "observeEvent(input$filterERSETZEN, {filter_aenderung(filter_aenderung() * -1)}, ignoreNULL = FALSE)" |>
      gsub(pattern = "ERSETZEN", replacement = .)
  }
  return(sapply(filter_ids, write_code))
}

#' @noRd
render_filter <- function(variable_id, parameter_gruppe = NULL, parameter_filter = NULL, ns){

  gruppen <-
    "SELECT DISTINCT v.reihe_id as id, r.beschr as beschr, r.reichweite_typ_id, rt.beschr as reichweite_typ
   FROM view_daten_link v
   LEFT JOIN reichweite r ON v.reihe_id = r.id
   LEFT JOIN reichweite_typ rt ON r.reichweite_typ_id = rt.id
   WHERE v.daten_id IN (SELECT id FROM daten WHERE variable_id = %s) AND
         v.tabelle_id = (SELECT id FROM tabelle WHERE bez = 'reichweite') AND
         v.reihe_id IN (SELECT id FROM reichweite WHERE reichweite_typ_id IN (%s))" |>
    sprintf(variable_id, paste(parameter_gruppe, collapse = ",")) |>
    get_query()

  if (!is.null(parameter_filter)){
    filter <-
      "SELECT beschr FROM reichweite WHERE id IN (%s)" |>
      sprintf(paste(parameter_filter, collapse = ",")) |>
      get_query() |>
      unlist() |>
      unname()
  } else {
    filter <- c()
  }

  tagList(
    lapply(
      parameter_gruppe,
      \(.){
        choices <- sort(gruppen$beschr[gruppen$reichweite_typ_id %in% .])
        if (any(choices %in% filter)){
          selected <- filter[filter %in% choices]
        } else {
          selected <- NULL
        }
        pickerInput(
          inputId  = ns(paste0("filter", .)),
          label    = gruppen$reichweite_typ[match(., gruppen$reichweite_typ_id)],
          choices  = choices,
          selected = selected,
          options  = get_picker_options(`actions-box` = TRUE),
          multiple = TRUE
        )
      }
    )
  ) |>
    renderUI()
}

process_input_for_parameter <- function(input_select, table, single_output = TRUE, na_value = NULL){

  new_value <- ""
  if (!is.null(input_select)){

    id <-
      "SELECT id FROM %s WHERE beschr IN (%s)" |>
      sprintf(
        table,
        paste(
          paste0("'", input_select, "'"),
          collapse = ", "
        )
      ) |>
      get_query() |>
      unlist()

    if (length(id) < length(input_select) & !is.null(na_value)){
      id <- c(id, na_value)
    }

    if (!is.null(id)){
      if (is.numeric(id)){

        if (single_output){
          if (length(id) %in% 1){
            new_value <- id
          }
        } else {
          new_value <- paste(sort(id), collapse = ",")
        }

      }
    }
  }

  return(URLencode(as.character(new_value)))
}

render_h <- function(text, color){
  h2(text, style = sprintf("color: %s;", color)) |>
    renderUI()
}

add_reactable_footer <- function(results){
  if(!is.null(nrow(results))){
    if(nrow(results) > 1){

      return(
        div(
          p(),
          p(style = "text-align: center; color: var(--grey);", paste0("Quelle(n):", results))
        )
      )

    }
  }
  return(HTML(""))
}

check_parameter_hf <- function(parameter){
  if (is.null(parameter)) return(0)
  if (any(is.na(parameter))) return(0)
  if (!(length(parameter) %in% 1)) return(0)
  if (!(parameter %in% 0:2)) return(0)
  return(parameter)
}

check_parameter_variable <- function(parameter){
  if (is.null(parameter)) return("")
  if (any(is.na(parameter))) return("")
  if (!(length(parameter) %in% 1)) return("")
  if (is.na(suppressWarnings(as.integer(parameter)))) return("")
  if (!unlist(get_query(paste0("SELECT COUNT(id) = 1 FROM variable WHERE id = ", parameter)))) return("")
  return(parameter)
}

get_value_by_id <- function(id, tabelle, label = "beschr"){
  if (is.null(id)) return(NULL)
  "SELECT %s FROM %s WHERE id IN (%s)" |>
    sprintf(label, tabelle, paste(id, collapse = ",")) |>
    get_query() |>
    unlist() |>
    unname()
}

get_gruppe_by_variable_id <- function(variable_id){
  "SELECT DISTINCT r.id, r.beschr
     FROM view_daten_link v
     LEFT JOIN reichweite_typ r ON v.reihe_id = r.id
     WHERE
       v.daten_id IN (SELECT id FROM daten WHERE variable_id = %s) AND
       v.tabelle_id = (SELECT id FROM tabelle WHERE bez = 'reichweite_typ')" |>
    sprintf(variable_id) |>
    get_query()
}

get_zeit_by_variable_id <- function(variable_id){
  "SELECT
     MIN(date_part('year', zeit_start)) as min,
     MAX(date_part('year', zeit_ende)) as max
   FROM daten
   WHERE variable_id = %s" |>
    sprintf(variable_id) |>
    get_query() |>
    as.vector()
}

render_zeit_slider <- function(zeit, zeit_select, ns){
  sliderTextInput(
    inputId  = ns("zeit"),
    label    = "Zeitspanne:",
    choices  = (zeit$min):(zeit$max),
    selected = zeit_select
  ) |>
    renderUI()
}

render_head <- function(parameter_hf){
  titel   <- paste("Tabellen und Abbildungen zu", c("allen Themen", "Bildung & Kompetenzen", "Forschung & Innovation"))
  farben  <- c("var(--blue)", "var(--color-bildung)", "var(--color-forschung)")
  klassen <- c("keine_handlung_triangle", "handlung1_triangle", "handlung2_triangle")
  for (klasse in klassen){
    if (klasse %in% klassen[parameter_hf + 1]){
      addCssClass("triangle", klasse)
    } else {
      removeCssClass("triangle", klasse)
    }
  }
  return(render_h(titel[parameter_hf + 1], farben[parameter_hf + 1]))
}

diverge_two_vectors <- function(vector_1, vector_2){
  vector_1 <- vector_1[!is.na(vector_1)]
  vector_2 <- vector_2[!is.na(vector_2)]

  no_difference <- all(vector_1 %in% vector_2) & all(vector_2 %in% vector_1)
  return(!no_difference)
}

get_comparison_variables <- function(start_variable_id){
 
   # timestamp <- Sys.time()
    
    vergleichs_variablen <-
      get_query(
        paste0(
          "WITH 
          VAR_START AS (
          SELECT DISTINCT d.wert_einheit_id, d.zeit_einheit_id, d.zeit_start, d.zeit_ende, r.reichweite_typ_id
            FROM daten d
            LEFT JOIN daten_reichweite dr ON d.id = dr.daten_id
            LEFT JOIN reichweite r ON dr.reichweite_id = r.id
            WHERE d.variable_id = ", start_variable_id, "),
          
          VERGLEICH AS (
          SELECT d.variable_id, d.wert_einheit_id, d.zeit_einheit_id, d.zeit_start, d.zeit_ende, r.reichweite_typ_id
          FROM daten d
          LEFT JOIN daten_reichweite dr ON d.id = dr.daten_id
          LEFT JOIN reichweite r ON dr.reichweite_id = r.id
          WHERE variable_id != ", start_variable_id, ")
          
          SELECT DISTINCT VERGLEICH.variable_id
          FROM VERGLEICH
          INNER JOIN VAR_START
          ON VAR_START.wert_einheit_id = VERGLEICH.wert_einheit_id 
          AND VAR_START.zeit_einheit_id = VERGLEICH.zeit_einheit_id 
          AND VAR_START.zeit_start = VERGLEICH.zeit_start 
          AND VAR_START.zeit_ende = VERGLEICH.zeit_ende
          AND VAR_START.reichweite_typ_id = VERGLEICH.reichweite_typ_id"
       )
      )
    
   # message(paste("Timestamp_Vergleich:", round(difftime(Sys.time(), timestamp, units = "secs"), 2), "Sekunden"))
    
    return(vergleichs_variablen)
    
  }


get_possible_variable2 <- function(variable_id, parameter_hf, parameter_tag){

  vergleichsvariable <- get_comparison_variables(variable_id)
  if (length(parameter_tag) < 1){
    variables <- get_variables(get_tags_by_hfs(hf = parameter_hf)$id)
  } else {
    variables <- get_variables(parameter_tag)
  }
  vergleichsvariable <- vergleichsvariable$variable_id[vergleichsvariable$variable_id %in% variables$id]
    
  if (length(vergleichsvariable) < 1){
    return(vergleichsvariable)
  }

  return(
    "SELECT id, beschr FROM variable WHERE id IN (%s)" |>
      sprintf(
        paste(
          paste0("'", vergleichsvariable, "'"),
          collapse = ","
        )
      ) |>
      get_query()
  )
}

check_filter <- function(filter, gruppe){
  if (is.null(gruppe) | all(nchar(filter) < 1)) return(filter)
  reichweiten <-
    "SELECT id, reichweite_typ_id FROM reichweite WHERE id IN (%s)" |>
    sprintf(paste(filter, collapse = ",")) |>
    get_query()
  if (!any(reichweiten$reichweite_typ_id %in% gruppe)) return("")
  return(reichweiten$id[reichweiten$reichweite_typ_id %in% gruppe])
}

recode_parameter <- function(parameter){
  if (any(parameter %in% "") | is.null(parameter) | length(parameter) < 1){
    return(NULL)
  } else {
    return(parameter)
  }
}

render_buttons <- function(abbildungen = 5, ns){
  buttons <-
    data.frame(
      id         = 1:(abbildungen + 1),
      id_and_typ = c("tab1", paste0("abb", 1:abbildungen)),
      label      = c("Tabelle", paste("Abbildung", 1:abbildungen))
    )
  tagList(
    apply(
      buttons, 1,
      \(.) {
        actionButton(
          ns(paste0("tab", .["id"])),
          .["label"],
          class = "tab_button",
          onclick = sprintf("selectTabs(%s)", .["id"])
        )
      }
    )
  )
}

write_hilfe <- function(){
  "<p>Auf dieser Seite sind alle verfübaren Variablen und Indikatoren zu finden. Diese können über das Feld <b>Variablen</b> aufgerufen und ausgewählt werden. Wenn eine Variable ausgewählt wurde, erscheinen weitere Optionen.</p>"
}

draw_hilfe <- function(ns){
  div(
    id = ns("hilfe"),
    style = "transition: all 1.5s ease; opacity: 0; position: absolute; top: 410px; right: 672px; pointer-events: none;",
    readLines("www/img/variablen_auswahl.svg") |>
      paste(collapse = " ") |>
      HTML()
  )
}

draw_balken <- function(){
  readLines("www/img/balken.svg") |>
    paste(collapse = " ") |>
    HTML()
}
