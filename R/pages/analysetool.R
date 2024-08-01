#' Necessary Packages/Functions

box::use(
  ../../R/utils/database[get_query, get_sql],
  ../../R/utils/ui[draw_under_construction, draw_zurueck_button],
  ../../R/utils/charts[produce_plot],
  ../../R/pkgs/wrangling/get_data[get_data],
  ../../R/pkgs/wrangling/get_comparison_variables[get_comparison_variables],
  
  shiny[
    NS, moduleServer, observeEvent,
    uiOutput, renderUI,
    fluidPage, fluidRow, column,
    HTML, tagList, div, h2, h3, p,
    reactiveVal, reactiveValues,
    actionButton, reactive,
    eventReactive,
    htmlOutput, plotOutput, renderPlot,
    renderText, textOutput,
    tabPanel, tabsetPanel,
    renderCachedPlot, mainPanel,
    updateActionButton,
    updateTabsetPanel, icon, isolate
  ],
  shinyWidgets[pickerInput, updatePickerInput,
               sliderTextInput],
  shiny.router[get_query_param, change_page, get_page],
  shinyBS[bsTooltip],
  shinycssloaders[withSpinner],
  reactable[reactable, reactableOutput, renderReactable, colDef, reactableTheme, reactableLang],
  sortable[bucket_list, add_rank_list],
  plotly[plotlyOutput, renderPlotly],
  utils[URLencode, URLdecode],
  urltools[param_get, param_set, path],
  htmltools[tagQuery, tags],
  rlist[list.append],
  stats[median],
  yaml[read_yaml],
  shinyjs[
    removeCssClass,
    addCssClass
  ]
)

URL_PATH             <- "analysetool"
LABEL_TABSET_TABELLE <- "Tabelle"
LABEL_TABSET_GRAFIK  <- "Abbildung"

#' UI Funktion Analysetool
#' @export

module_analysetool_ui <- function(id = URL_PATH, label = paste0(URL_PATH, "_m"), type = "all") {
  ns <- NS(id)
  fluidPage(
    #TODO läuft noch nicht
    # tags$head(
    #   tags$script(HTML('
    #   $(document).ready(function(){
    #     $(\'[data-toggle="tooltip"]\').tooltip();
    #   });
    # '))
    # ),
    div(
      class = "panel-content",
      div(
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
      # div(
      #   style = "display: flex; justify-content: space-between;",
      #   # h2("Explorer - Analysieren von Variablen"),
      #   div(draw_zurueck_button())
      # ),
      # uiOutput(ns("intro")),
      fluidRow(
        style = "padding: 10px; display: flex; margin: 0;",
        div(
          class = "content-box",
          style = "width: 100%;",
          div(
            style = "padding: 10px; display: flex; flex-direction: row; flex-wrap: wrap;
                padding: 16px 20px 1px 20px; align-items: center;",

            # Themenbereich/Tags
            pickerInput(
              inputId = ns("select_tag"),
              label = p(style = "color: var(--light-blue);
                        margin-bottom: 0px;",
                        "Themenbereiche:"),
              choices = c(""),
              options  = list(
                `actions-box`        = TRUE,
                `none-selected-text` = "nichts ausgewählt",
                `select-all-text`    = "alle auswählen",
                `deselect-all-text`  = "nichts auswählen",
                `live-search`        = TRUE
              ),
              multiple = TRUE
            ),

            div(
              class = "menu-box", style = "margin-left: auto; display: flex;
              flex-direction: column; align-items: flex-end;
              padding: 10px 20px 20px;",
              p("Speichern & Teilen:", style = "margin-bottom: 4px;"),
              div(
                style = "display: flex; align-self: center;",
                actionButton(
                  ns("speichern"),
                  icon  = icon("floppy-disk", class = "fa-solid"),
                  label = "",
                  class = "button_icon"
                ),
                actionButton(
                  ns("teilen"),
                  icon  = icon("share-nodes"),
                  label = "",
                  class = "button_icon"
                )
                # ,
                # actionButton(
                #   ns("bookmark"),
                #   icon  = icon("bookmark", class = "fa-solid"),
                #   label = "",
                #   class = "button_icon"
                # )
              )
            )
          ),

          div(
            style = "padding: 1px 20px 1px 20px; display: flex; flex-direction: row; flex-wrap: nowrap; align-items: center;",

            # Variable wählen
            div(
              style = "display: flex; flex-direction: column;",
              pickerInput(
                inputId = ns("select_vars"),
                label = "Variablenauswahl:",
                choices = c(""),
                options  = list(
                  `actions-box`        = FALSE,
                  `none-selected-text` = "nichts ausgewählt",
                  `select-all-text`    = "alle auswählen",
                  `deselect-all-text`  = "nichts auswählen",
                  `live-search`        = TRUE,
                  `max-options`        = 1
                ),
                multiple = TRUE
              )
            ),

            div(
              style = "margin-left: 20px; display: flex; flex-direction: column; align-items: flex-start;",
              div(
                style = "display: flex; align-items: center; margin-top: -6px;", # Adjusted the margin-top to align better
                p("Vergleichsvariable hinzufügen:", style = "margin: 0; font-weight: 700; margin-right: 10px;"),
                actionButton(
                  ns("new_var_btn"),
                  icon  = icon("square-plus", class = "fa-solid"),
                  label = "",
                  class = "button_icon"
                )
              ),
              uiOutput(ns("variable_vergleichen"), style = "margin-top: 2px; width: 100%;")
            )
          ),
          
          # 3. Schritt Wahl der Reichweiten/Filter
          uiOutput(ns("variable")),
          div(
            style = "display: flex; flex-wrap: wrap; margin-left: 30px;",
            div(uiOutput(ns("select_reichweite")), style = "max-width: 650px; width: 80%;"),
            div(uiOutput(ns("filter_reichweite"),  style = "max-width: 250px; width: 100%;
                         margin-left: 25px; margin-top: 65px;"))
          ),
          
          # #4. Reset-Button
          div(class = "menu-box", stlye = "margin: 20px",
              actionButton(
                ns("reset"),
                icon  = icon("repeat", class = "fa-solid"),
                label = "",
                class = "button_icon"
                #TODO vgl. oben, läuft noch nicht
                # ,
                # `data-toggl` = "tooltip",
                # text = "Hier setzen Sie alle Ihre Einstellungen zurück"
              )
          )
        )
      ),
      
      fluidRow(
        
        # 5. Ergebnisse in Tabs
        div(
          style = "margin-top: 24px",
          class = "tabset-menu",
          withSpinner(
            uiOutput(ns("result_tabs"))
          )
        )
        
      )
      
    )
  )
  
}

#' Server Funktion analysetool
#' @export

module_analysetool_server <- function(id = URL_PATH, type = "all"){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      
      # gesperrt wird verwendet, um die Aktualisierung im Rahmen von observe/observeEvents temporär zu verhindern
      gesperrt  <- reactiveVal(FALSE)
      
      daten <-
        reactiveValues(
          werte     = data.frame(),
          gruppen   = c(),
          auswahl   = c(),
          filter    = data.frame(type = c(), werte = c())[0,],
          variable  = c(),
          variable2 = c(),
          aenderung = c(),
          show_additional_var = FALSE
        )
      
      input_var  <- reactiveVal(data.frame())
      input_var2 <- reactiveVal(data.frame())
      input_tag  <- reactiveVal(data.frame())
      plot_list  <- reactiveVal(list())
      tab_liste  <- reactiveValues(tabs = c())
      
      filter_param <- vergleichen_translate_filter_param()
      
      # URL-Parameter werden zwischengespeichert, um den Verlauf/eine Aktualisierung besser nachzuvollziehen
      # Jeder mögliche URL-Parameter hat einen Eintrag in der reaktiven Liste und "" steht für NULL/Kein Eintrag
      eval(
        parse(
          text =
            paste0(
              "parameter <- reactiveValues(",
              paste(
                paste(c("at_gp", "at_tg", "hf", "at_vr", "at_vr2", "at_tab", filter_param$param), "= ''"),
                collapse = ","
              ),
              ")"
            )
        )
      )
      
      
      # Vergleichsvariable als Option hinzufügbar
      observeEvent(input$new_var_btn, {
        daten$show_additional_var <- TRUE
        output$variable_vergleichen <- renderUI({
          
          if(isTRUE(daten$show_additional_var) && nrow(input_var2()) > 0){
            pickerInput(
              inputId = ns("variable_vergleichen"),
              label = NULL,
              choices = sort(input_var2()$beschr[input_var2()$relevant]),
              selected = daten$variable2,
              options  = list(
                `actions-box`        = FALSE,
                `none-selected-text` = "nichts ausgewählt",
                `select-all-text`    = "alle auswählen",
                `deselect-all-text`  = "nichts auswählen",
                `live-search`        = TRUE #,
                #  `max-options`        = 1
              ),
              multiple = TRUE
            )
          } else {
            NULL
          }
          
        })
      })
      
      #Farbgestaltung nach HF
      observeEvent(
        get_query_param(), {
          if (get_page() == URL_PATH){
            
            param_hf <- get_query_param("hf")
            if (is.null(param_hf)) param_hf <- 0
            
            if (param_hf == 1){
              
              output$ueberschrift <- render_h("Analysen und Grafiken zu Bildung & Kompetenzen", "var(--color-bildung)")
              addCssClass("triangle",    "handlung1_triangle")
              removeCssClass("triangle", "handlung2_triangle")
              removeCssClass("triangle", "keine_handlung_triangle")
              
            } else if (param_hf == 2){
              
              output$ueberschrift <- render_h("Analysen und Grafiken zu Forschung & Innovation", "var(--color-forschung)")
              addCssClass("triangle",    "handlung2_triangle")
              removeCssClass("triangle", "handlung1_triangle")
              removeCssClass("triangle", "keine_handlung_triangle")
              
            } else {
              
              output$ueberschrift <- render_h("Analysen und Grafiken zu allen Themen", "var(--blue)")
              addCssClass("triangle",    "keine_handlung_triangle")
              removeCssClass("triangle", "handlung1_triangle")
              removeCssClass("triangle", "handlung2_triangle")
              
            }
          }
        },
        ignoreNULL = FALSE
      )
      
      
      # Ergebnisaufruf über Button
      results <- reactive({
        
        # output Tabelle mit Aufruf von get_data() der wrangling Fkt
        
        if(!is.null(daten$variable2)){
          vars <- c(daten$variable, daten$variable2)
        }else{
          vars <- daten$variable
        }
        
        auswahl <- daten$auswahl
        
        if("Zeit" %in% auswahl){
          if(all(grepl("Zeit", auswahl))) {
            auswahl <- NULL
          } else {
            auswahl <- auswahl[!grepl("Zeit", auswahl)]
          }
          
        }
        
        zeit <- NULL
        filter <- daten$filter$werte
        if(length(filter)==0) filter <- NULL
        filter_typ <- daten$filter$type
        
        if("Zeit" %in% filter_typ){
          
          if(all(grepl("Zeit", filter_typ))) {
            zeit <- c(filter[1], filter[length(filter)])
            filter <- NULL
          } else {
            zeit <- as.numeric(filter[grepl("^\\d+$", filter)])
            filter <- filter[!grepl("^\\d+$", filter)]
          }
        }


        data_wrangling_output <-
          get_data(
          variable = vars,
          group = unique(auswahl),#unique(daten$auswahl),
          filter = unique(filter),
          time_period = zeit
        )

        selected_data         <- data_wrangling_output$df
        selected_data_quellen <- data_wrangling_output$df_quellen

        list(
          selected_data = selected_data,
          selected_data_quellen = selected_data_quellen
        )

      })


      # Ergebnis-Output-Serverfunktionen
      observeEvent(
        results()$selected_data, {

          results_data    <- results()$selected_data
          results_quellen <- results()$selected_data_quellen

          daten_vorhanden <- FALSE
          if (get_page() %in% URL_PATH){
            if (!is.null(results_data)){
              if (is.data.frame(results_data)){
                daten_vorhanden <- TRUE
              }
            }
          }
          
          if (daten_vorhanden){
            
            output$result_tabs <-
              renderUI({
                
                result_tabs <-
                  list(
                    tabPanel(
                      LABEL_TABSET_TABELLE,
                      reactableOutput(ns("table")),
                      add_reactable_footer(results_quellen)
                    )
                  )
                
                
                if (!is.null(results_data)){
                  if (is.data.frame(results_data)){
                    if (nrow(results_data) > 1){ #produce_plot erwartet min. 2 Zeilen, bei einer Zeile grad nur Tabelle
                      
                      plot_list <-
                        produce_plot(
                          results()$selected_data,
                          chart_options_rules_dir = "chart_options_rules"
                        )
                      
                      grafiken <- c()
                      
                      for(i in seq_along(plot_list)) {
                        grafiken    <- c(grafiken, paste(LABEL_TABSET_GRAFIK, i))
                        result_tabs <-
                          append(
                            result_tabs,
                            list(
                              tabPanel(
                                grafiken[length(grafiken)],
                                div(
                                  class = "content-box",
                                  style = "width: 100%; height: 600px;",
                                  plotlyOutput(ns(paste("plot", i, sep = "")), height = "100%")
                                )
                              )
                            )
                          )
                      }
                      
                      tab_liste$tabs <- c(LABEL_TABSET_TABELLE, grafiken)
                      
                      for(i in seq_along(plot_list)) {
                        local({
                          my_i <- i
                          output[[paste("plot", my_i, sep = "")]] <- renderPlotly({
                            plot_list[[my_i]]
                          })
                        })
                      }
                      
                    }
                  }
                }
                
                result_tabs   <- list.append(result_tabs, id = ns("tab_menu"))
                aktueller_tab <- isolate(get_query_param("at_tab"))
                if (!is.null(aktueller_tab)){
                  aktueller_tab <- process_parameter_input_to_tab(aktueller_tab)
                  result_tabs   <- list.append(result_tabs, selected = aktueller_tab)
                }
                do.call(tabsetPanel, args = result_tabs)
              })
            
            # rendering tabelle, wenn Knopf gedrückt wird
            output$table <- renderReactable({
              vergleichen_draw_reactable(results()$selected_data)
            })
            
          } else {
            
            output$result_tabs <-
              renderUI({
                p("Daten auswählen") #TODO
              })
            
          }
        }, ignoreNULL = FALSE)
      
      observeEvent(input$reset, {

        daten$filter  <- data.frame()
        daten$gruppen <- c()
        daten$werte   <- data.frame()
        daten$auswahl <- c()
        daten$variable <- c()
        daten$variable2 <- c()
        show_additional_var <- FALSE

        updatePickerInput(session, "select_tag", selected = NULL)
        updatePickerInput(session, "select_vars", selected = NULL)
        output$variable_vergleichen <- renderUI({HTML("")})
        output$filter_reichweite <- renderUI({HTML("")})
        output$select_reichweite <- renderUI({HTML("")})
      
        # URL zurücksetzen
        current_url <- session$clientData$url_hash
        hf <- get_query_param()$hf
        new_url <- sprintf("#!/analysetool?hf=%s", hf)
        test <- get_query_param()
        change_page(new_url)
      
      })
      
      # Das zentrale Event bezieht sich auf die URL-Parameter; auf dieser Basis werden alle Anpassungen vorgenommen
      observeEvent(
        get_query_param(), {

          current_url <- session$clientData$url_hash
          if (!gesperrt()){
            if (get_page() %in% URL_PATH){
              if (TRUE){#!is.null(get_query_param())){
                
                gesperrt(TRUE)
                daten$aenderung <- FALSE
                
                # PARAMETER HF - Update des Handlungsfeldes / der Themen Auswahl __________________
                
                param_hf <-
                  get_query_param("hf") |>
                  recode_param_int(default = 0)
                
                if (param_hf != parameter$hf){
                  parameter$hf <- param_hf
                  
                  if (param_hf == 0){
                    
                    input_var(indikator_get_variables()) #TODO
                    input_var2(indikator_get_variables())
                    input_tag(get_sql("indikator_get_tag_by_variable", TRUE))
                    updatePickerInput(session, "select_tag",      choices = sort(input_tag()$beschr))
                    updatePickerInput(session, "select_vars", choices = sort(input_var()$beschr[input_var()$relevant]))
                    updatePickerInput(session, "variable_vergleichen", choices = sort(input_var2()$beschr[input_var2()$relevant]))
                    daten$aenderung <- TRUE
                    
                  } else if(param_hf == 1){
                    
                    input_var(indikator_get_variables())
                    input_var2(indikator_get_variables())
                    input_tag(indikator_get_tags_by_hfs(hf=1))
                    updatePickerInput(session, "select_tag",      choices = sort(input_tag()$beschr))
                    updatePickerInput(session, "select_vars", choices = sort(input_var()$beschr[input_var()$relevant]))
                    updatePickerInput(session, "variable_vergleichen", choices = sort(input_var2()$beschr[input_var2()$relevant]))
                    daten$aenderung <- TRUE
                    
                  } else if(param_hf == 2){
                    
                    input_var(indikator_get_variables())
                    input_var2(indikator_get_variables())
                    input_tag(indikator_get_tags_by_hfs(hf=2))
                    updatePickerInput(session, "select_tag",      choices = sort(input_tag()$beschr))
                    updatePickerInput(session, "select_vars", choices = sort(input_var()$beschr[input_var()$relevant]))
                    updatePickerInput(session, "variable_vergleichen", choices = sort(input_var2()$beschr[input_var2()$relevant]))
                    daten$aenderung <- TRUE
                  }
                  
                }
                
                # PARAMETER AT_TG - Update der Tags __________________________________________________
                
                param_at_tg <-
                  get_query_param("at_tg") |>
                  recode_param_int(vec = TRUE)
                
                # Nur Änderungen vornehmen, wenn sich etwas geändert hat
                if (!(all(param_at_tg %in% parameter$at_tg) &
                      all(parameter$at_tg %in% param_at_tg))){
                  
                  parameter$at_tg <- param_at_tg
                  selected_tags   <- input_tag()$beschr[input_tag()$id %in% param_at_tg]
                  
                  # Picker updaten, wenn ich die ausgewählten Items unterscheiden:
                  # Das ist der Fall, wenn die Paramter-Eingabe über die URL und nicht den Picker erfolgt
                  if (!(all(selected_tags %in% input$select_tag) &
                        all(input$select_tag %in% selected_tags))){
                    updatePickerInput(session, "select_tag", selected = input_tag()$beschr[input_tag()$id %in% param_at_tg])
                  }
                  
                  # Aktualisierung der Variablen aus Basis der Tags
                  
                  input_var(indikator_get_variables_by_tags(input_var(), selected_tags))
                  updatePickerInput(
                    session = session,
                    inputId = "select_vars",
                    selected = input$select_vars,
                    choices = sort(input_var()$beschr[input_var()$relevant])
                  )
                  input_var2(indikator_get_variables_by_tags(input_var2(), selected_tags))
                  updatePickerInput(
                    session = session,
                    inputId = "variable_vergleichen",
                    selected = input$variable_vergleichen,
                    choices = sort(input_var2()$beschr[input_var2()$relevant])
                  )
                  
                  
                  daten$aenderung <- TRUE
                }
                
                # PARAMETER AT_VR - Update der Variablen _____________________________________________
                
                param_at_vr <-
                  get_query_param("at_vr") |>
                  recode_param_int()
                
                if (param_at_vr != parameter$at_vr){
                  parameter$at_vr <- param_at_vr
                  
                  if (param_at_vr == ""){
                    
                    daten$filter  <- data.frame()
                    daten$gruppen <- c()
                    daten$werte   <- data.frame()
                    daten$auswahl <- c()
                    daten$variable <- c()
                    daten$variable2 <- c()
                    show_additional_var <- FALSE
                    
                    updatePickerInput(session, "select_vars", selected = NULL)
                    #updatePickerInput(session, "variable_vergleichen", selected = NULL)
                    output$variable_vergleichen <- renderUI({HTML("")})
                    output$filter_reichweite <- renderUI({HTML("")})
                    output$select_reichweite <- renderUI({HTML("")})
                    
                    
                  } else {
                    
                    variable <- get_query(paste0("SELECT id, beschr FROM variable WHERE id =", param_at_vr))
                    
                    if (nrow(variable) == 1){
                      
                      tabelle <- vergleiche_load_reichweiten_by_multiple_variables(variable$id)
                      
                      daten$filter  <- data.frame()
                      daten$gruppen <- sort(tabelle$gruppe)
                      daten$werte   <- tabelle$daten
                      daten$auswahl <- c()
                      daten$variable <- variable$beschr
                      daten$variable2 <- c()
                      
                      
                      output$filter_reichweite <- renderUI({HTML("")})
                      output$select_reichweite <- renderUI({
                        fluidRow(
                          p(style= "font-weight: 700; margin-left: 10px;
                            margin-top: 20px;",
                            "Variablenausprägungen:"),
                          indikator_draw_eimer(daten$gruppen, ns = ns)
                        )})
                      updatePickerInput(session, "select_vars", selected = variable$beschr)
                      updatePickerInput(session, "variable_vergleichen", selected = NULL)
                    }
                    
                  }
                  
                  daten$aenderung <- TRUE
                }
                
                # WEITERE VARIABLEN - Update weitere Variablen _______________________________________
                
                # Anpassen von Vergleichsvariable, falls Var1 wieder verändert/rausgenommen wurde
                if(is.null(daten$variable)){
                  
                }else{
                  selected_tags <- input_tag()$beschr[input_tag()$id %in% parameter$at_tg] # falls Tags soll das hier auch passen
                  input_var2(indikator_get_variables_by_tags(input_var2(), selected_tags))
                  input_var2(get_compatible_variables(input_var2(), daten$variable))
                }
                
                # von Tag Auswahl übergeben und miteinbeziehen in Datenoutput
                param_at_vr2 <-
                  get_query_param("at_vr2") |>
                  recode_param_int(TRUE)
                
                
                if (!(all(param_at_vr2 %in% parameter$at_vr2) &
                      all(parameter$at_vr2 %in% param_at_vr2))){
                  
                  parameter$at_vr2 <- param_at_vr2
                  daten_var2 <- 1:length(daten$variable2)
                  daten_var2 <- daten_var2[daten_var2 %in% param_at_vr2]
                  
                  if (length(daten_var2) > 0){
                    
                    daten$variable2 <- c()
                    updatePickerInput(session, "variable_vergleichen", selected = NULL)
                    
                    
                  } else if(param_at_vr == ""){
                    
                    daten$variable2 <- c()
                    updatePickerInput(session, "variable_vergleichen", selected = NULL)
                    
                  } else {
                    
                    variable  <- get_query(paste0("SELECT id, beschr FROM variable WHERE id =", param_at_vr))
                    variable2 <- get_query(paste0("SELECT id, beschr FROM variable WHERE id IN ('",
                                                  paste(param_at_vr2, collapse = "', '"), "')"))
                    tabelle <- vergleiche_load_reichweiten_by_multiple_variables(variable$id, variable2$id)
                    
                    daten$gruppen <- sort(tabelle$gruppe)
                    daten$variable2 <- variable2$beschr
                    
                    updatePickerInput(session, "variable_vergleichen", selected = variable2$beschr)
                    
                  }
                  
                  daten$aenderung <- TRUE
                }
                
                
                # PARAMETER AT_GP - Update der Gruppenauswahl / Eimer ________________________________
                
                param_at_gp <-
                  get_query_param("at_gp") |>
                  recode_param_int(vec = TRUE)
                
                if (!(all(param_at_gp %in% parameter$at_gp) &
                      all(parameter$at_gp %in% param_at_gp))){
                  
                  parameter$at_gp <- param_at_gp
                  daten_gruppe <- 1:length(daten$gruppen)
                  daten_gruppe <- daten_gruppe[daten_gruppe %in% param_at_gp]
                  
                  
                  if (length(daten_gruppe) > 0){
                    daten$auswahl <- daten$gruppen[daten_gruppe]
                  } else {
                    daten$auswahl <- c()
                  }
                  
                  if (length(daten$auswahl) > 0){
                    
                    # Änderung, die nicht im UI-Element persistiert ist?
                    eimer_aenderung <- FALSE
                    if (length(input$eimer_unterscheiden) != length(daten$auswahl)){
                      eimer_aenderung <- TRUE
                    } else if (length(daten$auswahl) > 0){
                      if (!(all(daten$auswahl %in% input$eimer_unterscheiden) &
                            all(input$eimer_unterscheiden %in% daten$auswahl))){
                        eimer_aenderung <- TRUE
                      }
                    }
                    if (eimer_aenderung){
                      output$select_reichweite <-
                        renderUI({
                          indikator_draw_eimer(daten$gruppen, daten$auswahl, ns)
                        })
                    }
                    
                    # Filter aktualisieren
                    output$filter_reichweite <-
                      renderUI({
                        indikator_draw_filter(daten$werte, daten$auswahl, ns)
                      })
                    
                  }
                  daten$aenderung <- TRUE
                  
                }
                
                # PARAMETER der Filter _______________________________________________________________
                
                for (filter_id in filter_param$param){
                  # at_re15
                  # filter_id <- "at_re15"
                  param_filter <-
                    get_query_param(filter_id) |>
                    recode_param_int(vec = TRUE)
                  
                  "test <- !(all(param_filter %in% parameter$ERSETZEN) &  all(parameter$ERSETZEN %in% param_filter))" |>
                    gsub(pattern = "ERSETZEN", replacement = filter_id) |>
                    parse(text = _) |>
                    eval()
                  
                  if (test){
                    aenderung <- TRUE
                    
                    "parameter$ERSETZEN <- param_filter" |>
                      gsub(pattern = "ERSETZEN", replacement = filter_id) |>
                      parse(text = _) |>
                      eval()
                    
                    eval(parse(text = paste0("input_filter <- input$", filter_param$bez[filter_param$param == filter_id])))
                    auswahl <-
                      daten$werte[,filter_param$beschr[filter_param$param == filter_id]] |>
                      unique() |>
                      sort()
                    auswahl <- auswahl[param_filter]
                    
                    update_filter <-
                      "updatePickerInput(session, '%s', selected = auswahl)" |>
                      sprintf(fmt = _, filter_param$bez[filter_param$param == filter_id]) |>
                      parse(text = _)
                    
                    if (length(input_filter) != length(auswahl)){
                      eval(update_filter)
                    } else if (length(input_filter) > 0){
                      if (!(all(input_filter %in% auswahl) &
                            all(auswahl %in% input_filter))){
                        eval(update_filter)
                      }
                    }
                    
                    daten_filter <- daten$filter
                    daten_filter <- daten_filter[daten_filter$type != filter_param$beschr[filter_param$param == filter_id],]
                    
                    if (length(auswahl) > 0){
                      daten_filter <-
                        rbind(
                          daten_filter,
                          data.frame(
                            type  = filter_param$beschr[filter_param$param == filter_id],
                            werte = auswahl
                          )
                        )
                    }
                    
                    daten$filter <- daten_filter[!is.na(daten_filter$werte),]
                    rm(update_filter, auswahl, param_filter, daten_filter)
                    
                  }
                  rm(test)
                }
                
                # PARAMETER der Tabsets ____________________________________________________________
                
                param_at_tab <- get_query_param("at_tab")
                if (is.null(param_at_tab)) param_at_tab <- ""
                
                if (!(param_at_tab %in% parameter$at_tab)){
                  
                  parameter$at_tab <- param_at_tab
                  if (length(tab_liste$tabs) > 0 & param_at_tab != ""){
                    tab <- process_parameter_input_to_tab(param_at_tab)
                    if (tab %in% tab_liste$tabs){
                      updateTabsetPanel(session, "tab_menu", tab)
                    } else if (paste(LABEL_TABSET_GRAFIK, 1) %in% tab_liste$tabs){
                      updateTabsetPanel(session, "tab_menu", paste(LABEL_TABSET_GRAFIK, 1))
                    }
                  }
                  
                }
                
              }
            }
          }
          gesperrt(FALSE)
        },
        ignoreNULL = FALSE
      ) # Here is the end of the big observeEvent function
      
      # Observe Input um die URL-Parameter anzupassen
      
      observeEvent(
        input$select_tag, {
          
          current_url      <- session$clientData$url_hash
          if (get_page() %in% URL_PATH){
            if (!gesperrt()){
              
              new_url <-
                param_set(
                  urls  = current_url,
                  key   = "at_tg",
                  value = process_input_for_parameter_tag(input$select_tag, input_tag())
                )
              
              if (new_url != current_url){
                change_page(new_url)
              }
              
            }
          }
        },
        ignoreNULL = TRUE
      )
      
      
      observeEvent(
        input$select_vars, {
          
          current_url <- session$clientData$url_hash
          if (get_page() %in% URL_PATH){
            if (!gesperrt()){
              
              new_url <-
                param_set(
                  urls = current_url,
                  key = "at_vr",
                  value = process_input_for_parameter_vr(input$select_vars, input_var())
                )
              
              if (new_url != current_url){
                change_page(new_url)
              }
              
            }
          }
        },
        ignoreNULL = FALSE
      )
      
      
      observeEvent(
        input$variable_vergleichen, {
          
          current_url <- session$clientData$url_hash
          if (get_page() %in% URL_PATH){
            if (!gesperrt()){
              
              new_value <- process_input_for_parameter_vr2(input$variable_vergleichen, input_var2())
              if (new_value %in% "") daten$variable2 <- c()
              new_url <-
                param_set(
                  urls = current_url,
                  key = "at_vr2",
                  value = new_value
                )
              
              if (new_url != current_url){
                change_page(new_url)
              }
              
            }
          }
          
        },
        ignoreNULL = FALSE
      )
      
      
      observeEvent(
        input$eimer_unterscheiden, {
          
          current_url <- session$clientData$url_hash
          if (get_page() %in% URL_PATH){
            if (!gesperrt()){
              if (length(daten$gruppen) > 0){
                
                new_url     <-
                  param_set(
                    urls  = current_url,
                    key   = "at_gp",
                    value = process_input_for_parameter_gp(daten$gruppen, input$eimer_unterscheiden)
                  )
                
                if (new_url != current_url){
                  change_page(new_url)
                }
                
              }
            }
          }
          
        },
        ignoreNULL = FALSE
      )
      
      observeEvent(
        input$tab_menu, {
          
          current_url <- session$clientData$url_hash
          if (get_page() %in% URL_PATH){
            if (!gesperrt()){
              if (!is.null(input$tab_menu)){
                
                new_url     <-
                  param_set(
                    urls  = current_url,
                    key   = "at_tab",
                    value = process_input_for_parameter_tab(input$tab_menu)
                  )
                
                if (new_url != current_url){
                  change_page(new_url)
                }
                
              }
            }
          }
          
        },
        ignoreNULL = FALSE
      )
      
      for (i in write_explorer_filter_observer(filter_param$bez)){
        eval(parse(text = i))
      }
      
    }
    
  )
  
  
}

#' Missing description
#' @noRd

vergleichen_translate_filter_param <- function(con){
  
  gsub_name <- function(x){gsub("[^a-z]", "", tolower(x))}
  
  # output <-
  #   data.frame(
  #     beschr =
  #       c(
  #         get_query(
  #           "SELECT beschr
  #      FROM reichweite_typ
  #      WHERE reichweite_klasse_id != (SELECT id FROM reichweite_klasse WHERE beschr = 'Räumliche Gebiete')"
  #         )[,1],
  #         "Räumliche Gebiete",
  #         "Zeit"
  #       )
  #   )
  output <-
    data.frame(
      beschr =
        c(
          get_query(
            "SELECT beschr
       FROM reichweite_typ"
          )[,1],
          "Zeit"
        )
    )
  
  output$bez <- gsub_name(output$beschr)
  output$param <- paste0("at_", paste0(substr(output$bez, 1, 1), substr(output$bez, nchar(output$bez), nchar(output$bez)), nchar(output$bez)))
  stopifnot("Parameter der Filter im Explorer sind nicht einzigartig" = all(!duplicated(output$param)))
  return(output)
}

#' Hilfsfunktion Update Link
#' @noRd

recode_param_int <- function(x, vec = FALSE, default = ""){
  if (is.null(x)) return(default)
  if (vec) x <- strsplit(x, ",")[[1]]
  x <- suppressWarnings(as.integer(x))
  if (any(is.na(x))) return(default)
  return(sort(x))
}

#' Hilfsfunktion zieht mögliche Variablen
#' @noRd

indikator_get_variables <- function(con){
  output <- get_query("SELECT id, beschr FROM variable")
  output$relevant <- TRUE
  
  #hf-Filter, falls keine Tags ausgewählt wurden
  # if()
  
  return(output)
}

#' Übersetzung hf zum filtern von tags aus yml file
#' @noRd

indikator_get_tags_by_hfs <- function(hf){
  
  hf_zuordnung <- read_yaml("yml/explorer/handlungsfeld_zuordnung.yml")
  
  tags <- get_sql("indikator_get_tag_by_variable", TRUE)
  
  if(hf == 1){
    tags <- tags[tags$beschr %in% hf_zuordnung$handlungsfeld1, ]
  } else if(hf == 2){
    tags <- tags[tags$beschr %in% hf_zuordnung$handlungsfeld2, ]
  }
  
  return(tags)
  
}


#' Übersetzung tags zum ziehen von Variablen aus DB
#' @noRd

indikator_get_variables_by_tags <- function(variable, selected_tags){
  
  if (length(selected_tags) == 0){
    variable$relevant <- TRUE
    return(variable)
  }
  
  tag_link <-
    get_query(
      "SELECT reihe_id, tags
       FROM view_tag_link_aggregated_beschr
       WHERE tabelle_id = (SELECT id FROM tabelle WHERE bez = 'variable')"
    )
  tag_link <- tag_link[tag_link$reihe_id %in% variable$id,]
  variable_id <-
    tag_link$reihe_id[
      apply(sapply(selected_tags, \(x) grepl(x, tag_link$tags)), 1, any)
    ]
  
  variable$relevant <- variable$id %in% variable_id
  
  return(variable)
}

get_compatible_variables <- function(variable2, variable){
  
  if (length(variable) == 0){
    variable2$relevant <- TRUE
    return(variable2)
  }
  
  vars_link <- get_comparison_variables(variable, skip = TRUE)
  
  variable2$tag_relevant <- variable2$relevant
  variable2$var_relevant <- variable2$beschr %in% vars_link$variable_beschr
  variable2$relevant <- variable2$tag_relevant & variable2$var_relevant
  
  variable2$tag_relevant <- NULL
  variable2$var_relevant <- NULL
  
  return(variable2)
}

#' Erstellt Drag-Drop-Eimer
#' @noRd

indikator_draw_eimer <- function(reichweiten = list(), selected = NULL, ns){
  
  if (is.null(reichweiten)){
    return(HTML(""))
  }
  
  if (length(reichweiten) < 1){
    return(HTML(""))
  }
  
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
  )
}

#' Erstellt Output-Tabelle
#' @noRd

vergleichen_draw_reactable <- function(daten){
  
  if (is.null(daten))  return(reactable(data.frame(keine = "daten")))
  if (nrow(daten) < 1) return(reactable(data.frame(keine = "daten")))
  
  reactable(
    daten[, !names(daten) %in% c("meta_info_einheiten", "meta_info_zeit", "meta_info_quellen")],
    columns = list(), # Links ergänzen
    showSortable = TRUE,
    searchable   = TRUE,
    filterable   = TRUE,
    highlight    = TRUE,
    defaultColDef = colDef(minWidth = 200, align = "center"),
    resizable = TRUE,
    showPageSizeOptions = TRUE,
    language =
      reactableLang(
        pageInfo          = "{rowStart} bis {rowEnd} von {rows} Einträgen",
        pageSizeOptions   = "Einträge pro Seite: {rows}",
        pagePreviousLabel = "Vorherige Seite",
        pageNextLabel     = "Nächste Seite",
        pageNext          = "weiter",
        pagePrevious      = "zurück",
        filterLabel       = "{Spalte} filtern nach...",
        searchPlaceholder = "Suchen..."
      ),
    details = function(index){
      datum <- daten[index,]
      div(
        class = "reactable-details",
        h3(paste0("Details zu der Variable '", datum$`Variable/n`, "'")),
        div(class = "reactable-details-label", "Beschreibung"),
        div(class = "reactable-details", p("kommt noch...")),
        div(class = "reactable-details-label", "Verfügbare Einheiten"),
        div(class = "reactable-details", p(datum$meta_info_einheiten)),
        div(class = "reactable-details-label", "Verfügbare Jahre"), # TODO: dynamisch für versch. Zeiteinheiten!
        div(class = "reactable-details", p(datum$meta_info_zeit)),
        div(class = "reactable-details-label", "Quellenangabe"),
        div(class = "reactable-details", p(datum$meta_info_quellen))

      )
    }
    
    
    
    
  )
  
}

#' url Anpassungen
#' @noRd

write_explorer_filter_observer <- function(filter_param_bez){
  
  gsub_name <- function(x){gsub("[^a-z]", "", tolower(x))}
  
  write_code <- function(x){
    "observeEvent(input$ERSETZEN, {

      current_url <- session$clientData$url_hash
      if (get_page() %in% URL_PATH){
        if (!gesperrt() & !is.null(get_query_param('hf'))){

          param_attr    <- filter_param[filter_param$bez == 'ERSETZEN',]
          auspraegungen <- sort(unique(daten$werte[,param_attr$beschr[1]]))
          auspraegungen <- (1:length(auspraegungen))[auspraegungen %in% input$ERSETZEN]
          current_url   <- session$clientData$url_hash

          new_url <-
            param_set(
              urls  = current_url,
              key   = param_attr$param[1],
              value = paste(auspraegungen, collapse = ',')
            )

          if (new_url != current_url){
            change_page(new_url)
          }

        }
      }
    }, ignoreNULL = FALSE
    )" |>
      gsub(
        pattern = "ERSETZEN",
        replacement = x
      )
  }
  
  return(sapply(filter_param_bez, write_code))
}

#' Missing description
#' @noRd

indikator_draw_filter <- function(werte, unterscheiden = NULL, ns){
  
  if (nrow(werte) < 1){
    return(HTML(""))
  }
  
  filter_ui <- HTML("")
  
  if (is.null(unterscheiden)){
    unterscheiden <- c()
  }
  
  if (length(unterscheiden) > 0){

    if("Zeit" %in% unterscheiden){

      unterscheiden <- setdiff(unterscheiden, "Zeit")

      gsub_name <- function(x){gsub("[^a-z]", "", tolower(x))}
      
      filter_ui <-
        tagList(
          lapply(
            unterscheiden,
            \(x){
              pickerInput(
                inputId  = ns(gsub_name(x)),
                label    = x,
                choices  = setdiff(unique(werte[,x]), "Insgesamt"),
                selected = NULL,
                options  = list(
                  `actions-box`        = TRUE,
                  `none-selected-text` = "nichts ausgewählt",
                  `select-all-text`    = "alle auswählen",
                  `deselect-all-text`  = "nichts auswählen",
                  `live-search`        = TRUE
                ),
                multiple = TRUE
              )
            }
          ),

          sliderTextInput(
            inputId = ns("zeit"),
            label = "Zeit",
            choices = as.numeric(unique(werte$Zeit)),
            selected = as.numeric(median(unique(werte$Zeit))):as.numeric(max(unique(werte$Zeit)))
          )
        )
    }else{

      gsub_name <- function(x){gsub("[^a-z]", "", tolower(x))}

      filter_ui <-
        tagList(
          lapply(
            unterscheiden,
            \(x){
              pickerInput(
                inputId  = ns(gsub_name(x)),
                label    = x,
                choices  = setdiff(unique(werte[,x]), "Insgesamt"),
                selected = NULL,
                options  = list(
                  `actions-box`        = TRUE,
                  `none-selected-text` = "nichts ausgewählt",
                  `select-all-text`    = "alle auswählen",
                  `deselect-all-text`  = "nichts auswählen",
                  `live-search`        = TRUE
                ),
                multiple = TRUE
              )
            }
          )
        )
    }
    
  }
  
  return(filter_ui)
}


vergleiche_load_reichweiten_by_multiple_variables <- function(variable, variable2 = NULL){
  
  # Neu Reichweiten-Typen als Schnittmenge aus Variablen
  reichweite_typ_query <-
    "with

    id as
    (SELECT DISTINCT reihe_id
    FROM view_daten_link
    WHERE tabelle_id = (SELECT id FROM tabelle WHERE bez = 'reichweite_typ')
    AND daten_id IN (SELECT daten_id
               FROM view_daten_link
               WHERE (tabelle_id = (SELECT id FROM tabelle WHERE bez = 'variable'))
               AND reihe_id IN ('%s'))
    ),

    beschr as
    (SELECT id, beschr
    FROM reichweite_typ)

    SELECT a.*, b.*
    FROM
    id a
    INNER JOIN beschr b
    ON a.reihe_id = b.id"
  
  
  reichweite_typ1 <- sprintf(reichweite_typ_query, variable) |> get_query()
  
  if(!is.null(variable2)){
    
    # reichweite_typ2 <- sprintf(reichweite_typ_query, variable2) |> get_query()
    # reichweite_typ <-  c(intersect(reichweite_typ1$beschr, reichweite_typ2$beschr), "Zeit")
    
    reichweite_typen_list <- lapply(variable2, function(var){
      sprintf(reichweite_typ_query, var) |> get_query()
    })
    reichweite_typen_list <- c(reichweite_typen_list, list(reichweite_typ1))
    beschr_list <- lapply(reichweite_typen_list, function(df) df$beschr)
    reichweite_typ <- c(Reduce(intersect, beschr_list), "Zeit")
    
  }else{
    reichweite_typ <- c(reichweite_typ1$beschr, "Zeit")
  }
  
  
  daten <-
    get_query(
      sprintf(
        "SELECT daten.id, variable.beschr as variable, zeit_start, wert, wert_einheit.beschr as einheit
         FROM daten
         LEFT JOIN wert_einheit ON daten.wert_einheit_id = wert_einheit.id
         LEFT JOIN variable     ON daten.variable_id = variable.id
         WHERE variable_id = %s",
        variable
      )
    )
  
  reichweite <-
    "SELECT reichweite.id as id, reichweite.beschr as reichweite, rtyp.beschr as reichweite_typ
     FROM reichweite
     LEFT JOIN reichweite_typ rtyp ON reichweite.reichweite_typ_id = rtyp.id
     WHERE reichweite.id IN (SELECT reichweite_id FROM daten_reichweite WHERE daten_id IN (SELECT id FROM daten WHERE variable_id = %s))" |>
    sprintf(variable) |>
    get_query()
  
  daten_reichweite <-
    get_query(
      sprintf(
        "SELECT daten_id, reichweite_id
         FROM daten_reichweite
         WHERE daten_id IN (SELECT id FROM daten WHERE variable_id = %s)",
        variable
      )
    )
  
  reichweite$gruppe <- reichweite$reichweite_typ
  
  daten[,unique(reichweite$gruppe)] <- NA
  
  for (i in 1:nrow(daten)){
    x <- daten_reichweite[daten_reichweite$daten_id == daten$id[i],]
    for (j in 1:nrow(x)){
      k <- reichweite$id == x$reichweite_id[j]
      l <- c(daten[i, reichweite$gruppe[k]], reichweite$reichweite[k])
      l <- l[!is.na(l)]
      daten[i, reichweite$gruppe[k]] <- paste(l, collapse = ", ")
    }
  }
  
  for (i in unique(reichweite$gruppe)){
    daten[,i] <- ifelse(is.na(daten[,i]), "Insgesamt", daten[,i])
  }
  
  names(daten) <- gsub("zeit_start", "Zeit", names(daten))
  daten$Zeit <- format(as.Date(daten$Zeit), "%Y")
  
  return(list(daten = daten, gruppe = reichweite_typ))
  
}

#' @noRd
process_input_for_parameter_tag <- function(input_select_tag, vergleichen_tags){
  new_value        <- ""
  if (!is.null(input_select_tag)){
    if (nrow(vergleichen_tags) > 0){
      new_value <-
        paste(
          sort(vergleichen_tags$id[vergleichen_tags$beschr %in% input_select_tag]),
          collapse = ","
        )
    }
  }
  return(URLencode(as.character(new_value)))
}

#' @noRd
process_input_for_parameter_gp <- function(daten_gruppen, input_eimer_unterscheiden){
  new_value <- (1:length(daten_gruppen))[daten_gruppen %in% input_eimer_unterscheiden]
  if (length(new_value) != 0){
    new_value <- paste(new_value, collapse = ",")
  } else {
    new_value <- ""
  }
  return(URLencode(as.character(new_value), reserved = TRUE))
}

#' @noRd
process_input_for_parameter_vr <- function(input_select_var, vergleichen_variable){
  new_value <- ""
  if (!is.null(input_select_var) & nrow(vergleichen_variable) > 0){
    new_value <- (vergleichen_variable$id[vergleichen_variable$beschr %in% input_select_var])[1]
  }
  return(URLencode(as.character(new_value), reserved = TRUE))
}

#' @noRd
process_input_for_parameter_vr2 <- function(input_select_var, vergleichen_variable2){
  new_value <- ""
  if (!is.null(input_select_var) & nrow(vergleichen_variable2) > 0){
    multi_new_values <- vergleichen_variable2$id[vergleichen_variable2$beschr %in% input_select_var]
    new_value        <- paste(multi_new_values, collapse = ",")
  } else {
    #daten$variable2 <- c()
    new_value <- ""
  }
  return(URLencode(as.character(new_value), reserved = TRUE))
}

#' @noRd
process_input_for_parameter_tab <- function(input_wert){
  new_value <- ""
  if (grepl(LABEL_TABSET_TABELLE, input_wert) | grepl(LABEL_TABSET_GRAFIK, input_wert)){
    new_value <- input_wert
    new_value <- gsub(LABEL_TABSET_TABELLE, "tab",    new_value)
    new_value <- gsub(LABEL_TABSET_GRAFIK,  "abb", new_value)
    new_value <- gsub(" ", "", new_value)
  }
  return(URLencode(as.character(new_value), reserved = TRUE))
}

#' @noRd
process_parameter_input_to_tab <- function(parameter){
  new_value <- ""
  if (grepl("tab", parameter) | grepl("abb", parameter)){
    new_value <- parameter
    new_value <- gsub("tab", LABEL_TABSET_TABELLE,             new_value)
    new_value <- gsub("abb", paste0(LABEL_TABSET_GRAFIK, " "), new_value)
  }
  if (new_value %in% ""){
    new_value <- LABEL_TABSET_TABELLE
  }
  return(new_value)
}

#' Missing description
#' @noRd
# 

render_h <- function(text, color){
  h2(text, style = sprintf("color: %s;", color)) |>
    renderUI()
}

# draw_explorer_details <- function(type){
#   
#   # text_in_box <- markdown(readLines("md/explorer_standard.md"))
#   text_in_box <- ""
#   
#   if (type == "handlung1") {
#     color_class <- "handlung1_triangle"
#     titel <- "Analyse - Bildung & Kompetenzen"
#   } else if (type == "handlung2") {
#     color_class <- "handlung2_triangle"
#     titel <- "Analyse - Forschung & Innovationen"
#   } else {
#     color_class <- "keine_handlung_triangle"
#     titel <- "Analyse - Alle Indikatoren"
#   }
#   
#   return(
#     tagList(
#       h2(titel, style = "text-align: center;"),
#       div(
#         style = "display: flex; flex-direction: row; margin: 1%;",
#         div(class = color_class, style = "width: 200px; height: auto; min-width: 200px; min-height: 200px;"),
#         div(
#           style = "padding: 0 30px;",
#           div(
#             style = "width: 100%;",
#             text_in_box
#           )
#         )
#       )
#     )
#   )
#   
# }

#' @noRd
add_reactable_footer <- function(results){
  if(!is.null(nrow(results))){
    if(nrow(results) > 1){
      
      return(
        fluidRow(
          p(),
          p(style = "text-align: center; color: var(--grey);", paste0("Quelle(n):", results))
        )
      )
      
    }
  }
  return(HTML(""))
}
