#' Missing description
#' @noRd
#'
load_table_by_variable_monitor <- function(variable){

  # browser()

  daten <-
    dbGetQuery(
      con,
      sprintf(
        "SELECT daten.id, variable.beschr as variable, jahr, wert, wert_einheit.beschr as einheit
         FROM daten
         LEFT JOIN wert_einheit ON daten.wert_einheit_id = wert_einheit.id
         LEFT JOIN variable     ON daten.variable_id = variable.id
         WHERE variable_id = %s",
        variable
      )
    )

  reichweite <-
    "SELECT reichweite.id as id, reichweite.beschr as reichweite, rtyp.beschr as reichweite_typ, rklasse.beschr as reichweite_klasse
     FROM reichweite
     LEFT JOIN reichweite_typ rtyp ON reichweite.reichweite_typ_id = rtyp.id
     LEFT JOIN reichweite_klasse rklasse ON rtyp.reichweite_klasse_id = rklasse.id
     WHERE reichweite.id IN (SELECT reichweite_id FROM daten_reichweite WHERE daten_id IN (SELECT id FROM daten WHERE variable_id = %s))" |>
    sprintf(variable) |>
    dbGetQuery(conn = con)

  daten_reichweite <-
    dbGetQuery(
      con,
      sprintf(
        "SELECT daten_id, reichweite_id
         FROM daten_reichweite
         WHERE daten_id IN (SELECT id FROM daten WHERE variable_id = %s)",
        variable
      )
    )

  reichweite$gruppe <-
    ifelse(
      reichweite$reichweite_klasse == "Räumliche Gebiete",
      reichweite$reichweite_klasse,
      reichweite$reichweite_typ
    )

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

  names(daten) <- gsub("jahr", "Zeit", names(daten))

  # daten <- daten %>%
  #   tidyr::pivot_longer(
  #     cols = c(everything(), - c(id, variable, Zeit, wert, einheit)),
  #     names_to = "Gliederung",
  #     values_to = "Kategorie"
  #   ) %>%
  #   dplyr::filter(!is.na(Kategorie)) %>%
  #   dplyr::select(- id)

  return(list(daten = daten, gruppe = unique(reichweite$gruppe)))
}

#' Missing description
#' @noRd

manage_explorer_data_monitor <- function(werte, gruppe = NULL, unterscheiden = NULL, filtern = NULL){

  # browser()

  unterscheiden_string <- deparse(substitute(unterscheiden))

  if (nrow(werte) < 1) return(NULL)

  if (is.null(gruppe)){
    gruppe <- c()
  }

  neue_gruppe <- gruppe
  if (is.null(unterscheiden)){
    unterscheiden <- c()
  }

  nicht_unterscheiden <- gruppe[!(gruppe %in% unterscheiden)]

  if (!is.null(filtern)){
    if (nrow(filtern) > 0){
      for (i in unique(filtern$type)){
        werte <- werte[werte[,match(i, names(werte))] %in% filtern[filtern$type == i,"werte"],]
      }
    }
  }

  for (i in nicht_unterscheiden){
    if ("Insgesamt" %in% werte[,i]) {
      werte <- werte[werte[,i] == "Insgesamt", names(werte) != i]
    } else if ("Deutschland" %in% werte[,i]) {
      werte <- werte[werte[,i] == "Deutschland", names(werte) != i]
    } else {
      x <- table(werte[,i])
      x <- (names(x)[max(x) == x])[1]
      werte <- werte[werte[,i] == x, names(werte) != i]
      rm(x)
    }
  }

  # if (!("Zeit" %in% unterscheiden)){
  #   werte <- werte[werte$Zeit == max(werte$Zeit),]
  # }

  names(werte)[names(werte) == "einheit"] <- "Messeinheit"
  names(werte)[names(werte) == "wert"] <- "Wert"
  names(werte)[names(werte) == "Zeit"] <- "Jahr"
  werte$Wert <- as.numeric(werte$Wert)

  neue_gruppe <- gruppe[!(gruppe %in% nicht_unterscheiden)]
  return(werte[,c("Jahr", "Wert", "Messeinheit", neue_gruppe)])
}

#' Missing description
#' @noRd

monitor_indicator_main_content_ui <- function(id, var_table) {
  ns <- NS(id)

  auswahlmoeglichkeiten <- setdiff(unique(colnames(var_table$daten)), c("id", "variable", "Jahr", "Wert", "Messeinheit")) #TODO noch weiter aus Funktion rausziehen?

  fluidPage(
    column(
      width = 3,
      selectInput(
        inputId = ns("gliederungsauswahl1_in"),
        label = "Bitte wählen Sie eine Gliederungsebene",
        choices = c("---", auswahlmoeglichkeiten)
      ),
      conditionalPanel(
        condition = sprintf("input.%s != '---'", ns("gliederungsauswahl1_in")),
        pickerInput(
          inputId = ns("kategorieauswahl1_in"),
          label = "Bitte wählen Sie, welche Daten Ihnen angezeigt werden sollen",
          choices = "---",
          options  = list(
            `actions-box`        = TRUE,
            `none-selected-text` = "nichts ausgewählt",
            `select-all-text`    = "alle auswählen",
            `deselect-all-text`  = "nichts auswählen",
            `live-search`        = TRUE
          ),
          multiple = TRUE
        )
      ),
      radioButtons(
        inputId = ns("darstellungsweise1_in"),
        label = "Darstellungsweise:",
        choices = c("Zeitverlauf", "Tabelle"),
        selected = "Zeitverlauf"
      )
    ),
    column(
      width = 9,
      conditionalPanel(
        condition = sprintf("input.%s == 'Karte'", ns("darstellungsweise1_in")),
        numericInput(
          inputId = ns("karte1_jahr_in"),
          label = "Jahr:",
          value = max(var_table$daten$Jahr),
          min = min(var_table$daten$Jahr),
          max = max(var_table$daten$Jahr)
        )
      ),
      uiOutput(ns("output_list"))
    )
  )
}

#' Missing description
#' @noRd

monitor_indicator_main_content_server <- function(id, var_table) {
  moduleServer(id, function(input, output, session) {

    # reactives

    filtered_data_gliederung <- reactive({
      manage_explorer_data_monitor(
        werte = var_table$daten,
        gruppe = var_table$gruppe,
        unterscheiden = input$gliederungsauswahl1_in
      )
    })

    filtered_data_gliederung_kategorie <- reactive({
      if (input$gliederungsauswahl1_in != "---" & !is.null(input$gliederungsauswahl1_in)) {
        filter_col <- input$gliederungsauswahl1_in
        filter_kat <- input$kategorieauswahl1_in

        return(filtered_data_gliederung() %>% filter(.[[filter_col]] %in% filter_kat))
      } else {
        return(NULL)
      }
    })

    filtered_data_gliederung_kategorie_karte <- reactive({
      req(filtered_data_gliederung_kategorie())
      if (input$darstellungsweise1_in == "Karte") {
        return(filtered_data_gliederung_kategorie() %>% filter(.[["Jahr"]] == as.numeric(input$karte1_jahr_in)))
      } else {
        return(NULL)
      }
    })

    plot_data <- reactive({
      if (input$gliederungsauswahl1_in == "---") {
        filtered_data_gliederung()
      } else if (input$darstellungsweise1_in == "Karte") {
        filtered_data_gliederung_kategorie_karte()
      } else {
        filtered_data_gliederung_kategorie()
      }
    })

    # observes

    observeEvent(input$gliederungsauswahl1_in, {
      if(input$gliederungsauswahl1_in != "---") {
        if(input$gliederungsauswahl1_in %in% colnames(filtered_data_gliederung())) {
          unique_values <- unique(filtered_data_gliederung()[[input$gliederungsauswahl1_in]])
          selected_values <- NULL

          if(length(unique_values) < 6) {
            selected_values <- unique_values
          } else {
            if(input$gliederungsauswahl1_in == "Räumliche Gebiete") {
              selected_values <- c("Deutschland", "Bayern", "Baden-Württemberg", "Nordrhein-Westfalen")
            } else {
              selected_values <- unique_values[1:5]
            }
          }

          updatePickerInput(
            session = session,
            inputId = "kategorieauswahl1_in",
            choices = unique_values,
            selected = selected_values
          )
        }
      } else {
        updatePickerInput(
          session = session,
          inputId = "kategorieauswahl1_in",
          choices = NULL,
          selected = NULL
        )
      }
    })

    observe({
      if (input$gliederungsauswahl1_in == "Räumliche Gebiete") {
        updateRadioButtons(
          session = session,
          inputId = "darstellungsweise1_in",
          choices = c("Zeitverlauf", "Tabelle", "Karte"),
          selected = input$darstellungsweise1_in
        )
      } else {
        updateRadioButtons(
          session = session,
          inputId = "darstellungsweise1_in",
          choices = c("Zeitverlauf", "Tabelle"),
          selected = ifelse(
            input$darstellungsweise1_in != "Karte",
            input$darstellungsweise1_in,
            "Zeitverlauf")
        )
      }
    })

    observe({
      if (input$darstellungsweise1_in == "Karte") {
        updateNumericInput(
          session = session,
          inputId = "karte1_jahr_in",
          value = max(filtered_data_gliederung_kategorie()$Jahr),
          min = min(filtered_data_gliederung_kategorie()$Jahr),
          max = max(filtered_data_gliederung_kategorie()$Jahr)
        )
      } else {
        updateNumericInput(
          session = session,
          inputId = "karte1_jahr_in",
          value = NULL,
          min = NULL,
          max = NULL
        )
      }
    })

    # outputs

    output$output_list <- renderUI({

      if (input$darstellungsweise1_in == "Tabelle" & nrow(plot_data() > 0)) {

        #TODO: Auslagerung Grundfunktionen in svVis

        renderDataTable({
          datatable(
            plot_data(),
            extensions = 'Buttons',
            options = list(
              dom = 'lBfrtip',
              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
              pageLength = 20,
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#185365', 'color': '#fff'});",
                "}"
              ),
              language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json')
            )
          )
        })

      } else if(input$darstellungsweise1_in == "Zeitverlauf" & nrow(plot_data() > 0)) {

        #TODO: Auslagerung Grundfunktionen in svVis

        renderPlotly({
          if(input$gliederungsauswahl1_in == "---") {
            config(
              toImageButtonOptions = list(filename = str_c("SV Monitor Wissenschaft")),
              modeBarButtonsToRemove = c("lasso2d", "zoomIn2d", "zoomOut2d"),
              locale = "de",
              displaylogo = FALSE,
              p = ggplotly(
                ggplot(plot_data(), aes(x = Jahr, y = Wert)) +
                  geom_line() +
                  suppressWarnings(geom_point(aes(
                    text = str_c("Jahr: ", Jahr, "<br>Wert: ",
                                 prettyNum(round(Wert, 1), big.mark = ".", decimal.mark = ","))), color = "#195365")) +
                  theme_pubr() +
                  theme(plot.title = element_text(hjust = -0.45, vjust = 2.12),
                        plot.margin = unit(c(1, 0.5, 1, 1), "cm"),
                        axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0)),
                        axis.text.x = element_text(angle = 45, hjust = 1)
                  ) +
                  labs(
                    x = "",
                    y = str_c(plot_data()$Messeinheit[1], "\n"),
                    title = str_c("", "\n")
                  ) +
                  scale_x_continuous(breaks = c(
                    min(plot_data()$Jahr):max(plot_data()$Jahr)
                  )) +
                  scale_color_manual(values = "#195365")
              )
            ) %>%
              layout(
                annotations =
                  list(
                    x = 1,
                    y = -0.3,
                    text = paste0(
                      "Quelle: ",
                      "TODO plot_data()$Quelle[1]."
                    ),
                    showarrow = F,
                    xref = 'paper',
                    yref = 'paper',
                    xanchor = 'right',
                    yanchor = 'auto',
                    xshift = 0,
                    yshift = 0,
                    font = list(size = 10, color = "black")
                  )
              )
          } else if (ncol(plot_data() == 4)) {
            gliederungsauswahl <- input$gliederungsauswahl1_in
            config(
              toImageButtonOptions = list(filename = str_c("SV Monitor Wissenschaft - ", " gegliedert nach ")),
              modeBarButtonsToRemove = c("lasso2d", "zoomIn2d", "zoomOut2d"),
              locale = "de",
              displaylogo = FALSE,
              p = ggplotly(
                ggplot(
                  plot_data(),
                  aes(x = Jahr, y = Wert, col = !!sym(input$gliederungsauswahl1_in))
                ) +
                  geom_line() +
                  suppressWarnings(geom_point(aes(
                    text = str_c("Jahr: ", Jahr, "<br>Kategorie: ", gliederungsauswahl, "<br>Wert: ",
                                 prettyNum(round(Wert, 1), big.mark = ".", decimal.mark = ","))))) +
                  theme_pubr() +
                  theme(plot.title = element_text(hjust = -0.45, vjust = 2.12),
                        plot.margin = unit(c(1, 0.5, 1, 1), "cm"),
                        axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0)),
                        axis.text.x = element_text(angle = 45, hjust = 1)
                  ) +
                  labs(
                    x = "",
                    y = str_c(plot_data()$Messeinheit[1], "\n"),
                    title = str_c("", "\n")
                  ) +
                  scale_x_continuous(breaks = c(
                    min(
                      plot_data()$Jahr
                    ):max(
                      plot_data()$Jahr
                    )
                  )) +
                  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",",
                                                                 scientific = FALSE)) +
                  scale_color_manual(
                    values = Farben,
                    name = gliederungsauswahl, drop = FALSE
                  ), tooltip = "text"
              )
            ) %>%
              layout(
                annotations =
                  list(
                    x = 1,
                    y = -0.3,
                    text = paste0(
                      "Quelle: ",
                      "TODOplot_data()$Quelle[1]", "."
                    ),
                    showarrow = F,
                    xref = 'paper',
                    yref = 'paper',
                    xanchor = 'right',
                    yanchor = 'auto',
                    xshift = 0,
                    yshift = 0,
                    font = list(size = 10, color = "black")
                  )
              )
          }
        })

      # } else if (input$darstellungsweise1_in == "Karte" & nrow(plot_data() > 0)) {  #TODO Performantere, nicht brechende Choropleth-Funktion
      #
      #   x <- plot_data() %>%
      #     rename("Region" = `Räumliche Gebiete`)
      #
      #   renderPlot({
      #     create_choropleth_map_germany(x, Wert)
      #   })

      } else {
        renderPlotly({
          ggplotly(ggplot() +
                     annotate("text", x = 2, y = 2, label = "Bitte Daten auswählen.") +
                     labs(x = "", y = "") +
                     theme_pubr() +
                     theme(line = element_blank(),
                           axis.text = element_blank()),
                   tooltip = NULL) %>%
            layout(xaxis = list(fixedrange = TRUE)) %>%
            layout(yaxis = list(fixedrange = TRUE)) %>%
            config(displayModeBar = FALSE)

        })}
    })

  })
}

# test module

ui <- fluidPage(
  monitor_indicator_main_content_ui("mein_modul_id", var_table)
)

server <- function(input, output, session) {
  monitor_indicator_main_content_server("mein_modul_id", var_table)
}

shinyApp(ui, server)
