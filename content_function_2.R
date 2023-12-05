load_table_by_variable <- function(variable){

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

  return(daten)
}

x <- manage_explorer_data( # TODO
  werte         = var_table$wert,
  gruppe        = var_table$gruppen[var_table$gruppen != "Zeit"],
  unterscheiden = var_table$auswahl,
  filtern       = var_table$filter
)

manage_explorer_data <- function(werte, gruppe = NULL, unterscheiden = NULL, filtern = NULL){

  # werte         <<- werte
  # gruppe        <<- gruppe
  # unterscheiden <<- unterscheiden
  # filtern       <<- filtern

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

  if (!("Zeit" %in% unterscheiden)){
    werte <- werte[werte$Zeit == max(werte$Zeit),]
  }

  neue_gruppe <- gruppe[!(gruppe %in% nicht_unterscheiden)]
  return(werte[,c("Zeit", "wert", "einheit", neue_gruppe)])
}

var_table <- load_table_by_variable(138)

ui <- function() {
  fluidPage(
    selectInput(
      inputId = "gliederungsauswahl1_in",
      label = "Bitte wählen Sie eine Gliederungsebene",
      choices = c("---", unique(var_table$Gliederung))
    ),
    conditionalPanel(
      condition = "input.gliederungsauswahl1_in !=  '---'",
      pickerInput(
      inputId = "kategorieauswahl1_in",
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
    uiOutput("output_list")
  )
}

server <- function(input, output, session) {

  # reactives

  filtered_data_gliederung <- reactive({
    var_table %>%
      filter(Gliederung == input$gliederungsauswahl1_in)
  })

  filtered_data_gliederung_kategorie <- reactive({
    filtered_data_gliederung() %>%
      filter(Kategorie %in% input$kategorieauswahl1_in)
  })

  # observes


  observeEvent(input$gliederungsauswahl1_in, {

    updatePickerInput(
      session = session,
      inputId = "kategorieauswahl1_in",
      choices = unique(
        filtered_data_gliederung()$Kategorie
      ),
      selected = unique(
        filtered_data_gliederung()$Kategorie
      )
    )
  })

  # outputs

  output$output_list <- renderUI({
    if (input$gliederungsauswahl1_in == "---") {
      renderTable(filter(var_table, Gliederung == "Räumliche Gebiete", Kategorie == "Deutschland"))
    } else {
      renderTable(filtered_data_gliederung_kategorie())
    }
  })

}

shinyApp(ui, server)
# profvis::profvis(shinyApp(ui, server)) #
