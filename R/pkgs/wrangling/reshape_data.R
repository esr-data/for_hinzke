
box::use(
  dplyr[select, mutate, mutate_at, vars],
  tidyr[separate]
)

#' Used as internal function
#' Funktion zum Extrahieren und Zusammenführen
#' @noRd

extracting_associated_categories <- function(string) {
  strsplit(string, " \\| ")[[1]] |>
    sapply(function(x)
      gsub(".*::([^:]+)", "\\1", x)) |>
    paste(collapse = " | ")
}

#' Used as internal function
#' reshape data
#' @noRd

reshape_data <- function(df) {

 # browser()

  if (all(length(unique(df$zeit_einheit)) == 1 & unique(df$zeit_einheit) == "Jahr")) {
    df$zeit <- format(as.Date(df$zeit_start, format = "%Y-%m-%d"), "%Y")
    df$zeit <- as.numeric(df$zeit)
  } else if(all(length(unique(df$zeit_einheit)) == 1 & unique(df$zeit_einheit) == "Jahre")){
    df$zeit <-
      paste(
        substr(df$zeit_start, 1, 4),
        "-",
        substr(df$zeit_ende, 1, 4)
      )
  } else if(all(length(unique(df$zeit_einheit)) > 1 &
            "Jahr" %in% unique(df$zeit_einheit) &
            "Jahre" %in% unique(df$zeit_einheit))){

    df1 <- df[df$zeit_einheit == "Jahr",]
    df1$zeit <- format(as.Date(df1$zeit_start, format = "%Y-%m-%d"), "%Y")
    df1$zeit <- as.numeric(df1$zeit)

    df2 <- df[df$zeit_einheit == "Jahre",]
    df2$zeit <-
      paste(
        substr(df2$zeit_start, 1, 4),
        "-",
        substr(df2$zeit_ende, 1, 4)
      )

    df <- rbind(df1, df2)
  }


  # Details extrahieren & aufbereiten


  # Quelle extrahieren & aufbereiten
  df_quellen <- unique(df$quelle_list)
  df_quellen <- trimws(unlist(strsplit(df_quellen, "\\|")))
  df_quellen <- unique(df_quellen[!df_quellen %in% c("", "---")])
  df_quellen <- paste0(" ", paste(df_quellen, collapse = ", "), ".")

  df <- df[, names(df)[names(df) != "quelle_list"]]

  df <- select(df,-c(zeit_start, zeit_ende))

  # bei mehreren Gruppen-Vars: Kategorien in eigene Spalten schreiben

  # Kategorien in gleicher Reihenfolge wie reichweite bringen und auslesen aus reichweite_typ_list
  df$reichweite_typ_list <- sapply(df$reichweite_typ_list,
                                   extracting_associated_categories)

  # Indikatoren als neue Spaltenüberschriften auslesen
  indikatoren <-
    strsplit(unique(df$reichweite_typ_list), " \\| ")[[1]]
  indikatoren <- indikatoren[indikatoren != "|"]

  # Reichweiten teilen und den Spalten zuordnen
  df <-
    df |>
    separate(reichweite_beschr_list, indikatoren, sep = " \\| ") |>
    select(-reichweite_typ_list,-daten_id)

  if (sum(grepl("max", colnames(df))) == 1) {
    df <- select(df,-max)
  }

  # sortieren für Typen-Zuweisung
  df <- df |>
    select(wert,
           wert_einheit,
           zeit,
           zeit_einheit,
           variable_beschr,
           everything())

  if(!("Faktorlevel" %in% df$wert_einheit)){
    df <- df |>
      mutate_at(vars(6:ncol(df)), as.factor) |>
      mutate(wert = as.numeric(wert))
  }else{
    df <- df |>
      mutate_at(vars(6:ncol(df)), as.factor) |>
      mutate(wert = as.character(wert))
  }

  #sortieren für Umbenennung und final
  df <- df[with(df, order(zeit, decreasing = FALSE)),]
  
  name_wert <- paste0("Wert (", paste(unique(df$wert_einheit), collapse = ", "), ")")
  name_zeit <- paste0("Zeit (", paste(unique(df$zeit_einheit), collapse = ", "), ")")

  df <- df |>
    select(-zeit_einheit, -wert_einheit)

  colnames(df)[1] <- name_wert
  colnames(df)[2] <- name_zeit
  colnames(df)[3] <- "Variable/n"


  df <- df |>
    select(-1, -2, everything(), 1:2)

  return(
    list(
      df = df,
      df_quellen = df_quellen
      )
    )

}
