
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
  if (df$zeit_einheit[1] == "Jahr") {
    df$zeit <- format(as.Date(df$zeit_start, format = "%Y-%m-%d"), "%Y")
    df$zeit <- as.numeric(df$zeit)
  }
  
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
  name_wert <- paste0("Wert (",unique(df$wert_einheit), ")")
  name_zeit <- paste0("Zeit (",unique(df$zeit_einheit), ")")
  
  df <- df |>
    select(-zeit_einheit, -wert_einheit)
  
  colnames(df)[1] <- name_wert
  colnames(df)[2] <- name_zeit
  colnames(df)[3] <- "Variable/n"
  
  df <- df |>
    select(-1, -2, everything(), 1:2)
  
  
  return(df)
}
