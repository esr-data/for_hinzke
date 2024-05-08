
box::use(
  . / get_data[query_magpie]
)

#' Used as internal function
#' checking available grouping (reichweiten_typen) & filter (reichweiten) for selected variable/s
#' @noRd

get_compatible_input <- function(variable, con = NULL, silent = FALSE){
  
  # Mögliche Werte pro Variable identifizieren
  possible_time            <- get_possible_values(variable, "zeit_einheit", con)
  possible_units           <- get_possible_values(variable, "wert_einheit", con)
  possible_kombis_reichweite       <- get_possible_values(variable, "reichweite_beschr_list", con)
  possible_kombis_reichweite_typen <- get_possible_values(variable, "typ_list", con)
  possible_kombis_reichweite_und_reichweite_typ <- get_possible_values(variable, "reichweite_typ_list", con)
  
  # Bei mehreren Variable Schnittmenge aus Möglichem bilden
  if(length(variable) > 1){
    possible_time <- keep_compatible_values(possible_time, variable, silent = silent)
    possible_unit <- keep_compatible_values(possible_units, variable, silent = silent)
    possible_kombis_reichweite_typen <- keep_compatible_values(possible_kombis_reichweite_typen, 
                                                               variable, silent = silent)
    possible_kombis_reichweite       <- keep_compatible_values(possible_kombis_reichweite, variable, 
                                                               silent = silent)
    possible_kombis_reichweite_und_reichweite_typ <- keep_compatible_values(
      possible_kombis_reichweite_und_reichweite_typ, variable, silent = silent)
  }
  
  return(
    list(
      possible_time = possible_time,
      possible_units = possible_units,
      possible_kombis_reichweite = possible_kombis_reichweite,
      possible_kombis_reichweite_typen = possible_kombis_reichweite_typen,
      possible_kombis_reichweite_und_reichweite_typ = possible_kombis_reichweite_und_reichweite_typ
    )
  )
}

#' Used as internal function
#' getting available options for given variable(s)
#' @noRd

get_possible_values <- function(variable, column, con = con){
  query_magpie(
    paste0(
      "SELECT DISTINCT variable_beschr, ", column, " ",
      "FROM mview_daten_reichweite_menge
     WHERE variable_beschr IN ('", paste(variable, collapse = "', '"), "')"
    ),
    con = con
  )
}


#' Used as internal function
#' checking compatibility
#' @noRd

keep_compatible_values <- function(values, variable, silent = FALSE){
  
  unique_values <- data.frame(value = unique(values[,2]))
  for(i in 1:length(variable)){
    unique_values[,paste0("variable_", i)] <-
      unique_values$value %in% unique(values[values[,1] == variable[i], 2])
  }
  
  if (any(apply(unique_values[,2:ncol(unique_values)], 1, all))){
    values <- values[values[,2] %in% unique_values$value[apply(unique_values[,2:ncol(unique_values)], 1, all)],]
  } else {
    if (!silent){
      message(
        sprintf(
          "The %s of the chosen variables are not compatible. Keep that in mind when further processing the data.",
          names(values)[2]
        )
      )
    }
  }
  return(values)
}
