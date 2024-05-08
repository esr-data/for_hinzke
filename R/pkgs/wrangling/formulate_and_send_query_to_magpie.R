
box::use(
  . / get_data[query_magpie],
  stringr[str_count]
)

#' Used as internal function
#' Function to find simplest reichweite category if none is submitted
#' @noRd

search_simplest_category <- function(possible_kombis_reichweite){
  
  if ("Deutschland" %in% possible_kombis_reichweite$reichweite_beschr_list){
    reichweite_select <- "Deutschland"
  } else {
    i <- min(str_count(possible_kombis_reichweite$reichweite_beschr_list, "\\|"))
    reichweite_select <-
      possible_kombis_reichweite$reichweite_beschr_list[
        str_count(possible_kombis_reichweite$reichweite_beschr_list,"\\|") == i
      ]
  }
  
  return(reichweite_select)
}


#' Used as internal function
#' Function to retrieve data
#' @noRd

formulate_and_send_query_to_magpie <- function(
    possible_kombis_reichweite,
    variable,
    time,
    time_period,
    group,
    filter,
    filter_typ,
    filter_combis_df,
    reichweite_typ_in, reichweite_in,
    con = NULL
){
  cols <-
    paste(
      c(
        "daten_id", "wert", "variable_beschr", "zeit_einheit", "wert_einheit",
        "zeit_start", "zeit_ende", "reichweite_beschr_list", "reichweite_typ_list"
      ),
      collapse = ", "
    )
  #für mengen "reichweite_id_list", "reichweite_menge_id_list"
  
  # Variable filtern & Spalten auswählen
  basis_query <-
    sprintf("SELECT  %s
            FROM mview_daten_reichweite_menge
            WHERE variable_beschr IN ('%s')",
            cols, paste(variable, collapse = "', '"))
  
  # Zeit filtern
  # Zeitpunkt
  if (is.null(time) &
      (!is.null(time_period) & length(time_period) == 1)){
    
    time <- time_period
    
  }
  
  if (!is.null(time)){
    
    zeit_query_start <- " "
    zeit_query <- sprintf(
      " AND extract(year from zeit_start) IN ('%s')",
      paste(time, collapse = "', '")
    )
    
    # Zeitspanne
  } else if (!is.null(time_period)){
    
    zeit_query_start <- " "
    zeit_query <- paste0(
      " AND extract(year from zeit_start) IN ('",
      paste(time_period[1]:time_period[2], collapse = "','"),
      "')"
    )
    
  } else if (is.null(time) & is.null(time_period) &
             is.null(group) &
             is.null(filter)){
    zeit_query_start <- " "
    zeit_query <- " "
    
  } else if(is.null(time) & is.null(time_period) &
            !is.null(group)){ # es gibt nur group ODER (group UND filter) -> sobald es group gibt, schauen wir auf reichweite_typ_in
    #Maximaler Zeitpunkt als Default
    zeit_query_start <- paste0(
      " with

            date as
            (SELECT MAX(zeit_start) as max
            FROM mview_daten_reichweite_menge
            WHERE variable_beschr IN ('", paste(variable, collapse = "', '"), "')
             AND typ_list = '", reichweite_typ_in, "'),

            mview as
            ("
    )
    zeit_query <- paste0(
      ")

          SELECT a.max, b.*
          FROM
          date a
          INNER JOIN mview b
          ON a.max = b.zeit_start"
    )
  }  else if(is.null(time) &
             is.null(time_period) &
             !is.null(filter) &
             is.null(group)){
    
    zeit_query_start <- paste0(
      " with

            date as
            (SELECT MAX(zeit_start) as max
            FROM mview_daten_reichweite_menge
            WHERE variable_beschr IN ('", paste(variable, collapse = "', '"), "')
            AND reichweite_beschr_list = '", reichweite_in[1], "'),

            mview as
            ("
    )
    zeit_query <- paste0(
      ")

          SELECT a.max, b.*
          FROM
          date a
          INNER JOIN mview b
          ON a.max = b.zeit_start"
    )
  }
  
  # Gruppen-/Filterkategorie filtern
  if(is.null(group) & is.null(filter)){
    reichweite_select <- search_simplest_category(possible_kombis_reichweite)
    reichweite_typ_query <- paste0(
      " AND reichweite_beschr_list = '", reichweite_select[1], "'"
    )
  }
  
  if(
    !is.null(group) &
    is.null(filter)
  ){
    reichweite_typ_query <- paste0(
      " AND typ_list = '", reichweite_typ_in, "'"
    )
  }
  
  if(
    !is.null(filter) &
    is.null(group)
  ){
    
    reichweite_typ_query <- paste0("
        AND (reichweite_beschr_list IN ('",
                                   paste(reichweite_in, collapse = "', '"),
                                   "'))"
    )
    
  }
  
  if(
    !is.null(filter) &
    !is.null(group)
  ){
    
    if(length(unique(filter_typ)) == 1){ # gleiche Gruppe von Filtern --> OR
      reichweite_typ_query <- paste0(
        "AND typ_list = '", reichweite_typ_in, "'
      AND (reichweite_beschr_list LIKE '%",
        paste(filter, collapse = "%' OR reichweite_beschr_list LIKE '%"),
        "%')"
      )
    } else{
      if(length(unique(filter_typ)) == length(filter_typ)){
        reichweite_typ_query <- paste0(
          "AND typ_list = '", reichweite_typ_in, "'
      AND (reichweite_beschr_list LIKE '%",
          paste(filter, collapse = "%' AND reichweite_beschr_list LIKE '%"),
          "%')"
        )
      } else if(length(unique(filter_typ)) < length(filter_typ)) # Mischung
        filter_combis_df <- filter_combis_df |>
          dplyr::rowwise() |>
          dplyr::mutate(input = paste(dplyr::c_across(everything()), collapse = "%' AND reichweite_beschr_list LIKE '%")) |>
          dplyr::mutate(input = paste0(input, "%') "))
      
      reichweite_typ_query <- paste0(
        "AND typ_list = '", reichweite_typ_in, "'
        AND ((reichweite_beschr_list LIKE '%",
        paste(filter_combis_df$input, collapse = " OR (reichweite_beschr_list LIKE '%"), ")")
    }
  }
  
  
  df <-
    query_magpie(
      paste0(
        zeit_query_start,
        basis_query,
        reichweite_typ_query,
        zeit_query
      ),
      con = con
    )
  
  return(df)
}


