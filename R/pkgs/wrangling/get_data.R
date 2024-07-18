
box::use(
  ../../utils/database[get_query, get_sql],
  . / get_compatible_input[get_compatible_input],
  . / get_possible_input[get_possible_input],
  . / formulate_and_send_query_to_magpie[formulate_and_send_query_to_magpie],
  . / reshape_data[reshape_data],

  duckdb[duckdb],
  checkmate[check_character],
  stringr[str_replace]
)

#' Hauptfunktion wrangling der DB Daten für Weiternutzung
#' @export

get_data <- function(
    variable,
    group  = NULL,
    filter = NULL,
    time = NULL,  time_period = NULL,
    time_measure = NULL, measure = NULL,
    silent = TRUE, skip = TRUE,
    con = NULL
){

  # --- step 0: package loading for R version ----
  
  if(!skip){
    
    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(svMagpie, DBI, checkmate, stringr, dplyr, tidyr)
    
  }
  
  
  # --- step 1: Check if input is correct (isolated check for different parameters) ----
  # only, if not from shiny
  if (skip == FALSE){
    
    if(is.null(con)) con <- connect_magpie()
    
    on.exit(dbDisconnect(con))
    
    variable <-
      check_existence(
        variable,
        "variable",
        silent = silent,
        con = con
      )
    
    stopifnot("Cannot proceed without at least one valid variable input." = !is.null(variable))
    
    if (length(group) > 0){
      group <-
        check_existence(
          group,
          "reichweite_typ",
          silent = silent,
          con    = con
        )
    }
    
    if (length(filter) > 0){
      filter <-
        check_existence(
          filter,
          "reichweite",
          silent = silent,
          con    = con
        )
    }
    
    if (length(time_measure) > 1){
      if (!silent){
        message("Considering different time measures is not possible. Only your first entry will be considered.")
      }
      time_measure <- time_measure[1]
    }
    
    if (!is.null(time_measure)){
      time_measure <-
        check_existence(
          time_measure,
          "zeit_einheit",
          silent = silent,
          con    = con
        )
    }
    
    if (!is.null(measure)){
      measure <-
        check_existence(
          measure,
          "wert_einheit",
          silent = silent,
          con    = con
        )
    }
  }else{
    if(is.null(variable)) return(NULL)
  }
  # --- step 2: get available categories for selected variable/s -----------------------
  
  possible_input <-
    get_compatible_input(
      variable,
      con = con,
      silent = silent,
      skip = skip
    )
  
  # --- step 3: check if chosen selection for selected variables is possible -----------
  
  filter_typ <- NULL
  
  if(length(c(filter, group)) > 0){
    
    
    
    if(!is.null(group)& is.null(filter)){
      filter_typ_df <-NULL
    }else{
      filter_typ_df <-
        align_reichweite_typen(
          #  variable,
          filter = filter,
          filter_typ,
          con = con
        )
      filter_typ <- filter_typ_df$rwt_beschr
      filter_combis_df <-
        expand.grid(
          split(filter_typ_df$rw_beschr, filter_typ_df$rwt_beschr)
        )
      
    }
  }
  
  
  # das klappt nicht aber für was genau wird das dann genutzt?
  # filter_combis <-
  #   ifelse(length(unique(filter_typ_df$rwt_beschr)) > 1,
  #   apply( filter_combis_df[ , 1:length(names(filter_combis_df))] , 1 , paste , collapse = ", " ),# BUG!!!
  #   filter_typ_df$rw_beschr)
  # } --> ggf gar nicht mehr notwendig?
  
  if (!silent){
    message(
      "Possible grouping options for this/these variable/s are: ",
      paste0(
        "\n",
        paste(
          unique(possible_input$possible_kombis_reichweite_typen$typ_list),
          collapse = "\n"
        )
      )
    )
  }
  
  
  # if(!is.null(group) & !is.null(filter)){
  
  reichweite_typ_in <-
    c(
      group,
      filter_typ
    ) |>
    unique()
  
  #  }
  
  reichweite_in <-
    filter |>
    unique()
  
  
  ## --- check match with possibilities ----------------------------------------
  if (
    length(c(group, filter)) > 0
  ){
    input_list <-
      get_possible_input(
        reichweite_typ_in = reichweite_typ_in,
        reichweite_in = reichweite_in, # momentan liste
        possible_input = possible_input,
        group = group,
        filter = filter,
        filter_combis = filter_combis,
        filter_typ = filter_typ,
        con = con,
        silent = silent, 
        skip = skip
      )
  }
  
  # --- step 4: retrieve selected data -------------------------------------------------
  
  df <-
    formulate_and_send_query_to_magpie(
      possible_kombis_reichweite = possible_input$possible_kombis_reichweite,
      variable = variable,
      time = time,
      time_period = time_period,
      group = group,
      filter = filter,
      filter_typ = filter_typ,
      filter_combis_df = filter_combis_df,
      reichweite_typ_in = input_list$reichweite_typ_in,
      reichweite_in = input_list$reichweite_in, # ergebnis ist liste
      con = con,
      skip = skip
    )

  
  # --- step 5: restructure data for plotting ------------------------------------------
  
  if(skip == FALSE){
    stopifnot(
      "Selected grouping and categorie variables are not available.
     Please select from the list of possible grouping options." = dim(df)[1] != 0
    )
  }else{
    
    if(dim(df)[1] == 0) return(NULL)
    
  }
  
  
  df <- reshape_data(df)
  
  
  return(df)
}


#' Ruft Suche bei Magpie auf
#' @export

query_magpie <- function(skip, query, con){

  if(skip == FALSE){
    return(dbGetQuery(con, query))
  }else{
    return(get_query(x = query))
  }
  
}

#' Prüft ob Varialbe vorhanden ist - brauchen wir hier nicht
#' @export

check_existence <-
  function(x,
           table_name,
           column = "beschr",
           silent = FALSE,
           con = NULL){
    
    if (!check_character(x, any.missing = FALSE)){
      stop(sprintf("Please select (a) valid vector of %s to plot.", table_name))
    }
    
    check <-
      x %in%
      query_magpie(
        skip = skip,
        sprintf("SELECT %s FROM %s", column, table_name),
        con = con
      )[,column]
    
    if (sum(check) == 0){
      
      if (!silent){
        message(
          sprintf(
            "none of your passed %s can be found. Please check for typos.",
            table_name
          )
        )
      }
      return(NULL)
      
    } else {
      
      if (sum(check) > 0 & (sum(check) < length(var))){
        if (!silent){
          message(
            paste(
              "Warning: The following",
              table_name,
              "could not be found:",
              paste(x[!check], collapse = ", "),
              ". Please check for typos. Proceeding with remaining",
              table_name,
              "only: ",
              paste(x[check], collapse = ", ")
            )
          )
        }
      } else {
        if (!silent){
          message(sprintf("All passed elements of %s found.", table_name))
        }
      }
      
    }
    
    return(x[check])
  }

#' Passt Ebenen von Filter (reichweiten) zu Gruppen (reichweite_typen) an
#' @export

align_reichweite_typen <- function(
    filter,
    filter_typ = NULL,
    con = NULL
){
  
  if (!(is.null(filter))){
    
    filter_typ_df <-
      paste0(
        "SELECT rw.beschr AS rw_beschr, rwt.beschr AS rwt_beschr
           FROM reichweite rw
           LEFT JOIN reichweite_typ rwt
           ON rw.reichweite_typ_id = rwt.id
           WHERE rw.beschr IN ('", paste(filter, collapse = "', '"), "')"
      ) |>
      query_magpie(con = con, skip = skip)
  }

  
  return(
    filter_typ_df
  )
}


