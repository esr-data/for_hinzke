
box::use(
  ../../utils/database[get_query]
)

#' Hauptfunktion wrangling der DB Daten für Weiternutzung
#' @export


get_data <-
  function(
    variablen_ids,
    group_ids = NULL,
    filter_ids = NULL,
    time_period
  ){

    query_base <-
      "SELECT v.* FROM daten d
                   LEFT JOIN view_daten v ON d.id = v.id
                   WHERE d.variable_id IN (%s)" |>
      sprintf(paste(variablen_ids, collapse = ","))


    query_time <-
    "AND extract(year from d.zeit_start) >= %s
                   AND extract(year from d.zeit_ende) <= %s" |>
      sprintf(min(time_period), max(time_period))

    query_group_step_1 <-
      "WITH daten_reichweite_typ AS (
        SELECT daten_reichweite.daten_id, daten_reichweite.reichweite_id, reichweite.reichweite_typ_id
        FROM daten_reichweite
        LEFT JOIN reichweite ON daten_reichweite.reichweite_id = reichweite.id)"

    query_group_step_2 <-
      "AND d.id IN (
        SELECT daten_id FROM daten_reichweite_typ WHERE reichweite_typ_id IN (%s)
      )" |>
      sprintf(paste(group_ids, collapse = ","))

    query_filter <-
     " AND d.id IN (
        SELECT daten_id FROM daten_reichweite_typ WHERE reichweite_id IN (%s)
    )" |>
      sprintf(paste(filter_ids, collapse = ","))

    if (length(group_ids) == 0){ # TODO - anders lösen
      group_ids <- NULL
      }


    if (all(is.null(c(group_ids, filter_ids)))){

      df <-
        paste(
          query_base,
          query_time
        )   |>
        get_query()

    } else if (all(is.null(filter_ids))){

      df <-
        paste(
          query_group_step_1,
          query_base,
          query_time,
          query_group_step_2
        )   |>
        get_query()

    } else {

      df <-
        paste(
          query_group_step_1,
          query_base,
          query_time,
          query_group_step_2,
          query_filter
        )   |>
        get_query()

    }

    # --- reshape ----
    # TODO: in eigene fkt auslagern?

    df$zeit <- substr(df$zeit_start, 1, 4) # SUPER UNFLEXIBEL!
    df <- df[, c("variable", "zeit", "region", "reichweite", "wert", "einheit")]
    names(df) <- c("Variable/n", "Zeit (Jahr)", "Region", "Reichweite/n", "Wert", "Werteinheit")

    return(df)

  }


##### back up ##################################################################
# --- ALT ----------------------------------------------------------------------
#'
#' get_data <- function(
#'     variable,
#'     group  = NULL,
#'     filter = NULL,
#'     time = NULL,  time_period = NULL,
#'     time_measure = NULL, measure = NULL,
#'     silent = TRUE, skip = TRUE,
#'     con = NULL
#' ){
#'
#'   # --- step 0: package loading for R version ----
#'
#'   if(!skip){
#'
#'     if (!require("pacman")) install.packages("pacman")
#'     pacman::p_load(DBI, checkmate, stringr, dplyr, tidyr)
#'
#'   }
#'
#'
#'   # --- step 1: Check if input is correct (isolated check for different parameters) ----
#'   # only, if not from shiny
#'   if (skip == FALSE){
#'
#'     if(is.null(con)) con <- connect_magpie()
#'
#'     on.exit(dbDisconnect(con))
#'
#'     variable <-
#'       check_existence(
#'         variable,
#'         "variable",
#'         silent = silent,
#'         con = con
#'       )
#'
#'     stopifnot("Cannot proceed without at least one valid variable input." = !is.null(variable))
#'
#'     if (length(group) > 0){
#'       group <-
#'         check_existence(
#'           group,
#'           "reichweite_typ",
#'           silent = silent,
#'           con    = con
#'         )
#'     }
#'
#'     if (length(filter) > 0){
#'       filter <-
#'         check_existence(
#'           filter,
#'           "reichweite",
#'           silent = silent,
#'           con    = con
#'         )
#'     }
#'
#'     if (length(time_measure) > 1){
#'       if (!silent){
#'         message("Considering different time measures is not possible. Only your first entry will be considered.")
#'       }
#'       time_measure <- time_measure[1]
#'     }
#'
#'     if (!is.null(time_measure)){
#'       time_measure <-
#'         check_existence(
#'           time_measure,
#'           "zeit_einheit",
#'           silent = silent,
#'           con    = con
#'         )
#'     }
#'
#'     if (!is.null(measure)){
#'       measure <-
#'         check_existence(
#'           measure,
#'           "wert_einheit",
#'           silent = silent,
#'           con    = con
#'         )
#'     }
#'   }else{
#'     if(is.null(variable)) return(NULL)
#'   }
#'   # --- step 2: get available categories for selected variable/s -----------------------
#'
#'   possible_input <-
#'     get_compatible_input(
#'       variable,
#'       con = con,
#'       silent = silent,
#'       skip = skip
#'     )
#'
#'   # --- step 3: check if chosen selection for selected variables is possible -----------
#'
#'   filter_typ <- NULL
#'
#'   if(length(c(filter, group)) > 0){
#'
#'
#'
#'     if(!is.null(group)& is.null(filter)){
#'       filter_typ_df <-NULL
#'     }else{
#'       filter_typ_df <-
#'         align_reichweite_typen(
#'           #  variable,
#'           filter = filter,
#'           filter_typ,
#'           con = con,
#'           skip = skip
#'         )
#'       filter_typ <- filter_typ_df$rwt_beschr
#'       filter_combis_df <-
#'         expand.grid(
#'           split(filter_typ_df$rw_beschr, filter_typ_df$rwt_beschr)
#'         )
#'
#'     }
#'   }
#'
#'
#'   # das klappt nicht aber für was genau wird das dann genutzt?
#'   # filter_combis <-
#'   #   ifelse(length(unique(filter_typ_df$rwt_beschr)) > 1,
#'   #   apply( filter_combis_df[ , 1:length(names(filter_combis_df))] , 1 , paste , collapse = ", " ),# BUG!!!
#'   #   filter_typ_df$rw_beschr)
#'   # } --> ggf gar nicht mehr notwendig?
#'
#'   if (!silent){
#'     message(
#'       "Possible grouping options for this/these variable/s are: ",
#'       paste0(
#'         "\n",
#'         paste(
#'           unique(possible_input$possible_kombis_reichweite_typen$typ_list),
#'           collapse = "\n"
#'         )
#'       )
#'     )
#'   }
#'
#'
#'   # if(!is.null(group) & !is.null(filter)){
#'
#'   reichweite_typ_in <-
#'     c(
#'       group,
#'       filter_typ
#'     ) |>
#'     unique()
#'
#'   #  }
#'
#'   reichweite_in <-
#'     filter |>
#'     unique()
#'
#'
#'   ## --- check match with possibilities ----------------------------------------
#'   if (
#'     length(c(group, filter)) > 0
#'   ){
#'     input_list <-
#'       get_possible_input(
#'         reichweite_typ_in = reichweite_typ_in,
#'         reichweite_in = reichweite_in, # momentan liste
#'         possible_input = possible_input,
#'         group = group,
#'         filter = filter,
#'         filter_combis = filter_combis,
#'         filter_typ = filter_typ,
#'         con = con,
#'         silent = silent,
#'         skip = skip
#'       )
#'
#'     test3 <<- input_list
#'   }
#'
#'   # --- step 4: retrieve selected data -------------------------------------------------
#'
#'   df <-
#'     formulate_and_send_query_to_magpie(
#'       possible_kombis_reichweite = possible_input$possible_kombis_reichweite,
#'       variable = variable,
#'       time = time,
#'       time_period = time_period,
#'       group = group,
#'       filter = filter,
#'       filter_typ = filter_typ,
#'       filter_combis_df = filter_combis_df,
#'       reichweite_typ_in = input_list$reichweite_typ_in,
#'       reichweite_in = input_list$reichweite_in, # ergebnis ist liste
#'       con = con,
#'       skip = skip
#'     )
#' test4 <<- df
#'
#'   # --- step 5: restructure data for plotting ------------------------------------------
#'
#'   if(skip == FALSE){
#'     stopifnot(
#'       "Selected grouping and categorie variables are not available.
#'      Please select from the list of possible grouping options." = dim(df)[1] != 0
#'     )
#'   }else{
#'
#'     if(dim(df)[1] == 0) return(NULL)
#'
#'   }
#'
#'   reshaped_data <- reshape_data(df)
#'   df <- reshaped_data$df
#'   df_quellen <- reshaped_data$df_quellen
#'
#'
#'   return(
#'     list(
#'       df = df,
#'       df_quellen = df_quellen
#'     )
#'   )
#' }
#'
#'
#' #' Ruft Suche bei Magpie auf
 #' @export
#'
query_magpie <- function(skip, query, con){

  if(skip == FALSE){
    return(dbGetQuery(con, query))
  }else{
    return(get_query(x = query))
  }

}
#'
#' #' Prüft ob Varialbe vorhanden ist - brauchen wir hier nicht
#' #' @export
#'
#' check_existence <-
#'   function(x,
#'            table_name,
#'            column = "beschr",
#'            silent = FALSE,
#'            con = NULL){
#'
#'     if (!check_character(x, any.missing = FALSE)){
#'       stop(sprintf("Please select (a) valid vector of %s to plot.", table_name))
#'     }
#'
#'     check <-
#'       x %in%
#'       query_magpie(
#'         skip = skip,
#'         sprintf("SELECT %s FROM %s", column, table_name),
#'         con = con
#'       )[,column]
#'
#'     if (sum(check) == 0){
#'
#'       if (!silent){
#'         message(
#'           sprintf(
#'             "none of your passed %s can be found. Please check for typos.",
#'             table_name
#'           )
#'         )
#'       }
#'       return(NULL)
#'
#'     } else {
#'
#'       if (sum(check) > 0 & (sum(check) < length(var))){
#'         if (!silent){
#'           message(
#'             paste(
#'               "Warning: The following",
#'               table_name,
#'               "could not be found:",
#'               paste(x[!check], collapse = ", "),
#'               ". Please check for typos. Proceeding with remaining",
#'               table_name,
#'               "only: ",
#'               paste(x[check], collapse = ", ")
#'             )
#'           )
#'         }
#'       } else {
#'         if (!silent){
#'           message(sprintf("All passed elements of %s found.", table_name))
#'         }
#'       }
#'
#'     }
#'
#'     return(x[check])
#'   }
#'
#' #' Passt Ebenen von Filter (reichweiten) zu Gruppen (reichweite_typen) an
#' #' @export
#'
#' align_reichweite_typen <- function(
#'     filter,
#'     filter_typ = NULL,
#'     con = NULL,
#'     skip
#' ){
#'
#'   if (!(is.null(filter))){
#'
#'     filter_typ_df <-
#'       paste0(
#'         "SELECT rw.beschr AS rw_beschr, rwt.beschr AS rwt_beschr
#'            FROM reichweite rw
#'            LEFT JOIN reichweite_typ rwt
#'            ON rw.reichweite_typ_id = rwt.id
#'            WHERE rw.beschr IN ('", paste(filter, collapse = "', '"), "')"
#'       ) |>
#'       query_magpie(con = con, skip = skip)
#'   }
#'
#'
#'   return(
#'     filter_typ_df
#'   )
#' }


