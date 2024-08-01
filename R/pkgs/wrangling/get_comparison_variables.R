box::use(
#  ../../R/utils/database[get_query]
  . / get_data[query_magpie],
  duckdb[duckdb]
)

get_comparison_variables <-
  function(
    start_variable,
    ueberschneidung_wert_einheit = TRUE,
    ueberschneidung_zeit_einheit = TRUE,
    ueberschneidung_zeit_auspraegung = TRUE,
    ueberschneidung_reichweite_reichweitetyp = TRUE,
    con = NULL,
    skip = skip
  ){

    timestamp <- Sys.time()

    wert_einheit <- "wert_einheit,"[ueberschneidung_wert_einheit]
    zeit_einheit <- "zeit_einheit,"[ueberschneidung_zeit_einheit]
    zeit_start <- "zeit_start,"[ueberschneidung_zeit_auspraegung]
    zeit_ende <- "zeit_ende,"[ueberschneidung_zeit_auspraegung]
    reichweite_typ_list <- "reichweite_typ_list"[ueberschneidung_reichweite_reichweitetyp]
    join_condition_wert_einheit <- "VAR_START.wert_einheit = VERGLEICH.wert_einheit AND "[ueberschneidung_wert_einheit]
    join_condition_zeit_einheit <- "VAR_START.zeit_einheit = VERGLEICH.zeit_einheit AND "[ueberschneidung_zeit_einheit]
    join_condition_zeit_start <- "VAR_START.zeit_start = VERGLEICH.zeit_start AND "[ueberschneidung_zeit_auspraegung]
    join_condition_zeit_ende <- "VAR_START.zeit_ende = VERGLEICH.zeit_ende AND "[ueberschneidung_zeit_auspraegung]
    join_condition_reichweite_typ_list <- "VAR_START.reichweite_typ_list = VERGLEICH.reichweite_typ_list"[ueberschneidung_reichweite_reichweitetyp]

    columns <-
      paste0(
        wert_einheit,
        zeit_einheit,
        zeit_start,
        zeit_ende,
        reichweite_typ_list
      )

    join_statement <-
      paste0(
        join_condition_wert_einheit,
        join_condition_zeit_einheit,
        join_condition_zeit_start,
        join_condition_zeit_ende,
        join_condition_reichweite_typ_list
      )

    if (!ueberschneidung_reichweite_reichweitetyp){

      columns <-
        substring(columns, first = 1, last = nchar(columns) - 1)

      join_statement <-
        substring(join_statement, first = 1, last = nchar(join_statement) - 4)

    }

    if(skip == FALSE){

      if(is.null(con)) con <- dbConnect(duckdb(), "data/magpie.db", read_only = TRUE)

      vergleichs_variablen <-
        get_query(
          paste0(
            "WITH VAR_START AS (
       SELECT DISTINCT ", paste(columns, collapse = "', '"),
            " FROM wrangling_input_view_daten
           WHERE variable_beschr ='", start_variable, "'),

       VERGLEICH AS (
       SELECT variable_beschr, ", paste(columns, collapse = "', '"),
            " FROM wrangling_input_view_daten
            WHERE variable_beschr !='", start_variable, "')

      SELECT DISTINCT VERGLEICH.variable_beschr
      FROM VERGLEICH
      INNER JOIN VAR_START
          ON ",  join_statement
          ),
          con = con
        )

    }else{
      vergleichs_variablen <-
        query_magpie(
          skip = skip,
          paste0(
            "WITH VAR_START AS (
            SELECT DISTINCT ", paste(columns, collapse = "', '"),
            " FROM wrangling_input_view_daten
           WHERE variable_beschr ='", start_variable, "'),

       VERGLEICH AS (
       SELECT variable_beschr, ", paste(columns, collapse = "', '"),
            " FROM wrangling_input_view_daten
            WHERE variable_beschr !='", start_variable, "')

      SELECT DISTINCT VERGLEICH.variable_beschr
      FROM VERGLEICH
      INNER JOIN VAR_START
          ON ",  join_statement
          ),
          con = con
        )
    }

    message(paste("Timestamp:", round(difftime(Sys.time(), timestamp, units = "secs"), 2), "Sekunden"))

    return(vergleichs_variablen)

  }
