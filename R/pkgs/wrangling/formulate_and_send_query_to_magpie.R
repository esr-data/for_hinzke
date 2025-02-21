
box::use(
  . / get_data[query_magpie],
  stringr[str_count],
  dplyr[rowwise, mutate, c_across, group_by, filter, distinct]
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
#' Function to retrieve and structure meta data for each variable
#' @noRd
get_variable_meta_data <-
  function(variable_beschr, skip, con){
#browser()
    query_meta_data <-
      paste0(
        "SELECT variable_beschr,
                zeit_start, zeit_ende, zeit_einheit,
                wert_einheit, quelle_list, tag_list
         FROM wrangling_input_view_daten
         WHERE variable_beschr IN ('",
        paste(variable_beschr, collapse = "', '"),
        "')"
      )

    meta_data_raw <-
      query_magpie(
        skip,
        query_meta_data,
        con = con
      )

    meta_data_prepared <-
      data.frame(
        variable_beschr = unique(meta_data_raw$variable_beschr),
        meta_info_einheiten = rep(NA, length(unique(meta_data_raw$variable_beschr))),
        meta_info_zeit      = rep(NA, length(unique(meta_data_raw$variable_beschr))),
        meta_info_quellen    = rep(NA, length(unique(meta_data_raw$variable_beschr)))
      )

    for(i in 1:nrow(meta_data_prepared)){

      sub <- meta_data_raw[meta_data_raw$variable_beschr == meta_data_prepared$variable_beschr[i], ]

      meta_data_prepared$meta_info_einheiten[i] <- paste(unique(sub$wert_einheit), collapse = ", ")

      variable_quellen <- unique(sub$quelle_list)
      variable_quellen <- trimws(unlist(strsplit(variable_quellen, "\\|")))
      variable_quellen <- unique(variable_quellen[!variable_quellen %in% c("", "---")])
      meta_data_prepared$meta_info_quellen[i] <- paste0(" ", paste(variable_quellen, collapse = ", "), ".")

      vorhandene_zeit_einheiten <- unique(sub$zeit_einheit)

      if(all(length(vorhandene_zeit_einheiten) == 1 & vorhandene_zeit_einheiten == "Jahr")){ # TODO: was, wenn nicht?

        jahre <- unique(substr(as.character(sub$zeit_start),1 , 4))
        min_jahr <- min(as.numeric(jahre))
        max_jahr <- max(as.numeric(jahre))
        seq_jahre <- seq(min_jahr, max_jahr, 1)

        if(all(seq_jahre == as.numeric(jahre))){

          meta_data_prepared$meta_info_zeit[i] <- paste0(min_jahr, " bis ", max_jahr)

        } else {

          meta_data_prepared$meta_info_zeit[i] <- paste(jahre[order(jahre)], collapse = ", ")

        }


      } else if (all(length(vorhandene_zeit_einheiten) == 1 & vorhandene_zeit_einheiten == "Jahre")){

        df_jahre <-
          sub |>
          distinct(
            zeit_start,
            zeit_ende
          ) |>
          group_by(
            zeit_start,
            zeit_ende
          ) |>
          mutate(
            zeit_spanne = paste0(
              "Jahre ",
              substr(zeit_start, 1, 4),
              "-",
              substr(zeit_ende, 1, 4)
            )
          )

        meta_data_prepared$meta_info_zeit[i] <- paste(unique(df_jahre$zeit_spanne), collapse = ", ")


      } else if (all(length(vorhandene_zeit_einheiten) > 1 &
                 "Jahr" %in% vorhandene_zeit_einheiten &
                 "Jahre" %in% vorhandene_zeit_einheiten)){

        sub_1 <-
          sub[sub$zeit_einheit == "Jahr",]

        sub_2 <-
          sub[sub$zeit_einheit == "Jahre",]

        jahre <- unique(substr(as.character(sub_1$zeit_start), 1 , 4))
        min_jahr <- min(as.numeric(jahre))
        max_jahr <- max(as.numeric(jahre))
        seq_jahre <- seq(min_jahr, max_jahr, 1)

        df_jahre <-
          sub_2 |>
          distinct(
            zeit_start,
            zeit_ende
          ) |>
          group_by(
            zeit_start,
            zeit_ende
          ) |>
          mutate(
            zeit_spanne = paste0(
              "Jahre ",
              substr(zeit_start, 1, 4),
              "-",
              substr(zeit_ende, 1, 4)
            )
          )

        meta_data_prepared$meta_info_zeit[i] <-
          paste0(
            "Einzelangaben für: ",
            seq_jahre,
            "; Durchschnittsangaben für: ",
            df_jahre$zeit_spanne,
            "."
          )



      }

    }

    return(meta_data_prepared)


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
    con = NULL,
    skip = skip
){
  cols <-
    paste(
      c(
        "daten_id", "wert", "variable_beschr", "zeit_einheit", "wert_einheit",
        "zeit_start", "zeit_ende", "reichweite_beschr_list", "reichweite_typ_list",
        "quelle_list"
      ), # TODO: "tag_list" hinzu und im weiteren Verlauf verwurschteln
      collapse = ", "
    )
  #für mengen "reichweite_id_list", "reichweite_menge_id_list"

  # Variable filtern & Spalten auswählen
  basis_query <-
    sprintf("SELECT  %s
            FROM wrangling_input_view_daten
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
            FROM wrangling_input_view_daten
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
            FROM wrangling_input_view_daten
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
          rowwise() |>
          mutate(input = paste(c_across(everything()), collapse = "%' AND reichweite_beschr_list LIKE '%")) |>
          mutate(input = paste0(input, "%') "))

      reichweite_typ_query <- paste0(
        "AND typ_list = '", reichweite_typ_in, "'
        AND ((reichweite_beschr_list LIKE '%",
        paste(filter_combis_df$input, collapse = " OR (reichweite_beschr_list LIKE '%"), ")")
    }
  }

  df <-
    query_magpie(
      skip,
      paste0(
        zeit_query_start,
        basis_query,
        reichweite_typ_query,
        zeit_query
      ),
      con = con
    )


  df_meta_data <-
    get_variable_meta_data(
      variable_beschr = unique(df$variable_beschr),
      skip,
      con = con
    )

  df <- # TODO: hübscher & direkter Lösen
    merge(
      df,
      df_meta_data,
      by = "variable_beschr"
    )



  return(df)
}


