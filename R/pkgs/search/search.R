box::use(DBI, stringr, dplyr, tidyr, utils, tibble, rlist, stats[aggregate])

#' @title Durchsuchen der Magpie-Datenbank
#' @description
#' Mithilfe der Funktion kann die Magpie-Datenbank durchsucht werden. Dabei werden i.d.R. die verschiedenen Strings (bez_lang, beschr, beschr_lang, descr) der Tabellen (u.a. variable, tag, reichweite usw.) nach dem Suchbegriff durchsucht. Die Zeichen `&` und `|` fungieren dabei als logisches `und` und `oder`.
#' @param search_term Character, Suchwort nach dem die Magpie-Datenbank durchsucht wird
#' @param data_to_search_through Datentabelle, die nach dem Suchwort durchsucht wird. Wenn `data_to_search_through = NULL`, wird die Funktion `get_data_to_search` verwendet, um die Tabelle zu generieren.
#' @param min_ranking_weight Numeric, Ranking-Gewichte, die kleiner sind werden rausgeschmissen. Wenn `min_ranking_weight = 0`, dann werden alle Ergebnisse ausgegeben.
#' @param table Characters, ein Vektor mit den Namen der Tabellen die durchsucht werden sollen. Wenn `table = NA`, dann werden alle Tabellen durchsucht.
#' @param filter_for todo
#' @param max_distance todo
#' @param limit_results_to todo
#' @param print_message Boolean, sollen Log-Nachrichten ausgegeben werden?
#' @return tibble-Dataframe mit den Suchergebnissen
#' @examples
#' search_tables("chemie")
#' search_tables("Forschung & Entwicklung")
#' @export

search_tables <- function(
    search_term,
    data_to_search_through = NULL,
    min_ranking_weight     = 0.55,
    table                  = NA,
    filter_for             = NA,
    max_distance           = 2,
    limit_results_to       = NA,
    beautify_results       = TRUE,
    print_messages         = to_print_log()
){

  if (is.null(data_to_search_through)){
    data_to_search_through <- get_data_to_search()
  }

  search_strs <- split_into_search_terms(search_term)
  operators   <- extract_operators(search_term = search_term)
  results <-
    lapply(
      search_strs,
      function (search_str){
        # In der Lapply: Nach Operatoren getrennte String durchsuchen ...

        # Teil-String ggf. weiter aufspalten falls Leerzeichen vorhanden sind.
        search_strs_i <- strsplit(search_str, " ")[[1]]
        n_i <- length(search_strs_i)

        result_i <-
          lapply(
            search_strs_i, \(.){
              # Tatsächliches Durchsuchen des nun endgültig aufgetrennten Strings:
              output <- search_prepared_string(., data_to_search_through, max_distance, min_ranking_weight, print_messages = print_messages)
              output <- output[,c("id", "id_in_table", "table", "column", "ranking_weight")]
              if (nrow(output) > 0){
                output$ranking_weight <- output$ranking_weight / n_i
                output$search_str <- .
              } else {
                output[1,] <- NA
                output$search_str <- .
                output <- output[0,]
              }
              return(output)
            }
          ) |>
          rlist::list.rbind()

        if (nrow(result_i) < 1){
          names(result_i)[names(result_i) == "id_in_table"]    <- "reihe_id"
          names(result_i)[names(result_i) == "table"]          <- "tabelle"
          names(result_i)[names(result_i) == "ranking_weight"] <- "ranking"
          names(result_i)[names(result_i) == "search_str"]     <- "ranking_list"
          return(result_i[,c("reihe_id", "tabelle", "ranking", "ranking_list")])
        }

        result_search_str <-
          aggregate(
            ranking_weight ~ id_in_table + table + search_str,
            data = result_i,
            FUN  = max
          )

        result_agg <-
          aggregate(
            ranking_weight ~ id + id_in_table + table + column,
            data = result_i,
            FUN  = sum
          )

        output_i <-
          aggregate(
            ranking_weight ~ id_in_table + table,
            result_agg,
            max
          ) |>
          tibble::tibble()
        output_i$suchbegriff <- search_str
        output_i$columns <-
          lapply(
            paste(output_i$id_in_table, output_i$table),
            \(.) result_i$column[paste(result_agg$id_in_table, result_agg$table) %in% .]
          )
        output_i$search_ids <-
          lapply(
            paste(output_i$id_in_table, output_i$table),
            \(.) result_agg$id[paste(result_agg$id_in_table, result_agg$table) %in% .]
          )
        output_i <- output_i[output_i$ranking_weight >= min_ranking_weight,]

        result_search_str$ranking_list <- paste(preprocess_str(result_search_str$search_str), round(result_search_str$ranking_weight, 2), sep = ":")
        result_search_str <- aggregate(ranking_list ~ table + id_in_table, result_search_str, \(.) paste(., collapse = ","))

        output_i$ranking_list <-
          result_search_str$ranking_list[
            match(
              paste(output_i$table, output_i$id_in_table),
              paste(result_search_str$table, result_search_str$id_in_table)
            )
          ] |>
          lapply(
            \(ranking){
              strsplit(strsplit(ranking, ",", fixed = TRUE)[[1]], ":") |>
                unlist() |>
                {\(.)
                  {
                    return_list        <- .[2 + (1:(length(.) / 2) - 1) * 2]
                    names(return_list) <- .[1 + (1:(length(.) / 2) - 1) * 2]
                    return(return_list)
                  }
                }()
            }
          )

        names(output_i)[names(output_i) == "id_in_table"]    <- "reihe_id"
        names(output_i)[names(output_i) == "table"]          <- "tabelle"
        names(output_i)[names(output_i) == "ranking_weight"] <- "ranking"
        return(output_i)
      }
    )

  if (beautify_results){
    results <-
      lapply(
        results,
        \(result){
          result$treffer <-
            lapply(
              result$ids,
              \(x) paste(sort(data_to_search_through$str[data_to_search_through$id %in% x]), collapse = " | ")
            ) |>
            unlist()
          result[
            order(result$ranking, decreasing = TRUE),
            c("suchbegriff", "tabelle", "reihe_id", "treffer", "ranking", "ranking_list")
          ]

        }
      )
  }

  if (length(results) == 1) results <- results[[1]]

  return(results)
}

search_prepared_string <- function(
    search_str,
    search_data,
    max_distance,
    min_ranking_weight,
    print_messages = to_print_log()
){
  if (print_messages) {
    message(paste0("Searching for '", search_str, "'."))
  }

  search_results <-
    search_unique_db_values(
      search_str   = search_str,
      search_data  = search_data,
      max_distance = max_distance,
      print_messages = print_messages
    )

  search_results$search_str         <- search_str
  search_results$search_str_prepped <- preprocess_str(search_results$search_str)

  search_results_ranked <- rank_search_results(result_df = search_results)
  search_results_ranked_filtered <-
    filter_by_ranking_weight(
      result_df = search_results_ranked,
      min_ranking_weight = min_ranking_weight,
      print_messages = print_messages
    )
  return(search_results_ranked_filtered)
}


search_unique_db_values <- function(
    search_str,
    search_data,
    max_distance = 2,
    print_messages = to_print_log()
) {

  # parse arguments
  search_str_prepped <- preprocess_str(search_str)
  max_distance       <- adapt_max_distance_to_search_str_len(max_distance, search_str_prepped)

  matches <-
    get_matches_from_search_data(
      search_data = search_data,
      search_str_prepped = search_str_prepped,
      max_distance = max_distance
    )

  if (print_messages){
    message(paste0("    Found ", nrow(matches), " fuzzy matches in unique values."))
  }

  return(matches)
}

get_matches_from_search_data <- function(search_data,
                                         search_str_prepped,
                                         max_distance) {

  match_1 <- get_logical_match_result(str_a = search_str_prepped,
                                      str_b = search_data$str_prepped,
                                      max_dist = max_distance)
  match_2 <-
    lapply(
      search_data$str_prepped,
      FUN = get_logical_match_result,
      str_b = search_str_prepped,
      max_dist = max_distance
    ) |>
    unlist()

  search_data_results <-
    search_data |>
    dplyr::filter(Reduce(`|`, list(match_1, match_2))) |>
    get_partial_edit_distances(search_str_prepped = search_str_prepped)

  # if there are matches, append each match to the result list
  matches <-
    search_data_results[
      search_data_results$ed_part <= max_distance &
        !is.na(search_data_results$str_prepped) &
        search_data_results$str_prepped != "",
    ]

  # find overall (not partial) edit distance of matches
  matches$ed_abs <-
    get_numeric_adist(str_a = search_str_prepped,
                      str_b = matches$str_prepped,
                      partial = FALSE)

  return(matches)
}


# --- GET_DATA -------------------------------------------------------------------------------------

#' @export
get_data_to_search <- function(tables = NA, columns = NA, con = NA, print_messages = to_print_log()){

  dont_need_con <- tryCatch(
    DBI::dbIsValid(con),
    error = function(cond) FALSE
  )

  possible_tables <-
    c(
      "tag", "variable", "wert_einheit", "zeit_einheit",
      "reichweite", "reichweite_typ", "reichweite_klasse",
      "quelle"
    )

  if (all(is.na(tables))){
    tables <- possible_tables
  } else {
    stopifnot(
      "Falsche Tabellen ausgewählt!" =
        all(tables %in% possible_tables)
    )
  }

  possible_columns <- c("bez_lang", "beschr", "beschr_lang", "descr")

  if (all(is.na(columns))){
    columns <- possible_columns
  } else {
    stopifnot(
      "Falsche Columns ausgewählt!" =
        all(columns %in% possible_columns)
    )
  }

  data_to_search <- data.frame()

  if (!dont_need_con){
    if (print_messages) message("  > Keine gültige Verbindung üebrgeben, Verbindung über svMagpie")
    con <- try(connect_magpie())
  }

  for (i in tables){

    new_data_to_search <-
      DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT id, %s FROM %s",
          paste(columns, collapse = ", "),
          i
        )
      )
    new_data_to_search$table <- i
    data_to_search <- rbind(data_to_search, new_data_to_search)
    rm(new_data_to_search)
  }

  if (!dont_need_con){
    try(DBI::dbDisconnect(con))
  }

  reorganised_data_to_search <- data.frame()
  data_to_search$column <- NA
  for (i in columns){
    new_reorganised_data_to_search <- data_to_search[,c("id", "table", "column", i)]
    names(new_reorganised_data_to_search)[c(1,4)] <- c("id_in_table", "str")
    new_reorganised_data_to_search <- new_reorganised_data_to_search[!is.na(new_reorganised_data_to_search$str),]
    new_reorganised_data_to_search$column <- i
    reorganised_data_to_search <- rbind(reorganised_data_to_search, new_reorganised_data_to_search)
    rm(new_reorganised_data_to_search)
  }

  reorganised_data_to_search$id <- 1:nrow(reorganised_data_to_search)
  reorganised_data_to_search <- reorganised_data_to_search[,c(ncol(reorganised_data_to_search), 1:(ncol(reorganised_data_to_search) - 1))]

  reorganised_data_to_search$str_prepped <- preprocess_str(reorganised_data_to_search$str)

  return(reorganised_data_to_search)
}

# --- RANK_SEARCH_RESULTS --------------------------------------------------------------------------

normalize_zero_to_one <- function(values, repole = FALSE) {
  max_value <- max(values)

  normalized_values <- (values / max_value)
  if (repole) {
    normalized_values <- 1 - normalized_values
  }
  return(normalized_values)
}

# all subfunctions returning a value between zero and one

# assign a low weight for search_strings of less or equal length
# than the max_distance used and a one for all strings
# which are longer than max_distance
calculate_search_str_len <- function(df, max_distance = 2) {
  search_str_len <- unlist(lapply(df$str_prepped, FUN = nchar))
  values <-
    ifelse(search_str_len <= max_distance,
           (search_str_len - 1) / max_distance,
           1)
  return(values)
}

# assign a low weight for db strings of less or equal length
# than the max_distance used and a one for all strings
# which are significantly
calculate_db_str_len <- function(df, max_distance = 2) {
  db_str_len <- unlist(lapply(df$str_prepped, FUN = nchar))
  values <-
    ifelse(db_str_len <= 2 * max_distance,
           (db_str_len - 1) / (max_distance * 2),
           1)
  return(values)
}

# assign a low weight for large differences in search and db string,
# make the weight penalty depend on overall length
# (short strings, larger penalty)
calculate_str_diff <-
  function(
    df,
    max_distance = 2,
    stretch_factor = 0.5
  ){
    db_str_len <- unlist(lapply(df$str_prepped, FUN = nchar))
    search_str_len <-
      unlist(lapply(df$search_str_prepped, FUN = nchar))
    diffs <- abs(db_str_len - search_str_len)
    values <-
      ifelse(diffs < (max_distance / stretch_factor),
             1 - stretch_factor * diffs,
             0)
    return(values)
  }

# give a high weight to low absolute edit distances,
# aka matching both full strings
calculate_ed_abs <- function(df) {
  values <- normalize_zero_to_one(df$ed_abs, repole = TRUE) |>
    tidyr::replace_na(0)
  return(values)
}

# give a high weight to low partial edit distances,
# aka perfect submatch
calculate_ed_part_db_in_search <- function(df) {
  values <- normalize_zero_to_one(df$ed_part_2, repole = TRUE) |>
    tidyr::replace_na(0)
  return(values)
}

# give a high weight to low partial edit distances,
# aka perfect submatch
calculate_ed_part_search_in_db <- function(df) {
  values <- normalize_zero_to_one(df$ed_part_1, repole = TRUE) |>
    tidyr::replace_na(0)
  return(values)
}

# give higher weights to more important tables
calculate_column_preference <- function(df) {
  column_preference <- c(
    "variable_beschr" = 1,
    "zeit_einheit" = 0.2,
    "wert_einheit" = 0.2,
    "reichweite_beschr_list" = 0.7,
    "reichweite_typ_list" = 0.5,
    "typ_list" = 0.5,
    "reichweite_klasse_list" = 0.6,
    "klasse_list" = 0.5,
    "menge_zugeh_reichweiten" = 0.2,
    "quelle_list" = 0.5,
    "tag_list" = 0.8
  )
  values <-
    ifelse(df$column %in% names(column_preference),
           column_preference[df$column],
           0)
  return(values)
}

calculate_data_points <- function(df) {
  values <-
    unlist(lapply(df$ids, length)) |>
    log() |>
    normalize_zero_to_one()
  return(values)
}

calculate_db_str_len_dist_ratio <- function(df) {
  lengths <- lapply(df$str_prepped, FUN = nchar) |>
    unlist()
  values <- (df$ed_part_2 / lengths) |>
    normalize_zero_to_one(repole = TRUE)
  return(values)
}

calculate_search_str_len_dist_ratio <- function(df) {
  lengths <- lapply(df$search_str_prepped, FUN = nchar) |>
    unlist()
  values <- (df$ed_part_1 / lengths) |>
    normalize_zero_to_one(repole = TRUE)
  return(values)
}

# current problem: max_distance is hardcoded here as 2,
# should be global value as well
calculate_search_word_is_db_word <- function(df) {
  worddists <-
    utils::adist(
      paste0("\\b", df$search_str_prepped[1], "\\b"),
      df$str_prepped,
      fixed = FALSE,
      ignore.case = TRUE
    ) |>
    as.numeric()
  worddists[worddists > 2] <- 3
  values <- normalize_zero_to_one(worddists, repole = TRUE)
  return(values)
}

calculate_single_ranking_scores <- function(result_df, with_data_points = TRUE) {
  ranking <- data.frame("result_id" = seq.int(nrow(result_df)))

  ranking$search_str_len <-
    calculate_search_str_len(df = result_df)
  ranking$db_str_len <- calculate_db_str_len(df = result_df)
  ranking$str_diff <- calculate_str_diff(df = result_df)
  ranking$ed_part_search_in_db <-
    calculate_ed_part_search_in_db(df = result_df)
  ranking$ed_part_db_in_search <-
    calculate_ed_part_db_in_search(df = result_df)
  ranking$ed_abs <- calculate_ed_abs(df = result_df)
  ranking$column_preference <-
    calculate_column_preference(df = result_df)
  ranking$search_str_len_dist_ratio <-
    calculate_search_str_len_dist_ratio(df = result_df)
  ranking$db_str_len_dist_ratio <-
    calculate_db_str_len_dist_ratio(df = result_df)
  ranking$search_word_is_db_word <-
    calculate_search_word_is_db_word(df = result_df)

  return(ranking)
}


filter_by_ranking_weight <-
  function(result_df, min_ranking_weight = 0.5, print_messages = to_print_log()) {
    old_len <- nrow(result_df)
    result_df <-
      result_df[result_df$ranking_weight >= min_ranking_weight, ]
    new_len <- nrow(result_df)
    if(print_messages){
      message(
        paste0(
          "    Dropped ",
          old_len - new_len,
          " results due to low ranking score, now there are ",
          new_len,
          " results."
        )
      )
    }
    return (result_df)
  }


rank_search_results <-
  function(result_df,
           factor_weights = c(
             "search_str_len" = 2,
             "db_str_len" = 4,
             "str_diff" = 4,
             "ed_part_db_in_search" = 4,
             "ed_part_search_in_db" = 8,
             "ed_abs" = 4,
             "column_preference" = 4,
             "search_str_len_dist_ratio" = 4,
             "db_str_len_dist_ratio" = 2,
             "search_word_is_db_word" = 6
           )) {

    ranking <- calculate_single_ranking_scores(result_df = result_df)
    result_df$ranking_weight <- 0
    sum_weights <- sum(factor_weights)

    for (factor in names(factor_weights)) {
      weight <- factor_weights[[factor]] / sum_weights
      result_df$ranking_weight <-
        (result_df$ranking_weight + (ranking[[factor]] * weight))
    }

    result_df <- result_df |>
      dplyr::arrange(-ranking_weight) |>
      dplyr::mutate(rank = dplyr::row_number())

    return(result_df)
  }


# --- SEARCH_CONVERTED_DATA ------------------------------------------------------------------------

get_logical_match_result <- function(str_a, str_b, max_dist) {

  contains_match <-
    agrepl(
      str_a,
      str_b,
      max.distance = max_dist,
      costs =
        list(
          "insertions" = 1,
          "deletions" = 1,
          "substitutions" = 1
        ),
      ignore.case = TRUE,
      fixed = TRUE
    )

  return(contains_match)
}

get_numeric_adist <- function(str_a, str_b, partial = TRUE) {
  adists <-
    utils::adist(
      str_a,
      str_b,
      fixed = TRUE,
      partial = partial,
      ignore.case = TRUE
    ) |>
    as.numeric()
  return (adists)
}

get_matches_from_table_column <- function(table_column, search_str_prepped, max_distance) {

  match_1 <-
    get_logical_match_result(
      str_a = search_str_prepped,
      str_b = table_column$str_prepped,
      max_dist = max_distance
    )

  match_2 <-
    lapply(
      table_column$str_prepped,
      FUN      = get_logical_match_result,
      str_b    = search_str_prepped,
      max_dist = max_distance
    ) |>
    unlist()

  table_column <-
    table_column |>
    Reduce(`|`, list(match_1, match_2)) |>
    filter() |>
    get_partial_edit_distances(search_str_prepped = search_str_prepped)

  # if there are matches, append each match to the result list
  matches <-
    table_column[
      table_column$ed_part <= max_distance &
        !is.na(table_column$str_prepped) &
        table_column$str_prepped != "",
    ]

  # find overall (not partial) edit distance of matches
  matches$ed_abs <-
    get_numeric_adist(
      str_a = search_str_prepped,
      str_b = matches$str_prepped,
      partial = FALSE
    )

  return(matches)
}

get_partial_edit_distances <- function(column_data, search_str_prepped) {

  # get partial overlap
  column_data$ed_part_1 <-
    get_numeric_adist(str_a = search_str_prepped,
                      str_b = column_data$str_prepped)

  column_data$ed_part_2 <-
    get_numeric_adist(str_a = column_data$str_prepped,
                      str_b = search_str_prepped)

  # add minimum of both submatches
  column_data$ed_part <-
    pmin(column_data$ed_part_1, column_data$ed_part_2)
  column_data[is.na(column_data$ed_part), c("ed_part")] <-
    Inf

  return (column_data)
}

adapt_max_distance_to_search_str_len <- function(max_distance, search_str_prepped, print_messages = to_print_log()) {

  if (nchar(search_str_prepped) <= max_distance) {
    max_distance <- nchar(search_str_prepped)
    if (print_messages){
      message(
        paste0(
          "max_distance was reduced because search string ",
          search_str_prepped,
          " was too short"
        )
      )
    }
  }
  return(max_distance)
}

# --- UTILS ----------------------------------------------------------------------------------------

preprocess_str <- function(str) {
  str <- tolower(str) |>
    stringr::str_replace_all("[:]+", " ") |>
    stringr::str_replace_all("[^ 0-9a-zäöüß&|]", "") |>
    stringr::str_replace_all("ä", "ae") |>
    stringr::str_replace_all("ö", "oe") |>
    stringr::str_replace_all("ü", "ue") |>
    stringr::str_replace_all("ß", "ss") |>
    stringr::str_squish()
  return(str)
}

split_into_search_terms <- function(search_term) {
  search_terms <- stringr::str_split_1(search_term, "&") |>
    lapply(FUN = stringr::str_trim) |>
    unlist()
}

extract_operators <- function(search_term) {
  operators <- stringr::str_extract_all(search_term, "[\\&]") |>
    unlist()
  operators <- c("", operators)
  return (operators)
}

to_print_log <- function() {
  print_log <- Sys.getenv("PRINT_LOG")
  if (print_log %in% "FALSE") return(FALSE)
  return(TRUE)
}
