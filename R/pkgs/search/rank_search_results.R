
box::use(
  dplyr = dplyr[
    filter,
    select,
    mutate,
    arrange,
    group_indices,
    group_by,
    n,
    row_number
  ],
  tidyr = tidyr[replace_na]
)

print_messages <- FALSE

normalize_zero_to_one <- function(values) {
  max_value <- max(values)
  normalized_values <- 1 - (values / max_value)
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
  function(df,
           max_distance = 2,
           stretch_factor = 0.5) {
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
  values <- normalize_zero_to_one(df$ed_abs) |>
    tidyr$replace_na(0)
  return(values)
}

# give a high weight to low partial edit distances,
# aka perfect submatch
calculate_ed_part <- function(df) {
  values <- normalize_zero_to_one(df$ed_part) |>
    tidyr$replace_na(0)
  return(values)
}

# give higher weights to more important tables
calculate_column_preference <- function(df) {
  column_preference <- c(
    "variable_beschr" = 1,
    "zeit_einheit" = 0.2,
    "wert_einheit" = 0.5,
    "reichweite_beschr_list" = 0.8,
    "reichweite_typ_list" = 0.6,
    "typ_list" = 0.5,
    "reichweite_klasse_list" = 0.6,
    "klasse_list" = 0.5,
    "menge_zugeh_reichweiten" = 0.7,
    "quelle_list" = 0.5,
    "tag_list" = 1
  )
  values <-
    ifelse(df$column %in% names(column_preference),
           column_preference[df$column],
           0)
  return(values)
}

# give higher weights to more important tables
calculate_column_preference <- function(df) {
  column_preference <- c(
    "variable_beschr" = 1,
    "zeit_einheit" = 0.2,
    "wert_einheit" = 0.5,
    "reichweite_beschr_list" = 0.8,
    "reichweite_typ_list" = 0.6,
    "typ_list" = 0.5,
    "reichweite_klasse_list" = 0.6,
    "klasse_list" = 0.5,
    "menge_zugeh_reichweiten" = 0.7,
    "quelle_list" = 0.5,
    "tag_list" = 1
  )
  values <-
    ifelse(df$column %in% names(column_preference),
           column_preference[df$column],
           0)
  return(values)
}

calculate_data_points <- function(df) {
  values <- unlist(lapply(df$ids, length)) |>
    log() |>
    normalize_zero_to_one()
  return(values)
}

calculate_single_ranking_scores <- function(result_df) {
  ranking <- data.frame("result_id" = seq.int(nrow(result_df)))

  ranking$search_str_len <-
    calculate_search_str_len(df = result_df)
  ranking$db_str_len <- calculate_db_str_len(df = result_df)
  ranking$str_diff <- calculate_str_diff(df = result_df)
  ranking$ed_part <- calculate_ed_part(df = result_df)
  ranking$ed_abs <- calculate_ed_abs(df = result_df)
  ranking$column_preference <-
    calculate_column_preference(df = result_df)
  ranking$data_points <-
    calculate_data_points(df = result_df)

  return(ranking)
}

#' @export
filter_by_ranking_weight <-
  function(result_df, min_ranking_weight = 0.5) {
    old_len <- nrow(result_df)
    result_df <-
      result_df[result_df$ranking_weight >= min_ranking_weight,]
    new_len <- nrow(result_df)
    if (print_messages){
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

#' @export
rank_search_results <-
  function(result_df,
           factor_weights = c(
             "search_str_len" = 1,
             "db_str_len" = 1,
             "str_diff" = 1,
             "ed_part" = 4,
             "ed_abs" = 2,
             "column_preference" = 1,
             "data_points" = 0.5

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
      dplyr$arrange(-ranking_weight) |>
      dplyr$mutate(rank = dplyr$row_number())

    return(result_df)
  }
