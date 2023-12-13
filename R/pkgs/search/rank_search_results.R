
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
calculate_str_diff <- function(df, max_distance = 2) {
  db_str_len <- unlist(lapply(df$str_prepped, FUN = nchar))
  search_str_len <-
    unlist(lapply(df$search_str_prepped, FUN = nchar))
  diffs <- abs(db_str_len - search_str_len)
  values <- ifelse(diffs < max_distance, 1 - diffs, 0)
  return(values)
}

# give a high weight to low absolute edit distances,
# aka matching both full strings
calculate_ed_abs <- function(df) {
  ed_max <- max(df$ed_abs)
  values <- 1 - (df$ed_abs / ed_max)
  values <- replace_na(values, 0)
  return(values)
}

# give a high weight to low partial edit distances,
# aka perfect submatch
calculate_ed_part <- function(df) {
  ed_max <- max(df$ed_part)
  values <- 1 - (df$ed_part / ed_max)
  values <- replace_na(values, 0)
  return(values)
}

# give higher weights to more important tables
calculate_table_preference <- function(df) {
  table_preference <- c(
    "quelle" = 0.2,
    "reichweite" = 0.5,
    "reichweite_klasse" = 0.2,
    "reichweite_typ" = 0.2,
    "tag" = 1,
    "variable" = 1,
    "wert_einheit" = 0.2
  )
  values <-
    ifelse(df$table %in% names(table_preference),
           table_preference[df$table],
           0)
  return(values)
}

# give higher weight to results with multiple matches in a row
calculate_matchcount_per_id <- function(df) {
  values <- df %>%
    group_by(table, id) %>%
    mutate(counts_per_id = n(), .groups = 'keep') %>%
    ungroup() %>%
    select(counts_per_id) %>%
    unlist() %>%
    as.numeric()
  values = ifelse(values > 1, 1, 0)
  return(values)
}

# give higher weight to results that match more data points
calculate_data_points <- function(df) {
  # not implemented yet
  conn <- get_valid_conn(NA)
  return(1)
}

filter_by_ranking_weight <- function(df, min_ranking_weight = 0.5) {
  old_len <- nrow(df)
  df <- df[df$ranking_weight >= min_ranking_weight, ]
  new_len <- nrow(df)
  message(paste0(
    "Dropped ",
    old_len - new_len,
    " results due to low ranking score, now there are ",
    new_len,
    " results."
  ))
  return (df)
}


rank_search_results <-
  function(df,
           factor_weights = c(
             "search_str_len" = 1,
             "db_str_len" = 1,
             "str_diff" = 1,
             "ed_part" = 1,
             "ed_abs" = 1,
             "table_preference" = 1,
             "matchcount_per_id" = 1#,
             #"data_points" = 1
           )) {
    ranking <- data.frame("result_id" = seq.int(nrow(df)))
    
    ranking$search_str_len <- calculate_search_str_len(df = df)
    ranking$db_str_len <- calculate_db_str_len(df = df)
    ranking$str_diff <- calculate_str_diff(df = df)
    ranking$ed_part <- calculate_ed_part(df = df)
    ranking$ed_abs <- calculate_ed_abs(df = df)
    ranking$table_preference <- calculate_table_preference(df = df)
    ranking$matchcount_per_id <-
      calculate_matchcount_per_id(df = df)
    ranking$data_points <- calculate_data_points(df = df)
    df$ranking_weight <- 0
    sum_weights <- sum(factor_weights)
    
    for (factor in names(factor_weights)) {
      weight <- factor_weights[[factor]]  / sum_weights
      df$ranking_weight <-
        (df$ranking_weight + (ranking[[factor]] * weight))
    }
    
    df <- dplyr::arrange(df, -ranking_weight) %>%
      mutate(rank = 1:nrow(.))
    
    df <-
      df[c("id",
           "table",
           "column",
           "db_content",
           "ranking_weight",
           "rank")]
    return(df)
  }

