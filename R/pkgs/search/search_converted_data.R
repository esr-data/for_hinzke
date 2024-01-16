
box::use(
  dplyr = dplyr[filter, select, mutate, group_by_at, summarise, vars],
  utils = utils[adist],
  ms_string = . / string_prep[preprocess_str]
)

print_messages <- FALSE

get_logical_match_result <-
  function(str_a, str_b, max_dist) {
    contains_match <- agrepl(
      str_a,
      str_b,
      max.distance = max_dist,
      costs = list(
        "insertions" = 1,
        "deletions" = 1,
        "substitutions" = 1
      ),
      ignore.case = TRUE,
      fixed = TRUE
    )
    return(contains_match)
  }

get_numeric_adist <-
  function(str_a, str_b, partial = TRUE) {
    adists <- as.numeric(utils$adist(
      str_a,
      str_b,
      fixed = TRUE,
      partial = partial,
      ignore.case = TRUE
    ))
    return (adists)
  }


get_matches_from_table_column <- function(table_column,
                                          search_str_prepped,
                                          max_distance) {
  match_1 <- get_logical_match_result(str_a = search_str_prepped,
                                      str_b = table_column$str_prepped,
                                      max_dist = max_distance)
  match_2 <- unlist(
    lapply(
      table_column$str_prepped,
      FUN = get_logical_match_result,
      str_b = search_str_prepped,
      max_dist = max_distance
    )
  )
  table_column <- table_column |>
    filter(Reduce(`|`, list(match_1, match_2))) |>
    get_partial_edit_distances(search_str_prepped = search_str_prepped)

  # if there are matches, append each match to the result list
  matches <-
    table_column[table_column$ed_part <= max_distance &
                   !is.na(table_column$str_prepped) &
                   table_column$str_prepped != "",]

  # find overall (not partial) edit distance of matches
  matches$ed_abs <-
    get_numeric_adist(str_a = search_str_prepped,
                      str_b = matches$str_prepped,
                      partial = FALSE)

  return(matches)
}


get_partial_edit_distances <-
  function(column_data, search_str_prepped) {
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

adapt_max_distance_to_search_str_len <-
  function(max_distance, search_str_prepped) {
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


#' @export
search_unique_db_values <- function(search_str,
                                    data_long,
                                    data_unique,
                                    max_distance = 2) {
  # parse arguments
  search_str_prepped <- ms_string$preprocess_str(search_str)

  max_distance <-
    adapt_max_distance_to_search_str_len(max_distance, search_str_prepped)

  matches <-
    get_matches_from_table_column(
      table_column = data_unique,
      search_str_prepped = search_str_prepped,
      max_distance = max_distance
    )

  if (print_messages){
    message(paste0("    Found ", nrow(matches), " fuzzy matches in unique values."))
  }

  results <-
    merge(data_long, matches, how = "right", on = "db_content") |>
    dplyr$group_by_at(dplyr$vars(-id)) |>
    dplyr$summarise(ids = list(sort(unique(id))), .groups = "keep")
  results$search_str <- search_str
  results$search_str_prepped <- search_str_prepped

  if (print_messages){
    message(paste0("    Final result match data has ", nrow(results), " rows."))
  }

  return(results)
}
