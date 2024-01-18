
box::use(
  stringr    = stringr[str_split_1, str_trim],
  ms_string  = . / utils[extract_operators],
  ms_data    = . / convert_magpie_data[get_column_vector, get_db_data_unique],
  ms_search  = . / search_converted_data[search_unique_db_values],
  ms_rank    = . / rank_search_results[rank_search_results, filter_by_ranking_weight],
  ms_combine = . / combine_by_logical_operators[combine_by_logical_operators, aggregate_ranking],
  dplyr[filter]
)

print_messages <- FALSE

# Get all DB matches, their origin and their ranking given a search term and converted magpie data 
#' @export
get_results <- function(search_term,
                        db_values_long,
                        min_ranking_weight = 0.65,
                        conn = NA,
                        table = NA,
                        max_distance = 2,
                        limit_results_to = NA) {
  search_strs <-
    unlist(lapply(stringr$str_split_1(search_term, "\\&|\\|"), FUN = stringr$str_trim))

  operators <-
    ms_string$extract_operators(search_term = search_term)

  conn <- ms_data$get_valid_conn(conn)
  columns_to_search_in <-
    ms_data$get_column_vector(table, conn = conn)

  db_values_long <- db_values_long |>
    dplyr::filter(db_values_long$column %in% columns_to_search_in)
  db_values_unique <-
    ms_data$get_db_data_unique(data_long = db_values_long)

  final_results <- NA

  for (i in seq_along(search_strs)) {
    search_term <- search_strs[i]
    if (print_messages) message(paste0("Searching for '", search_term, "'."))
    search_results <- ms_search$search_unique_db_values(
      search_term,
      data_long = db_values_long,
      data_unique = db_values_unique,
      max_distance = max_distance
    )

    search_results_ranked <-
      ms_rank$rank_search_results(result_df = search_results)
    search_results_ranked_filtered <-
      ms_rank$filter_by_ranking_weight(result_df = search_results_ranked,
                                       min_ranking_weight = min_ranking_weight)

    final_results <-
      ms_combine$combine_by_logical_operators(
        new_results = search_results_ranked_filtered,
        current_results = final_results,
        operator = operators[i]
      )
  }
  final_results <-
    ms_combine$aggregate_ranking(final_results = final_results)

  return(final_results)
}
