get_results <- function(search_term,
                        min_ranking_weight = 0.7,
                        conn = NA,
                        table = NA,
                        max_distance = 3,
                        limit_results_to = NA) {
  all_search_results <- search_magpie(
    search_term,
    conn = conn,
    table = table,
    max_distance = max_distance
  )

  all_search_results_ranked <-
    rank_search_results(df = all_search_results)
  search_results <-
    filter_by_ranking_weight(df = all_search_results_ranked, min_ranking_weight =
                               min_ranking_weight)
  result_data <-
    retrieve_daten(search_results,
                   conn = conn,
                   limit_results_to = limit_results_to)
  final_results <-
    rerank_search_data_results(result_data_list = result_data, search_results =
                                 search_results)
  return(final_results)
}
