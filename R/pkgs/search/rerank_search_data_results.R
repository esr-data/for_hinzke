



rerank_search_data_results <-
  function(result_data_list, search_results) {
    all_ids <- c()
    
    for (i in 1:length(result_data_list)) {
      ids <- sort(result_data_list[[i]][, "id"])
      str_ids <- paste(ids, collapse = "|")
      all_ids <- c(all_ids, str_ids)
      
    }
    
    
    search_results_grouped <- search_results %>%
      mutate(ids=all_ids) %>%
      group_by(ids) %>%
      summarize(
      table=list(table),          
      column=list(column),
      db_content=list(db_content),
      ranking_weight=list(ranking_weight),
      rank=list(rank))
      
    counts <- data.frame(ids = all_ids,
                         ranking_weight = search_results$ranking_weight) %>%
      filter(ids != "") %>%
      group_by(ids) %>%
      summarise(new_ranking_weight = sum(ranking_weight)) %>%
      select(c("ids", "new_ranking_weight")) %>%
      filter(!duplicated(.))
    
    counts <- counts[order(-counts$new_ranking_weight),]
    
    new_result_data_list <- list()
    if (nrow(counts) > 0) {
      for (i in 1:nrow(counts)) {
        not_found <- TRUE
        j <- 1
        str_id <- counts$ids[i]
        while (not_found) {
          if (paste(sort(result_data_list[[j]][, "id"], descending = TRUE), collapse =
                    "|") == str_id) {
            new_result_data_list[[i]] <- result_data_list[[j]]
            not_found <- FALSE
          }
          else {
            j <- j + 1
          }
        }
      }
    }
    
    message(paste0(
      "Filtered from ",
      length(result_data_list),
      " to ",
      length(new_result_data_list),
      " unique and non-null results."
    ))
    return(list(new_result_data_list, search_results_grouped))
  }
