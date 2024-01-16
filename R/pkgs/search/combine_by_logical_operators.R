
box::use(
  dplyr = dplyr[filter, select, mutate, rowwise, ungroup, row_number]
)

print_messages <- FALSE

merge_by_and_operator <- function(current_results, new_results) {
  if (!is.data.frame(current_results)) {
    results <- new_results
  }
  else {
    results <-
      merge(current_results,
            new_results,
            by = c("column", "str_prepped", "db_content", "ids")) |>
      dplyr$rowwise() |>
      dplyr$mutate(
        ed_part = list(c(
          unlist(ed_part.x), unlist(ed_part.y)
        )),
        ed_abs = list(c(unlist(ed_abs.x), unlist(ed_abs.y))),
        search_str = list(c(
          unlist(search_str.x), unlist(search_str.y)
        )),
        search_str_prepped = list(c(
          unlist
          (search_str_prepped.x),
          unlist(search_str_prepped.y)
        )),
        ranking_weight = list(c(
          unlist(ranking_weight.x), unlist(ranking_weight.y)
        ))
      ) |>
      dplyr$ungroup() |>
      dplyr$select(
        c(
          "db_content",
          "column",
          "str_prepped",
          "ed_part",
          "ed_abs",
          "ids",
          "search_str",
          "search_str_prepped",
          "ranking_weight"
        )
      )

  }
  return (results)
}

aggregate_or_values <- function(x, y) {
  if (is.na(x) & !is.na(y)) {
    return(y)
  }
  else if (!is.na(x) & is.na(y)) {
    return(x)
  }
  else if (!is.na(x) & !is.na(y)) {
    return(list(c(x, y)))
  }
  else {
    return(NA)
  }
}

merge_by_or_operator <- function(current_results, new_results) {
  if (!is.data.frame(current_results)) {
    results <- new_results
  }
  else {
    results <-
      merge(
        current_results,
        new_results,
        all = TRUE,
        by = c("column", "str_prepped", "db_content", "ids")
      ) |>
      dplyr$rowwise() |>
      dplyr$mutate(
        ed_part = aggregate_or_values(ed_part.x, ed_part.y),
        ed_abs = aggregate_or_values(ed_abs.x, ed_abs.y),
        search_str = aggregate_or_values(search_str.x, search_str.y),
        search_str_prepped = aggregate_or_values(search_str_prepped.x, search_str_prepped.y),
        ranking_weight = aggregate_or_values(ranking_weight.x, ranking_weight.y)
      ) |>
      dplyr$ungroup() |>
      dplyr$select(
        c(
          "db_content",
          "column",
          "str_prepped",
          "ed_part",
          "ed_abs",
          "ids",
          "search_str",
          "search_str_prepped",
          "ranking_weight"
        )
      )

  }
  return (results)
}

#' @export
combine_by_logical_operators <-
  function(new_results,
           current_results = NA,
           operator = "") {
    if (operator == "&") {
      combined_results <-
        merge_by_and_operator(current_results = current_results,
                              new_results = new_results)
    }
    else if (operator == "|") {
      combined_results <-
        merge_by_or_operator(current_results = current_results,
                             new_results = new_results)
    }
    else {
      combined_results <- new_results
    }
    return(combined_results)
  }

#' @export
aggregate_ranking <- function(final_results) {
  final_results <- final_results |>
    rowwise() |>
    dplyr$mutate(ranking_weight = mean(unlist(ranking_weight))) |>
    ungroup() |>
    dplyr$arrange(-ranking_weight) |>
    dplyr$mutate(rank = dplyr$row_number())
  if (print_messages){
    message(
      paste0(
        "Aggregation of all logical operators results in ",
        nrow(final_results),
        " final result rows!"
      )
    )
  }

  return(final_results)
}
