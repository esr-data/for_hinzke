
#-------------------------------------------------------------------------------
box::purge_cache()

box::use(
  ms_search = ./get_search_results[get_results],
  ms_data    = . / convert_magpie_data[get_valid_conn, get_db_data_long],
)
conn <- ms_data$get_valid_conn(NA)
cached_long_db_data <- ms_data$get_db_data_long(conn = conn)

results <-
  ms_search$get_results("daten | forschung & entwicklung",
              db_values_long = cached_long_db_data,
              conn = conn)

results
