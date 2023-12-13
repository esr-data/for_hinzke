# TODO: Keep connection alive to select data


retrieve_daten_ids_by_quelle <- function(id) {
  sql_part <- paste0("SELECT dq.daten_id FROM home.daten_quelle dq ",
                     "WHERE dq.quelle_id = ",
                     id)
  return(sql_part)
}

retrieve_daten_ids_by_reichweite <- function(id) {
  sql_part <-
    paste0("SELECT dr.daten_id FROM home.daten_reichweite dr ",
           "WHERE dr.reichweite_id = ",
           id)
  return(sql_part)
}

retrieve_daten_ids_by_reichweite_typ <- function(id) {
  sql_part <- paste0(
    "SELECT dr.daten_id FROM home.daten_reichweite dr ",
    "WHERE dr.reichweite_id IN ",
    "(SELECT r.id FROM home.reichweite r ",
    "WHERE r.reichweite_typ_id =",
    id,
    ")"
  )
  return(sql_part)
}

retrieve_daten_ids_by_reichweite_klasse <- function(id) {
  sql_part <- paste0(
    "SELECT dr.daten_id FROM home.daten_reichweite dr",
    " WHERE dr.reichweite_id IN ",
    "(SELECT r.id FROM home.reichweite r ",
    "WHERE r.reichweite_typ_id IN (",
    "SELECT rt.id from home.reichweite_typ rt ",
    "WHERE rt.reichweite_klasse_id = ",
    id,
    "))"
  )
  return(sql_part)
}

retrieve_daten_ids_by_tag <- function(id) {
  # TODO currently just looking for tagged data, not what we want (?)
  sql_part <- paste0(
    "SELECT id FROM mview_daten_id vt WHERE ", 
    id, 
    "=ANY(vt.tag)"
  )
  return(sql_part)
}

retrieve_daten_ids_by_variable <- function(id) {
  sql_part <-
    paste0("SELECT d.id FROM daten d WHERE d.variable_id = ",
           id)
  return(sql_part)
}

retrieve_daten_ids_by_wert_einheit <- function(id) {
  sql_part <-
    paste0("SELECT d.id FROM daten d WHERE d.wert_einheit_id = ",
           id)
  return(sql_part)
}

#-------------------------------------------------------------------------------

get_daten_id_retrieval_function <- function(table_name) {
  table_to_retrieval_function = c(
    "quelle" = retrieve_daten_ids_by_quelle,
    "reichweite" = retrieve_daten_ids_by_reichweite,
    "reichweite_klasse" = retrieve_daten_ids_by_reichweite_klasse,
    "reichweite_typ" = retrieve_daten_ids_by_reichweite_typ,
    "tag" = retrieve_daten_ids_by_tag,
    "variable" = retrieve_daten_ids_by_variable,
    "wert_einheit" = retrieve_daten_ids_by_wert_einheit
  )
  
  return(table_to_retrieval_function[[table_name]])
  
}


retreive_data_view_from_db <- function(table_name, id, conn, limit_results_to) {
  if (!is.na(limit_results_to)) {
    add_limit_to_sql <- paste0(" LIMIT ", limit_results_to)
  }
  else {
    add_limit_to_sql <- ""
  }
  
  conn <- get_valid_conn(conn)
  
  daten_id_retrieval_function <-
    get_daten_id_retrieval_function(table_name)
  
  daten_id_retrieval_sql <- daten_id_retrieval_function(id = id)
  
  sql <- paste0("SELECT * FROM home.mview_daten_id vd WHERE vd.id IN (",
                daten_id_retrieval_sql,
                ")",
                add_limit_to_sql,
                ";")
  
  daten <- svMagpie::get_query(sql, con = conn)
  
  return(daten)
}


retrieve_daten <- function(search_results, conn, limit_results_to=NA) {
  results <- list()
  if (length(search_results) > 0) {
    for (i in 1:nrow(search_results)) {
      search_result <- unlist(search_results[i,])
      data <-
        retreive_data_view_from_db(search_result[["table"]],
                                   search_result[["id"]],
                                   conn = conn,
                                   limit_results_to=limit_results_to)
      results[[i]] <- data
    }
  }
  
  return(results)
}
