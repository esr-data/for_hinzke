
box::use(
  stringr[str_count]
)


get_possible_input <- function(
    filter_typ,
    reichweite_typ_in = NULL,
    reichweite_in = NULL,
    possible_input,
    group = NULL,
    filter = NULL,
    filter_combis = NULL,
    con = NULL,
    silent = FALSE,
    skip = skip
){
  
  ## --- group -----------------------------------------------------------------
  
  if (!is.null(group)){
    
    group <- reichweite_typ_in # noch aufzuräumen: direkt alles in Abhängigkeit von reichweite_typ_in? Ja!
    
    possible_reichweiten_typ <- possible_input$possible_kombis_reichweite_typen$typ_list
    
    if (length(group) == 1){
      
      possible_reichweiten_typ <- possible_reichweiten_typ[grepl(group, possible_reichweiten_typ)]
      
      result_number_of_groups <-
        min(str_count(possible_reichweiten_typ, "\\|")) + 1
      
      if (result_number_of_groups == length(group)){
        
        reichweite_typ_in <- group
        
      } else {
        
        min_reichweiten_typ_in <-
          possible_reichweiten_typ[str_count(possible_reichweiten_typ, "\\|") == result_number_of_groups - 1]
        
        i <- ifelse(length(grep("Land",  min_reichweiten_typ_in)) > 0,
                    grep("Land",  min_reichweiten_typ_in),
                    1)
        
        reichweite_typ_in <- min_reichweiten_typ_in[i]
        
        if (!silent){
          message(
            "The selected group category exists only in combination with other categories. The group combination '",
            paste0(reichweite_typ_in, "' is selected.")
          )
          
        }
        
      }
      
    } else {
      
      help_df <-
        data.frame(
          matrix(
            nrow = length(possible_reichweiten_typ),
            ncol = length(group) + 1
          )
        )
      
      for (i in 1:length(group)){
        
        help_df[, i] <- grepl(group[i], possible_reichweiten_typ)
        
      }
      
      help_df[, length(group) + 1] <- rowMeans(help_df[, 1:length(group) ])
      
      possible_reichweiten_typ <- possible_reichweiten_typ[help_df[, length(group) + 1] == 1]
      
      
      if (max(help_df[, length(group) + 1]) < 1){
        
        if (!silent){
          message("Selected group combination is not available for this plot. Please select another category/combination from options above.")
        }
        
        reichweite_typ_in <- NULL
        
      } else {
        
        result_number_of_groups <-
          min(str_count(possible_reichweiten_typ, "\\|")) + 1
        
        reichweite_typ_in <-
          possible_reichweiten_typ[which.min(str_count(possible_reichweiten_typ, "\\|"))]
        
        if (result_number_of_groups > length(group)){
          if (!silent){
            message(
              "The selected group category exists only in combination with other categories. The group combination '",
              paste0(reichweite_typ_in, "' is selected.")
            )
          }
          
        } else {
          if (!silent){
            
            message(
              "The selected group '",
              paste0(reichweite_typ_in, "' can be applied.")
            )
          }
        }
      }
    }
    
  }
  
  ## --- filter ----------------------------------------------------------------
  else if (is.null(group) & !is.null(filter)){
    
    possible_reichweite <- possible_input$possible_kombis_reichweite$reichweite_beschr_list
    
    ### --- 1 filter -----------------------------------------------------------
    
    if (length(filter) == 1){
      possible_reichweite <- possible_reichweite[grepl(filter, possible_reichweite)]
      
      result_number_of_filters <-
        min(str_count(possible_reichweite, "\\|")) + 1
      
      if (result_number_of_filters == length(filter)){
        
        reichweite_in <- filter
        
      } else {
        
        min_reichweite_in <-
          possible_reichweite[str_count(possible_reichweite, "\\|") == result_number_of_filters - 1]
        
        i <- ifelse(length(grep("Deutschland",  min_reichweite_in)) > 0,
                    grep("Deutschland",  min_reichweite_in),
                    1)
        
        reichweite_in <- min_reichweite_in[i]
        
        if (!silent){
          message(
            "The selected filter category exists only in combination with other categories. The filter combination '",
            paste0(reichweite_in, "' is selected.")
          )
        }
      }
    } else if (length(filter) > 1 & length(unique(filter_typ)) == 1){
      ### --- several filters, exactly 1 rwt/group -------------------------------
      help_df <-
        data.frame(
          matrix(
            nrow = length(possible_reichweite),
            ncol = length(filter) + 1
          )
        )
      
      for (j in 1:length(filter)){
        #print(j)
        help_df[1:length(possible_reichweite), j] <- grepl(filter[j], possible_reichweite)
        
      }
      
      help_df[, length(filter) + 1] <- rowMeans(help_df[, 1:length(filter) ])
      
      possible_reichweite_temp <- possible_reichweite[help_df[, length(filter) + 1] == 1/length(filter)]
      
      reichweite_in <-
        possible_reichweite_temp[str_count(possible_reichweite_temp, "\\|") == min(str_count(possible_reichweite_temp, "\\|"))]
      
      #message(paste0("das ist reichweite_in:", paste(reichweite_in, collapse = ", ")))
      
    } else {
      
      ### --- several filters, several groups (equal or unqueal length) ----------
      
      #message("********************* RICHTIG ABGEBOGEN ****************************")
      
      help_df <-
        data.frame(
          matrix(
            nrow = length(possible_reichweite),
            ncol = length(filter) + 1
          )
        )
      
      for (j in 1:length(filter)){
        
        help_df[, j] <- grepl(filter[j], possible_reichweite)
        
      }
      
      help_df[, length(filter) + 1] <- rowMeans(help_df[, 1:length(filter) ])
      
      max_uebereinstimmung <- max(help_df[, length(filter) + 1])
      
      possible_reichweite_temp <- possible_reichweite[help_df[, length(filter) + 1] == max_uebereinstimmung]
      
      reichweite_in <-
        possible_reichweite_temp[str_count(possible_reichweite_temp, "\\|") == min(str_count(possible_reichweite_temp, "\\|"))]
      
      if(!silent){
        message(paste0(
          "The selected filters are: '",
          paste(reichweite_in, collapse = "', '"), "'."
        ))
      }
      
    }
    
    
  }
  
  if (length(c(reichweite_typ_in, reichweite_in)) > 0) {
    
    return(list(reichweite_typ_in = reichweite_typ_in, reichweite_in = reichweite_in))
    
  } else {
    
    if(!silent){
      message("Ups. Irgendwas ging bei der Erstellung von 'input_list' daneben! Es wurde weder ein möglciher Reichweite-Typ, noch eine passende Reichweite auf Basis der Angaben gefunden :-( ...")
    }
    
  }
  
}
