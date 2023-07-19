# Some trials have multiple country protocols, so we first split the protocol
# into country-specific information (instance)

extract_summaries <- function(lines) {
  
  indices <- c()
  
  number <- 0
  for (line in lines) {
    number <- number + 1
    m <- str_match(line, "^Summary$")
    if (!is.na(m)) {
      indices <- append(indices, number)
    }
  }
  return(indices)
}


# Extract full title
# TODO: get title by other means if not available in the protocol
extract_title <- function(lines) {
  
  result <- NA
  
  for (line in lines) {
    
    if (is.na(result)) {
      
      m <- str_match(line, "A\\.3 Full title of the trial:(.+)")
      
      if (!is.na(m[2])) {
        result <- str_squish(m[2])
      }
    } else if (line == "" || startsWith(line, "A.") || startsWith(line, "B.")) {
      break
    } else {
      result <- paste(result, str_squish(line))
    }
  }
  
  return(result)
}


# Extract member state concerned
extract_member_state <- function(lines) {
  
  for (line in lines) {
    m <- str_match(line,"A\\.1 Member State Concerned:(.+)")
    
    if(!is.na(m[2])) {
      member_state <- str_squish(m[2])
      return(member_state)
    }
    
  }
  
  return(NA)
}


# Extract trial status
extract_trial_status <- function(lines) {
  
  for (line in lines) {
    m <- str_match(line,"P\\. End of Trial Status:(.+)")
    
    if(!is.na(m[2])) {
      trial_status <- str_squish(m[2])
      return(trial_status)
    }
    
  }
  
  return(NA)
}


# Extract sponsor name
extract_sponsor_name <- function(lines) {
  
  for (line in lines) {
    m <- str_match(line,"B\\.1\\.1 Name of Sponsor:(.+)")
    
    if(!is.na(m[2])) {
      sponsor_name <- str_squish(m[2])
      return(sponsor_name)
    }
    
  }
  
  return(NA)
}


# Extract sponsor status
extract_sponsor_status <- function(lines) {
  
  for (line in lines) {
    m <- str_match(line,"B\\.3\\.1 and B\\.3\\.2	Status of the sponsor:(.+)")
    
    if(!is.na(m[2])) {
      sponsor_status <- str_squish(m[2])
      return(sponsor_status)
    }
    
  }
  
  return(NA)
}


# Extract sponsor country
extract_sponsor_country <- function(lines) {
  
  for (line in lines) {
    m <- str_match(line,"B\\.1\\.3\\.4	Country:(.+)")
    
    if(!is.na(m[2])) {
      sponsor_country <- str_squish(m[2])
      return(sponsor_country)
    }
    
  }
  
  return(NA)
}


# Extract funder
extract_funder <- function(lines) {
  
  for (line in lines) {
    m <- str_match(line,"B\\.4\\.1 Name of organisation providing support:(.+)")
    
    if(!is.na(m[2])) {
      funder <- str_squish(m[2])
      return(funder)
    }
    
  }
  
  return(NA)
}


# Extract registration date
extract_date <- function(lines) {

  for (line in lines) {
    m <- str_match(line,"^Date on which this record was first entered in the EudraCT database: ([0-9]{4}-[0-9]{2}-[0-9]{2})$")

    if(!is.na(m[2])) {
      reg_date <- str_squish(m[2])
      return(reg_date)
    }

  }

  return(NA)
}


# # NEW Extract ISRCTN identifiers
# extract_isrctn_ids <- function(lines) {
#   
#   isrctn_ids <- c()
#   
#   for (line in lines) {
#     m <- str_match(line,"A\\.5\\.1 ISRCTN \\(International Standard Randomised Controlled Trial\\) number: (.+)")
#     
#     if(!is.na(m[2])) {
#       isrctn_id <- str_squish(m[2])
#       if (!isrctn_id %in% isrctn_ids) {
#         isrctn_ids <- append(isrctn_ids, isrctn_id)
#       }
#     }
#   }
#   return(isrctn_ids)
# }

# NEW Extract ISRCTN identifiers
extract_isrctn_id <- function(lines) {
  
  for (line in lines) {
    m <- str_match(line,"A\\.5\\.1 ISRCTN \\(International Standard Randomised Controlled Trial\\) number: (.+)")
    
    if(!is.na(m[2])) {
      isrctn_id <- str_squish(m[2])
      return(isrctn_id)
    }
  }
  return(NA)
}


# NEW Extract CT.gov identifiers
extract_ctgov_id <- function(lines) {

  for (line in lines) {
    m <- str_match(line,"A\\.5\\.2 US NCT \\(ClinicalTrials\\.gov registry\\) number: (.+)")

    if(!is.na(m[2])) {
      ctgov_id <- str_squish(m[2])
      return(ctgov_id)
    }
  }
  return(NA)
}


# NEW Extract WHO identifiers
extract_who_id <- function(lines) {
  
  for (line in lines) {
    m <- str_match(line,"A\\.5\\.3 WHO Universal Trial Reference Number \\(UTRN\\): (.+)")
    
    if(!is.na(m[2])) {
      who_id <- str_squish(m[2])
      return(who_id)
    }
  }
  return(NA)
}


# # NEW Extract other identifiers
# # Difficult case: 2017-002124-24
# extract_other_ids <- function(lines) {
#   
#   other_ids <- c()
#   
#   index <- which(lines == "A.5.4 Other Identifiers:")
#   
#   if (length(index) > 0) {
#     for (i in 1:length(index)) {
#       other_id <- lines[index[i] + 1]
#       if (!other_id %in% other_ids) {
#         other_ids <- append(other_ids, other_id)
#       }
#     }
#   }
#   return(other_ids)
# }

# NEW Extract other identifiers
# Difficult case: 2017-002124-24
extract_other_id <- function(lines) {
  
  index <- which(lines == "A.5.4 Other Identifiers:")
  
  if (length(index) > 0) {
    for (i in 1:length(index)) {
      other_id <- lines[index[i] + 1]
      return(other_id)
    }
  }
  return(NA)
}
  

# Extract identifiers from the protocol
# extract_identifiers <- function(lines) {
#   
#   ctg_identifier <- NA
#   other_identifier <- NA
#   
#   # Search for a CT.gov identifier
#   
#   for (line in lines) {
#     m <- str_match(line, "A\\.5\\.2 US NCT \\(ClinicalTrials\\.gov registry\\) number: (.+)")
#     if (!is.na(m[2])) {
#       
#       if (!is.na(ctg_identifier)) {
#         print("More than one NCT identifier found!")
#         break
#       }
#       
#       ctg_identifier <- m[2]
#     }
#   }
#   
#   # Search further identifiers
#   
#   if (length(lines) > 0 & length(grep("^A.5.4 Other Identifiers:", lines))) {
#     
#     index <- which(lines == "A.5.4 Other Identifiers:")
#     if (length(index) > 1) {
#       print("More than one other identifier found!")
#       index <- index[1]
#     }
#     other_identifier <- lines[index + 1]
#   }
#   
#   
#   return(c(ctg_identifier, other_identifier))
# }

# Extract CT.gov identifier from the Results page (if any)

extract_ctgov <- function(resulttablerows) {
  ctg_id <- NA
  
  for (resrow in resulttablerows) {
    
    resrow <- gsub("[\n\r]", "", resrow)
    
    resrow <- resrow %>%
      stringr::str_extract(
        "^US NCT number(.*)+"
      )
    
    if (! is.na(resrow)) {
      ctg_id <- sub(
        "^US NCT number(.*)",
        "\\1",
        resrow
      ) %>%
        trimws() %>%
        trimws(whitespace = "[ -]") %>%
        as.character()
      
      if (ctg_id == "") {
        ctg_id <- NA
      }
      
    }
    
    if (!is.na(ctg_id)) {
      break  
    }
  }
  
  return(ctg_id)
}

# Extract ISRCTN identifier from the Results page (if any)

extract_isrctn <- function(resulttablerows) {
  isrctn_id <- NA
  
  for (resrow in resulttablerows) {
    
    resrow <- gsub("[\n\r]", "", resrow)
    
    resrow <- resrow %>%
      stringr::str_extract(
        "^ISRCTN number(.*)+"
      )
    
    if (! is.na(resrow)) {
      isrctn_id <- sub(
        "^ISRCTN number(.*)",
        "\\1",
        resrow
      ) %>%
        trimws() %>%
        trimws(whitespace = "[ -]") %>%
        as.character()
      
      if (isrctn_id == "") {
        isrctn_id <- NA
      }
      
    }
    if (!is.na(isrctn_id)) {
      break  
    }
  }
  return(isrctn_id)
}


# Extract WHO identifier from the Results page (if any)

extract_who <- function(resulttablerows) {
  who_id <- NA
  
  for (resrow in resulttablerows) {
    
    resrow <- gsub("[\n\r]", "", resrow)
    
    resrow <- resrow %>%
      stringr::str_extract(
        "^WHO universal trial number \\(UTN\\)(.*)+"
      )
    
    if (! is.na(resrow)) {
      who_id <- sub(
        "^WHO universal trial number \\(UTN\\)(.*)",
        "\\1",
        resrow
      ) %>%
        trimws() %>%
        trimws(whitespace = "[ -]") %>%
        as.character()
      
      if (who_id == "") {
        who_id <- NA
      }
      
    }
    if (!is.na(who_id)) {
      break  
    }
  }
  return(who_id)
}


# Extract other identifier from the Results page (if any)

extract_other <- function(resulttablerows) {
  other_id <- NA
  
  for (resrow in resulttablerows) {
    
    resrow <- gsub("[\n\r]", "", resrow)
    
    resrow <- resrow %>%
      stringr::str_extract(
        "^Other trial identifiers(.*)+"
      )
    
    if (! is.na(resrow)) {
      other_id <- sub(
        "^Other trial identifiers(.*)",
        "\\1",
        resrow
      ) %>%
        trimws() %>%
        trimws(whitespace = "[ -]") %>%
        as.character()
      
      if (other_id == "") {
        other_id <- NA
      }
      
    }
    if (!is.na(other_id)) {
      break  
    }
  }
  return(other_id)
}


extract_all <- function (trn) {
  
  # Download the protocol
  protocol <- euctr_download_protocol(trn)
  
  if (length(protocol) == 1 && is.na(protocol)) {
    return(NA)
  }
  
  # Find the boundaries of each country-specific instance in the protocol
  summaries <- extract_summaries(protocol)
  
  if (!is.null(summaries)) {
    boundaries <- data.frame(
      start = summaries,
      end = if (length(summaries) == 1) {
        c(length(protocol)) 
      } else {
        append(summaries[2:length(summaries)], length(protocol)) 
      }
    )
  } else {
    print("Misformatted protocol!")
    return(NA)
  }
 
  result <- data.frame(
    euctr_id = character(),
    title = character(),
    member_state = character(),
    trial_status = character(),
    sponsor_name = character(),
    sponsor_status = character(),
    sponsor_country = character(),
    funder = character(),
    reg_date = character()
    )
  
  found_ids <- data.frame(
    euctr_id = character(),
    isrctn_id = character(),
    ctgov_id = character(),
    who_id = character(),
    other_id = character(),
    provenance = character()
  )
  
  for (row in 1:nrow(boundaries)) { 
    start <- boundaries[row, "start"]
    end <- boundaries[row, "end"]
    
    instance <- protocol[start:end]

    # Get the title
    full_title <- extract_title(instance)
    
    # Get the member state
    member_state <- extract_member_state(instance)
    
    # Get the trial status
    trial_status <- extract_trial_status(instance)
    
    # Get the sponsor name
    sponsor_name <- extract_sponsor_name(instance)
    
    # Get the sponsor status
    sponsor_status <- extract_sponsor_status(instance)
    
    # Get the sponsor country
    sponsor_country <- extract_sponsor_country(instance)
    
    # Get the funder
    funder <- extract_funder(instance)
    
    # Get the registration date
    date <- extract_date(instance)
    
    # Store in results dataframe
    result <- result %>%
      add_row(
        euctr_id = trn,
        title = full_title,
        member_state = member_state,
        trial_status = trial_status,
        sponsor_name = sponsor_name,
        sponsor_status = sponsor_status,
        sponsor_country = sponsor_country,
        funder = funder,
        reg_date = date
      )
    
    # Get any additional identifiers from the protocol instance
    isrctn_id <- extract_isrctn_id(instance)
    ctgov_id <- extract_ctgov_id(instance)
    who_id <- extract_who_id(instance)
    other_id <- extract_other_id(instance)
    
    if (any(!is.na(c(isrctn_id, ctgov_id, who_id, other_id)))) {
      found_ids <- found_ids %>%
        add_row(euctr_id = trn,
                isrctn_id = isrctn_id,
                ctgov_id = ctgov_id,
                who_id = who_id,
                other_id = other_id,
                provenance = paste0("protocol-", row))
      }
    }
  
  # Download the Results page (if any)
  results_data <- euctr_download_results(trn)
  
  # Get the CT.gov identifier from the results
  ctgov <- extract_ctgov(results_data)
  
  # Get the ISRCTN identifier from the results
  isrctn <- extract_isrctn(results_data)
  
  # Get the WHO identifier from the results
  who <- extract_who(results_data)
  
  # Get other identifier from the results
  other <- extract_other(results_data)
  
  found_ids <- found_ids %>%
    add_row(euctr_id = trn,
            isrctn_id = isrctn,
            ctgov_id = ctgov,
            who_id = who,
            other_id = other,
            provenance = "results")
  
  return(list(result, found_ids))
}


# Gets all info for n EUCTR trials
combine_info <- function(trials) {
  
  table_other <- data.frame(
    euctr_id = character(),
    title = character(),
    member_state = character(),
    trial_status = character(),
    sponsor_name = character(),
    sponsor_status = character(),
    sponsor_country = character(),
    funder = character(),
    reg_date = character()
  )
  
  table_identifiers <- data.frame(
    euctr_id = character(),
    ctgov_id = character(),
    other_id = character(),
    provenance = character()
  )
  
  # final_table <- data.frame(
  #   euctr_id = character(),
  #   title = character(),
  #   member_state = character(),
  #   trial_status = character(),
  #   sponsor_name = character(),
  #   sponsor_status = character(),
  #   sponsor_country = character(),
  #   funder = character(),
  #   reg_date = character(),
  #   other_id = character(),
  #   field = character(),
  #   provenance = character()
  # )
  
  unresolved <- data.frame(
    unresolved_id = character()
  )
  
  for (trial in trials) {
    
    res <- extract_all(trial)
    
    if (length(res) == 1 && is.na(res)) {
      print(paste0(trial, " could not be resolved!"))
      
      unresolved <- unresolved |> 
        add_row(
            unresolved_id = trial
            )
      next
      }
    
    res_other <- res[[1]]
    table_other <- rbind(table_other, res_other)
    
    res_identifiers <- res[[2]]
    table_identifiers <- rbind(table_identifiers, res_identifiers)
    
    # final_table <- full_join(table_other,
    #                          table_identifiers,
    #                          by = "euctr_id",
    #                          multiple = "all")
  }
  return(list(table_other, table_identifiers, unresolved))
}