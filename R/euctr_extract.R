# Extract full title of the trial

extract_title <- function(lines) {
  
  for (line in lines) {
    m <- str_match(line,"A\\.3 Full title of the trial:(.+)")
    
    if(!is.na(m[2])){
      return(m[2])
    }
  }
  
  return(NA)
}


# Extract registration date(s)
## TODO: collect registration dates across all country protocols

extract_date <- function(lines) {
  
  for (line in lines) {
    m <- str_match(line,"^Date on which this record was first entered in the EudraCT database: ([0-9]{4}-[0-9]{2}-[0-9]{2})$")
    
    if(!is.na(m[2])) {
      return(m[2])
    }

  }
  
  return(NA)
}

# Extract identifiers from the protocol
## TODO: collect identifiers across all country protocols

extract_identifiers <- function(lines) {
  
  ctg_identifier <- NA
  other_identifier <- NA
  
  # Search for a CT.gov identifier
  
  for (line in lines) {
    m <- str_match(line, "A\\.5\\.2 US NCT \\(ClinicalTrials\\.gov registry\\) number: (.+)")
    if (!is.na(m[2])) {
      
      if (!is.na(ctg_identifier)) {
        print("More than one NCT identifier found!")
        print(trn)
        break
      }
      
      ctg_identifier <- m[2]
    }
  }
  
  # Search further identifiers
  
  if (length(lines) > 0 & length(grep("^A.5.4 Other Identifiers:", lines))) {
    
    index <- which(lines == "A.5.4 Other Identifiers:")
    if (length(index) > 1) {
      print("More than one other identifier found!")
      print(trn)
      index <- index[1]
    }
    other_identifier <- lines[index + 1]
  }
  
  
  return(c(ctg_identifier, other_identifier))
}

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


extract_all <- function (trn) {
  
  # Download the protocol
  protocol <- euctr_download_protocol(trn)
 
  # Get the title
  full_title <- extract_title(protocol)
  
  # Get the registration date
  date <- extract_date(protocol)
  
  # Store in results dataframe
  result <- data.frame(
    euctr_id = character(),
    title = character(),
    reg_date = character()
  ) %>%
    add_row(
      euctr_id = trn,
      title = full_title,
      reg_date = date
    )
  
  # Get any additional identifiers
  ids <- extract_identifiers(protocol)
  
  # Store in separate dataframe
  found_ids <- data.frame(
    euctr_id = character(),
    other_id = character(),
    field = character(),
    provenance = character()
  )
  
  found_ids <- found_ids %>%
    add_row(euctr_id = trn,
            other_id = ids[1],
            field="US NCT number",
            provenance = "protocol") %>%
    add_row(euctr_id = trn,
            other_id = ids[2],
            field = "other",
            provenance = "protocol")
  
  
  # Download the Results page (if any)
  results_data <- euctr_download_results(trn)
  
  # Get the CT.gov identifier
  ctgov <- extract_ctgov(results_data)
  
  # if (!is.na(ctgov))
  found_ids <- found_ids %>%
    add_row(euctr_id = trn,
            other_id = ctgov,
            field = "US NCT number",
            provenance = "results")
  
  # Get the ISRCTN identifier
  isrctn <- extract_isrctn(results_data)
  
  #if (!is.na(isrctn))
  found_ids <- found_ids %>%
    add_row(euctr_id = trn,
            other_id = isrctn,
            field = "ISRCTN number",
            provenance = "results")
  
  return(list(result, found_ids))
}


# Gets all info for n EUCTR trials
combine_info <- function(trials) {
  
  table_other <- data.frame(
    euctr_id = character(),
    title = character(),
    reg_date = character()
  )
  
  table_identifiers <- data.frame(
    euctr_id = character(),
    other_id = character(),
    field = character(),
    provenance = character()
  )
  
  final_table <- data.frame(
    euctr_id = character(),
    title = character(),
    reg_date = character(),
    other_id = character(),
    field = character(),
    provenance = character()
  )
  
  for (trial in trials) {
    
    res <- extract_all(trial)
    
    res_other <- res[[1]]
    table_other <- rbind(table_other, res_other)
    
    res_identifiers <- res[[2]]
    table_identifiers <- rbind(table_identifiers, res_identifiers)
    
    final_table <- full_join(table_other, table_identifiers, by = "euctr_id")
  }
  return(final_table)
}