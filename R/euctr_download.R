euctr_download_url <- function (url, name, retry = 10) {
  
  new_name <- paste0("cache/", name, ".txt")
  
  if (!file.exists(new_name)) {
    print(paste0("Downloading ", url, " to ", name))

    dlfile <- tempfile()
    
    if (!dir.exists("cache")) {
      dir.create("cache")
    }
    
    found <- FALSE
    
    # Adjust the sleep as needed
    Sys.sleep(1 + runif(1, 0, 2))
    
    for (i in 1:retry) {
    
      ## I'd much prefer to use `polite`, but there's a TLS problem, so
      ## this is basically the only solution that works
      utils::download.file(url, dlfile, method="curl", extra="-k -s")
      
      if (file.size(dlfile) > 0) {
        file.rename(dlfile, new_name)
        found <- TRUE
        break
      }

      # Adjust the sleep as needed
      Sys.sleep(1 + runif(1, 0, 2))
    }
    
    if (!found) {
      return(NA)
    }
  }
  
  return(new_name)
}


euctr_download_protocol <- function (trn) {
  
  ## Check for well-formed input
  assertthat::assert_that(
    is.character(trn),
    grepl(
      "^[0-9]{4}-[0-9]{6}-[0-9]{2}$",
      trn
    )
  )
  
  new_name <- paste0(trn, ".protocol")
  
  url <- paste0(
    "https://www.clinicaltrialsregister.eu/",
    "ctr-search/rest/download/full?query=",
    trn,
    "&mode=current_page"
  )
  
  path <- euctr_download_url(url, new_name)

  if (is.na(path)) {
    return(NA)
  }
  
  return(readLines(path))
}


euctr_download_results <- function (trn) {
  
  ## Check for well-formed input
  assertthat::assert_that(
    is.character(trn),
    grepl(
      "^[0-9]{4}-[0-9]{6}-[0-9]{2}$",
      trn
    )
  )
  
  new_name <- paste0(trn, ".results")
  
  url <- paste0(
    "https://www.clinicaltrialsregister.eu/ctr-search/",
    "trial/",
    trn,
    "/results"
  )
  
  path <- euctr_download_url(url, new_name)
  
  # TODO: handle NA
  
  html <- rvest::read_html(path)
  
  resulttablerows <- html %>%
    rvest::html_nodes("#resultContent table tr") %>%
    rvest::html_text2()
  
  return(resulttablerows)
}