# Returns the following for a set of EUCTR trials ids:
# full title, registration date(s), any identifier(s) in protocol or results

library(dplyr)
library(readr)
library(here)
library(stringr)

source(here("R", "euctr_download.R"))
source(here("R", "euctr_extract.R"))

# Update input file name here (ids from EU Trials Tracker)
data <- read_csv(here("data", "2022-12-03_charite-euctr-trials.csv"))

trials <- data %>%
  distinct(id, .keep_all = TRUE) %>%
  pull(id)

results <- combine_info(trials)

# Create dataframe of final results table with all the extracted fields
euctr_data <- results[[1]]

# Create dataframe of any unresolved trials in the EUCTR
unresolved <- results[[2]]

write_csv(euctr_data, here("data", "2023-07-18_charite-ids.csv"))