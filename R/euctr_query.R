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

# Create dataframe of fields from the EUCTR protocol
euctr_data <- results[[1]]

# Create dataframe of any additional ids found in the EUCTR protocol/results
identifiers <- results[[2]]

# Create dataframe of any unresolved trials in the EUCTR
unresolved <- results[[3]]

write_csv(euctr_data, here("data", "2023-07-20_euctr_data.csv"))
write_csv(identifiers, here("data", "2023-07-20_identifiers.csv"))
write_csv(unresolved, here("data", "2023-07-20_unresolved.csv"))