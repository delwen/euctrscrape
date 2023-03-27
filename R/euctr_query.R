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
  pull(id)

results <- combine_info(trials)

write_csv(results, here("data", "2023-03-27_charite-ids.csv"))