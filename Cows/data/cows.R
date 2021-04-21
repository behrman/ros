# Convert data to CSV.

# Author: Bill Behrman
# Version: 2021-03-23

# Libraries
library(tidyverse)

# Parameters
  # Input file
file_in <- here::here("Cows/cows.dat")
  # Output file
file_out <- here::here("Cows/cows.csv")

#==============================================================================

file_in %>% 
  read_table2(skip = 4) %>% 
  janitor::clean_names() %>% 
  write_csv(file_out)
