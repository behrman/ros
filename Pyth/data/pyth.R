# Convert data to CSV

# Author: Bill Behrman
# Version: 2021-04-12

# Libraries
library(tidyverse)

# Parameters
  # Input data file
file_in <- here::here("Pyth/pyth.txt")
  # Output CSV file
file_out <- here::here("Pyth/pyth.csv")

#===============================================================================

file_in %>% 
  read_table2(col_types = cols(.default = col_double())) %>% 
  write_csv(file_out)
