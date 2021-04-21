Regression and Other Stories: Helicopters
================
Andrew Gelman, Jennifer Hill, Aki Vehtari
2021-04-20

-   [1 Overview](#1-overview)
    -   [1.8 Exercises](#18-exercises)

Tidyverse version by Bill Behrman.

Example data file for helicopter flying time exercise. See Chapter 1 in
Regression and Other Stories.

------------------------------------------------------------------------

``` r
# Packages
library(tidyverse)

# Parameters
  # U.S. Presidential election results and GDP growth
file_helicopters <- here::here("Helicopters/data/helicopters.txt")
  # Common code
file_common <- here::here("_common.R")

#===============================================================================

# Run common code
source(file_common)
```

# 1 Overview

## 1.8 Exercises

Data

``` r
helicopters <- 
  file_helicopters %>% 
  read.table(header = TRUE) %>% 
  as_tibble(.name_repair = str_to_lower)

helicopters
```

    #> # A tibble: 20 x 4
    #>    helicopter_id width_cm length_cm time_sec
    #>            <int>    <dbl>     <dbl>    <dbl>
    #>  1             1      4.6       8.2     1.64
    #>  2             1      4.6       8.2     1.74
    #>  3             1      4.6       8.2     1.68
    #>  4             1      4.6       8.2     1.62
    #>  5             1      4.6       8.2     1.68
    #>  6             1      4.6       8.2     1.7 
    #>  7             1      4.6       8.2     1.62
    #>  8             1      4.6       8.2     1.66
    #>  9             1      4.6       8.2     1.69
    #> 10             1      4.6       8.2     1.62
    #> # … with 10 more rows
