Regression and Other Stories: Pollution
================
Andrew Gelman, Jennifer Hill, Aki Vehtari
2021-04-20

-   [12 Transformations and
    regression](#12-transformations-and-regression)
    -   [12.9 Exercises](#129-exercises)

Tidyverse version by Bill Behrman.

A pollution data set. See Chapter 12 in Regression and Other Stories.

Source: McDonald, G.C. and Schwing, R.C. (1973) ‘Instabilities of
regression estimates relating air pollution to mortality’,
Technometrics, vol.15, 463-482. See data/pollution.txt for the
explanation of the variables.

------------------------------------------------------------------------

# 12 Transformations and regression

## 12.9 Exercises

``` r
# Packages
library(tidyverse)

# Parameters
  # Data on air polution and mortality
file_pollution <- here::here("Pollution/data/pollution.csv")
  # Common code
file_common <- here::here("_common.R")

#===============================================================================

# Run common code
source(file_common)
```

Data.

``` r
pollution <- read_csv(file_pollution)

glimpse(pollution)
```

    #> Rows: 60
    #> Columns: 16
    #> $ prec  <dbl> 36, 35, 44, 47, 43, 53, 43, 45, 36, 36, 52, 33, 40, 35, 37, 35,…
    #> $ jant  <dbl> 27, 23, 29, 45, 35, 45, 30, 30, 24, 27, 42, 26, 34, 28, 31, 46,…
    #> $ jult  <dbl> 71, 72, 74, 79, 77, 80, 74, 73, 70, 72, 79, 76, 77, 71, 75, 85,…
    #> $ ovr65 <dbl> 8.1, 11.1, 10.4, 6.5, 7.6, 7.7, 10.9, 9.3, 9.0, 9.5, 7.7, 8.6, …
    #> $ popn  <dbl> 3.34, 3.14, 3.21, 3.41, 3.44, 3.45, 3.23, 3.29, 3.31, 3.36, 3.3…
    #> $ educ  <dbl> 11.4, 11.0, 9.8, 11.1, 9.6, 10.2, 12.1, 10.6, 10.5, 10.7, 9.6, …
    #> $ hous  <dbl> 81.5, 78.8, 81.6, 77.5, 84.6, 66.8, 83.9, 86.0, 83.2, 79.3, 69.…
    #> $ dens  <dbl> 3243, 4281, 4260, 3125, 6441, 3325, 4679, 2140, 6582, 4213, 230…
    #> $ nonw  <dbl> 8.8, 3.5, 0.8, 27.1, 24.4, 38.5, 3.5, 5.3, 8.1, 6.7, 22.2, 16.3…
    #> $ wwdrk <dbl> 42.6, 50.7, 39.4, 50.2, 43.7, 43.1, 49.2, 40.4, 42.5, 41.0, 41.…
    #> $ poor  <dbl> 11.7, 14.4, 12.4, 20.6, 14.3, 25.5, 11.3, 10.5, 12.6, 13.2, 24.…
    #> $ hc    <dbl> 21, 8, 6, 18, 43, 30, 21, 6, 18, 12, 18, 88, 26, 31, 23, 1, 6, …
    #> $ nox   <dbl> 15, 10, 6, 8, 38, 32, 32, 4, 12, 7, 8, 63, 26, 21, 9, 1, 4, 8, …
    #> $ so2   <dbl> 59, 39, 33, 24, 206, 72, 62, 4, 37, 20, 27, 278, 146, 64, 15, 1…
    #> $ humid <dbl> 59, 57, 54, 56, 55, 54, 56, 56, 61, 59, 56, 58, 57, 60, 58, 54,…
    #> $ mort  <dbl> 922, 998, 962, 982, 1071, 1030, 935, 900, 1002, 912, 1018, 1025…
