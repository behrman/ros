Regression and Other Stories: RiskyBehavior
================
Andrew Gelman, Jennifer Hill, Aki Vehtari
2021-04-20

-   [15 Other generalized linear
    models](#15-other-generalized-linear-models)
    -   [15.10 Exercises](#1510-exercises)

Tidyverse version by Bill Behrman.

Data from a randomized trial targeting couples at high risk of HIV
infection. The intervention provided counseling sessions regarding
practices that could reduce their likelihood of contracting HIV. Couples
were randomized either to a control group, a group in which just the
woman participated, or a group in which both members of the couple
participated. One of the outcomes examined after three months was
“number of unprotected sex acts.” See Chapter 15 in Regression and Other
Stories.

Reference: El-Bassel, N., Witte, S. S., Gilbert, L., Wu, E., Chang, M.,
Hill, J., and Steinglass, P. (2003). The efficacy of a
relationship-based HIV/STD prevention program for heterosexual couples.
*American journal of public health*, **93**, 963–969.

DOI:
[10.2105/AJPH.93.6.963](https://ajph.aphapublications.org/doi/10.2105/AJPH.93.6.963)

------------------------------------------------------------------------

``` r
# Packages
library(tidyverse)

# Parameters
  # Data from a randomized trial targeting couples at high risk of HIV infection
file_risky <- here::here("RiskyBehavior/data/risky.csv")
  # Common code
file_common <- here::here("_common.R")

#===============================================================================

# Run common code
source(file_common)
```

# 15 Other generalized linear models

## 15.10 Exercises

``` r
risky <- read_csv(file_risky)

risky
```

    #> # A tibble: 434 x 6
    #>    sex   couples women_alone bs_hiv   bupacts fupacts
    #>    <chr>   <dbl>       <dbl> <chr>      <dbl>   <dbl>
    #>  1 woman       0           1 negative       7      32
    #>  2 woman       0           0 negative       2       5
    #>  3 woman       0           0 positive       0      15
    #>  4 woman       0           0 negative      24       9
    #>  5 woman       1           0 negative       2       2
    #>  6 woman       1           0 negative      15       4
    #>  7 woman       1           0 positive       9       2
    #>  8 woman       0           1 positive       9       1
    #>  9 woman       0           0 positive       2       0
    #> 10 woman       0           1 positive      40       0
    #> # … with 424 more rows
