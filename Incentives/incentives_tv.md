Regression and Other Stories: Electric Company
================
Andrew Gelman, Jennifer Hill, Aki Vehtari
2021-04-20

-   [19 Causal inference using direct
    regression](#19-causal-inference-using-direct-regression)
    -   [19.5 Challenges of interpreting regression coefficients as
        treatment
        effects](#195-challenges-of-interpreting-regression-coefficients-as-treatment-effects)

Tidyverse version by Bill Behrman.

Simple analysis of incentives data. See Chapter 19 in Regression and
Other Stories.

------------------------------------------------------------------------

``` r
# Packages
library(tidyverse)
library(rstanarm)

# Parameters
  # Incentives data
file_incentives <- here::here("Incentives/data/incentives.csv") 
  # Common code
file_common <- here::here("_common.R")

#===============================================================================

# Run common code
source(file_common)
```

# 19 Causal inference using direct regression

## 19.5 Challenges of interpreting regression coefficients as treatment effects

Data from a meta-analysis of studies of incentives in sample surveys.

``` r
incentives <- read_csv(file_incentives)

incentives
```

    #> # A tibble: 62 x 5
    #>    rr_diff value prepay  gift burden
    #>      <dbl> <dbl>  <dbl> <dbl>  <dbl>
    #>  1       3  1.24      1     0      0
    #>  2       6  2.47      1     1      0
    #>  3       9 14.7       0     0      1
    #>  4       4 24.6       0     0      1
    #>  5       6 43.1       0     0      1
    #>  6      13 17.3       0     0      1
    #>  7      10 21.6       0     0      1
    #>  8      15 43.2       0     0      1
    #>  9      16 10.3       0     0      1
    #> 10      -1  5.65      0     0      1
    #> # … with 52 more rows

Fit linear regression.

``` r
set.seed(447)

fit <- 
  stan_glm(
    rr_diff ~ value + prepay + gift + burden,
    data = incentives,
    refresh = 0
  )

print(fit, digits = 2)
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      rr_diff ~ value + prepay + gift + burden
    #>  observations: 62
    #>  predictors:   5
    #> ------
    #>             Median MAD_SD
    #> (Intercept)  1.61   1.63 
    #> value        0.12   0.04 
    #> prepay       4.02   2.06 
    #> gift        -5.37   2.23 
    #> burden       2.94   1.58 
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 6.02   0.56  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

The above coefficients should not be directly interpreted as causal
effects. Although incentive conditions were assigned randomly *within*
each experiment, the differences in the conditions were not assigned at
random *between* experiments. Thus, when comparing incentives
implemented in different surveys, what we have is an observational
study.
