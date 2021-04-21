Regression and Other Stories: Elections Economy – model checking
================
Andrew Gelman, Jennifer Hill, Aki Vehtari
2021-04-20

-   [7 Linear regression with a single
    predictor](#7-linear-regression-with-a-single-predictor)
    -   [7.2 Checking the model-fitting procedure using fake-data
        simulation](#72-checking-the-model-fitting-procedure-using-fake-data-simulation)

Tidyverse version by Bill Behrman.

Elections Economy – model checking. Checking the model-fitting procedure
using fake-data simulation. See Chapter 7 in Regression and Other
Stories.

------------------------------------------------------------------------

``` r
# Packages
library(tidyverse)
library(rstanarm)

# Parameters
  # U.S. Presidential election results and GDP growth
file_hibbs <- here::here("ElectionsEconomy/data/hibbs.dat")
  # Common code
file_common <- here::here("_common.R")
  
#===============================================================================

# Run common code
source(file_common)
```

# 7 Linear regression with a single predictor

## 7.2 Checking the model-fitting procedure using fake-data simulation

Actual data.

``` r
hibbs <- 
  file_hibbs %>% 
  read.table(header = TRUE) %>% 
  as_tibble()

hibbs
```

    #> # A tibble: 16 x 5
    #>     year growth  vote inc_party_candidate other_candidate
    #>    <int>  <dbl> <dbl> <chr>               <chr>          
    #>  1  1952   2.4   44.6 Stevenson           Eisenhower     
    #>  2  1956   2.89  57.8 Eisenhower          Stevenson      
    #>  3  1960   0.85  49.9 Nixon               Kennedy        
    #>  4  1964   4.21  61.3 Johnson             Goldwater      
    #>  5  1968   3.02  49.6 Humphrey            Nixon          
    #>  6  1972   3.62  61.8 Nixon               McGovern       
    #>  7  1976   1.08  49.0 Ford                Carter         
    #>  8  1980  -0.39  44.7 Carter              Reagan         
    #>  9  1984   3.86  59.2 Reagan              Mondale        
    #> 10  1988   2.27  53.9 Bush, Sr.           Dukakis        
    #> # … with 6 more rows

Parameters for simulation data.

``` r
a <- 46.3
b <- 3.1
sigma <- 3.9
```

Parameters and posterior uncertainty interval probabilities to test.

``` r
params_probs <- 
  tribble(
    ~x,    ~param,
    a,     "(Intercept)",
    b,     "x",
    sigma, "sigma"
  ) %>% 
  mutate(prob = list(c(0.5, 0.90, 0.95))) %>% 
  unnest(cols = prob)

params_probs
```

    #> # A tibble: 9 x 3
    #>       x param        prob
    #>   <dbl> <chr>       <dbl>
    #> 1  46.3 (Intercept)  0.5 
    #> 2  46.3 (Intercept)  0.9 
    #> 3  46.3 (Intercept)  0.95
    #> 4   3.1 x            0.5 
    #> 5   3.1 x            0.9 
    #> 6   3.1 x            0.95
    #> 7   3.9 sigma        0.5 
    #> 8   3.9 sigma        0.9 
    #> 9   3.9 sigma        0.95

Check whether parameter is within posterior uncertainty interval.

``` r
in_posterior_interval <- function(fit, x, param, prob) {
  posterior_interval <- posterior_interval(fit, prob = prob, pars = param)
  tibble(
    param = param,
    prob = prob,
    in_posterior_interval = 
      (x >= posterior_interval[1]) && (x <= posterior_interval[2])
  )
}
```

Generate simulation data, fit linear regression model to data, and
determine whether parameters are in their posterior uncertainty
intervals.

``` r
sim <- function() {
  data <- 
    tibble(
      x = hibbs$growth,
      y = a + b * x + rnorm(length(x), mean = 0, sd = sigma)
    )
  
  fit <- stan_glm(y ~ x, data = data, refresh = 0)
  
  params_probs %>% 
    pmap_dfr(in_posterior_interval, fit = fit)
}
```

``` r
n_sims <- 1000
```

Perform simulation 1,000 times.

``` r
set.seed(378)

sims <- map_dfr(seq_len(n_sims), ~ sim())
```

We can now check the proportion of simulations where the posterior
uncertainty intervals covered the parameters used to generate the random
data for the fits.

``` r
sims %>% 
  mutate(
    param = 
      case_when(
        param == "(Intercept)" ~ "a",
        param == "x" ~ "b",
        TRUE ~ param
      )
  ) %>% 
  group_by(param, prob) %>% 
  summarize(posterior_interval_prop = mean(in_posterior_interval)) %>% 
  ungroup() %>% 
  knitr::kable()
```

| param | prob | posterior\_interval\_prop |
|:------|-----:|--------------------------:|
| a     | 0.50 |                     0.480 |
| a     | 0.90 |                     0.903 |
| a     | 0.95 |                     0.940 |
| b     | 0.50 |                     0.492 |
| b     | 0.90 |                     0.899 |
| b     | 0.95 |                     0.944 |
| sigma | 0.50 |                     0.503 |
| sigma | 0.90 |                     0.921 |
| sigma | 0.95 |                     0.956 |

In all cases, the proportion of simulations where the posterior
uncertainty interval covered the parameters was close to the
probabilities defining the intervals.
