Regression and Other Stories: Scalability
================
Andrew Gelman, Jennifer Hill, Aki Vehtari
2021-04-20

-   [22 Advanced regression and multilevel
    models](#22-advanced-regression-and-multilevel-models)
    -   [22.8 Computational efficiency](#228-computational-efficiency)
        -   [Parallel processing](#parallel-processing)
        -   [Mode-based approximations](#mode-based-approximations)

Tidyverse version by Bill Behrman.

Demonstrate how the computation time scales with bigger data. See
Chapter 22 in Regression and Other Stories.

------------------------------------------------------------------------

``` r
# Packages
library(tidyverse)
library(bench)
library(rstanarm)

# Parameters
  # Common code
file_common <- here::here("_common.R")

#===============================================================================

# Run common code
source(file_common)
```

# 22 Advanced regression and multilevel models

## 22.8 Computational efficiency

### Parallel processing

``` r
getOption("mc.cores")
```

    #> NULL

``` r
options(mc.cores = parallel::detectCores())

getOption("mc.cores")
```

    #> [1] 8

### Mode-based approximations

Simulated data.

``` r
set.seed(1656)

a <-  2
b <- 3
sigma <- 1

nrow <- 1e4
ncol <- 100

data <- 
  set_names(c("x", str_c("noise_", seq_len(ncol - 1)))) %>% 
  map_dfc(~ rnorm(nrow)) %>% 
  mutate(
    y = if_else(a + b * x + sigma * rnorm(nrow) > 0, 1, 0)
  )
```

We then fit the logistic regression three different ways:

``` r
set.seed(407)

benchmarks <- 
  bench::mark(
    fit_1 <- glm(y ~ ., family = binomial(link = "logit"), data = data),
    fit_2 <- 
      stan_glm(
        y ~ .,
        family = binomial(link = "logit"),
        data = data,
        algorithm = "optimizing"
      ),
    fit_3 <- 
      stan_glm(
        y ~ .,
        family = binomial(link = "logit"),
        data = data
      ),
    check = FALSE,
    memory = FALSE
  )
```

``` r
v <-
  summary(benchmarks, relative = TRUE) %>% 
  select(relative_time = median)
  
v
```

    #> # A tibble: 3 x 1
    #>   relative_time
    #>           <dbl>
    #> 1          1   
    #> 2          1.82
    #> 3         92.0

`stan_glm()` with the optimizing algorithm took 1.8 times as long as
`glm()`. `stan_glm()` with the default sampling algorithm took 92 times
as long. In other words, `stan_glm()` with the sampling algorithm took
51 times longer than with the optimizing algorithm.

Let’s compare the coefficients for the three models:

``` r
tibble(
  model = list(fit_1, fit_2, fit_3),
  `(Intercept)` = map_dbl(model, ~ coef(.)[["(Intercept)"]]),
  x = map_dbl(model, ~ coef(.)[["x"]]),
  noise_max = 
    map_dbl(
      model,
      ~ coef(.) %>% 
        keep(str_detect(names(.), "^noise_")) %>% 
        max(abs(.))
    )
) %>% 
  select(!model)
```

    #> # A tibble: 3 x 3
    #>   `(Intercept)`     x noise_max
    #>           <dbl> <dbl>     <dbl>
    #> 1          3.68  5.51     0.122
    #> 2          3.77  5.63     0.124
    #> 3          3.77  5.65     0.125

The coefficients in all models for the non-noise terms are close to each
other, especially those from `stan_glm()`. In all cases, the
coefficients of the noise terms are small.
