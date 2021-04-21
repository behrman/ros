Regression and Other Stories: Elections Economy
================
Andrew Gelman, Jennifer Hill, Aki Vehtari
2021-04-20

-   [8 Fitting regression models](#8-fitting-regression-models)
    -   [8.1 Least squares, maximum likelihood, and Bayesian
        inference](#81-least-squares-maximum-likelihood-and-bayesian-inference)
        -   [Where do the standard errors come from? Using the
            likelihood surface to assess uncertainty in the parameter
            estimates](#where-do-the-standard-errors-come-from-using-the-likelihood-surface-to-assess-uncertainty-in-the-parameter-estimates)
    -   [8.4 Comparing two fitting function: `lm()` and
        `stan_glm()`](#84-comparing-two-fitting-function-lm-and-stan_glm)
        -   [Reproducing maximum likelihood using `stan_glm()` with flat
            priors and
            optimization](#reproducing-maximum-likelihood-using-stan_glm-with-flat-priors-and-optimization)

Tidyverse version by Bill Behrman.

Present uncertainty in parameter estimates. See Chapter 8 in Regression
and Other Stories.

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

# 8 Fitting regression models

## 8.1 Least squares, maximum likelihood, and Bayesian inference

### Where do the standard errors come from? Using the likelihood surface to assess uncertainty in the parameter estimates

Data

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

Classical least squares linear regression.

``` r
fit_1 <- lm(vote ~ growth, data = hibbs)

arm::display(fit_1)
```

    #> lm(formula = vote ~ growth, data = hibbs)
    #>             coef.est coef.se
    #> (Intercept) 46.25     1.62  
    #> growth       3.06     0.70  
    #> ---
    #> n = 16, k = 2
    #> residual sd = 3.76, R-Squared = 0.58

The linear regression coefficients returned by `lm()` are at the maximum
of a multivariate normal likelihood function.

Coefficients and coefficient likelihood.

``` r
coef_1 <- coef(fit_1)
a_1 <- coef_1[["(Intercept)"]]
b_1 <- coef_1[["growth"]]
vcov_1 <- vcov(fit_1)
a_1_se <- sqrt(diag(vcov_1))[["(Intercept)"]]
b_1_se <- sqrt(diag(vcov_1))[["growth"]]
n_points <- 201

v <- 
  expand_grid(
    a = seq(a_1 - 4 * a_1_se, a_1 + 4 * a_1_se, length.out = n_points),
    b = seq(b_1 - 4 * b_1_se, b_1 + 4 * b_1_se, length.out = n_points)
  ) %>% 
  mutate(
    prob = mvtnorm::dmvnorm(x = as.matrix(.), mean = coef_1, sigma = vcov_1)
  )

v %>% 
  ggplot(aes(a, b)) +
  geom_contour(aes(z = prob)) +
  geom_point(data = tibble(a = a_1, b = b_1), color = "red") +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  labs(title = "Coefficients and coefficient likelihood")
```

<img src="hills_tv_files/figure-gfm/unnamed-chunk-4-1.png" width="100%" />

This plot displays the likelihood for a simple example as a function of
the coefficients a and b. Strictly speaking, this model has three
parameters – a, b, and *σ* – but for simplicity we display the
likelihood of a and b conditional on the estimated *σ̂*.

Coefficients with ±1 standard error and coefficient likelihood.

``` r
v %>% 
  ggplot(aes(a, b)) +
  geom_contour(aes(z = prob)) +
  annotate(
    "segment",
    x    = c(a_1 - a_1_se, a_1),
    xend = c(a_1 + a_1_se, a_1),
    y    = c(b_1, b_1 - b_1_se),
    yend = c(b_1, b_1 + b_1_se)
  ) +
  geom_point(data = tibble(a = a_1, b = b_1), color = "red") +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  labs(
    title = "Coefficients with ±1 standard error and coefficient likelihood"
  )
```

<img src="hills_tv_files/figure-gfm/unnamed-chunk-5-1.png" width="100%" />

This plot shows the maximum likelihood estimate in red and also includes
the uncertainty bars showing ±1 standard error for each parameter.

## 8.4 Comparing two fitting function: `lm()` and `stan_glm()`

### Reproducing maximum likelihood using `stan_glm()` with flat priors and optimization

A Bayesian model using flat priors will result in a posterior
distribution that is the same as the likelihood function for classical
least squares regression. If we direct the algorithm to perform
optimization instead of sampling, it will seek the same maximum
likelihood estimate that is returned by classical least squares
regression.

``` r
set.seed(466)

fit_2 <- 
  stan_glm(
    vote ~ growth,
    data = hibbs,
    refresh = 0,
    prior = NULL,
    prior_intercept = NULL,
    prior_aux = NULL,
    algorithm = "optimizing"
  )
```

``` r
coef_2 <- coef(fit_2)

coef_2 - coef_1
```

    #> (Intercept)      growth 
    #>    -0.00298     0.02453

The coefficients obtained through optimizing are close to the least
square coefficients.

We’ll now direct the algorithm to to fit the sample model with sampling
(MCMC) instead of optimization.

``` r
set.seed(466)

fit_3 <- 
  stan_glm(
    vote ~ growth,
    data = hibbs,
    refresh = 0,
    prior = NULL,
    prior_intercept = NULL,
    prior_aux = NULL,
    algorithm = "sampling"
  )
```

``` r
coef_3 <- coef(fit_3)

coef_3 - coef_1
```

    #> (Intercept)      growth 
    #>    -0.00542    -0.00782

The coefficients obtained through sampling are also close to the least
square coefficients.
