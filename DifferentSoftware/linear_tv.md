Regression and Other Stories: Different software options
================
Andrew Gelman, Aki Vehtari
2021-04-20

-   [A Computing in R](#a-computing-in-r)
    -   [A.8 Working with rstanarm fit
        objects](#a8-working-with-rstanarm-fit-objects)
        -   [Fitting Stan models in R using
            brms](#fitting-stan-models-in-r-using-brms)

Tidyverse version by Bill Behrman.

Linear regression using different software options. See Appendix A in
Regression and Other Stories.

------------------------------------------------------------------------

``` r
# Packages
library(tidyverse)
library(arm)
library(brms)
library(rstanarm)
library(tidymodels)

# Parameters
  # Common code
file_common <- here::here("_common.R")

#===============================================================================

# Run common code
source(file_common)
```

# A Computing in R

## A.8 Working with rstanarm fit objects

Simulated data.

``` r
set.seed(563)

n <- 100

b <- 1:3
sigma <- 2

data <- 
  tibble(
    x_1 = rnorm(n),
    x_2 = rnorm(n),
    y = b[1] + b[2] * x_1 + b[3] * x_2 + rnorm(n, mean = 0, sd = sigma)
  )
```

#### Fit and display using `lm()`

``` r
fit_lm <- lm(y ~ x_1 + x_2, data = data)

display(fit_lm)
```

    #> lm(formula = y ~ x_1 + x_2, data = data)
    #>             coef.est coef.se
    #> (Intercept) 1.01     0.22   
    #> x_1         1.59     0.24   
    #> x_2         2.60     0.21   
    #> ---
    #> n = 100, k = 3
    #> residual sd = 2.19, R-Squared = 0.70

Extract coefficient estimates.

``` r
coef(fit_lm)
```

    #> (Intercept)         x_1         x_2 
    #>        1.01        1.59        2.60

Extract coefficient uncertainties.

``` r
se.coef(fit_lm)
```

    #> (Intercept)         x_1         x_2 
    #>       0.219       0.239       0.207

#### Fit and display using `stan_glm()`

``` r
set.seed(925)

fit_stan_glm <- stan_glm(y ~ x_1 + x_2, data = data, refresh = 0)

print(fit_stan_glm, digits = 2)
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      y ~ x_1 + x_2
    #>  observations: 100
    #>  predictors:   3
    #> ------
    #>             Median MAD_SD
    #> (Intercept) 1.02   0.21  
    #> x_1         1.59   0.24  
    #> x_2         2.60   0.21  
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 2.19   0.16  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

Run again with different seed to see some simulation variability.

``` r
set.seed(552)

fit_stan_glm <- stan_glm(y ~ x_1 + x_2, data = data, refresh = 0)

print(fit_stan_glm, digits = 2)
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      y ~ x_1 + x_2
    #>  observations: 100
    #>  predictors:   3
    #> ------
    #>             Median MAD_SD
    #> (Intercept) 1.02   0.22  
    #> x_1         1.58   0.25  
    #> x_2         2.59   0.20  
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 2.20   0.16  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

Extract coefficient estimates.

``` r
coef(fit_stan_glm)
```

    #> (Intercept)         x_1         x_2 
    #>        1.02        1.58        2.59

Extract coefficient uncertainties.

``` r
se(fit_stan_glm)
```

    #> (Intercept)         x_1         x_2 
    #>       0.219       0.246       0.204

#### Fit and display using tidymodels

Tidymodels is an ecosystem of R packages for modeling. It has an
advantage of providing a common interface to a growing number of [model
types and engines](https://www.tidymodels.org/find/parsnip/). It can be
used for modeling tasks such as data preprocessing, resampling, and
parameter tuning. You can learn more at:

-   [Tidymodels](https://www.tidymodels.org/)
-   [Tidy Modeling with R](https://www.tmwr.org/)

We will illustrate how to fit the above two models.

Each type of model supported by tidymodels may have multiple
computational engines. Here are the ones currently available for linear
regression.

``` r
show_engines("linear_reg")
```

    #> # A tibble: 5 x 2
    #>   engine mode      
    #>   <chr>  <chr>     
    #> 1 lm     regression
    #> 2 glmnet regression
    #> 3 stan   regression
    #> 4 spark  regression
    #> 5 keras  regression

Here’s how to fit the linear regression using `lm()`.

``` r
fit_tm_lm <- 
  linear_reg() %>% 
  set_engine("lm") %>% 
  fit(y ~ x_1 + x_2, data = data)

fit_tm_lm
```

    #> parsnip model object
    #> 
    #> Fit time:  1ms 
    #> 
    #> Call:
    #> stats::lm(formula = y ~ x_1 + x_2, data = data)
    #> 
    #> Coefficients:
    #> (Intercept)          x_1          x_2  
    #>        1.01         1.59         2.60

The tidymodels fit contains the `lm()` fit.

``` r
display(fit_tm_lm$fit)
```

    #> stats::lm(formula = y ~ x_1 + x_2, data = data)
    #>             coef.est coef.se
    #> (Intercept) 1.01     0.22   
    #> x_1         1.59     0.24   
    #> x_2         2.60     0.21   
    #> ---
    #> n = 100, k = 3
    #> residual sd = 2.19, R-Squared = 0.70

Here’s how to fit the linear regression using Stan.

``` r
set.seed(552)

fit_tm_stan <- 
  linear_reg() %>% 
  set_engine("stan") %>% 
  fit(y ~ x_1 + x_2, data = data)

fit_tm_stan
```

    #> parsnip model object
    #> 
    #> Fit time:  565ms 
    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      y ~ x_1 + x_2
    #>  observations: 100
    #>  predictors:   3
    #> ------
    #>             Median MAD_SD
    #> (Intercept) 1.0    0.2   
    #> x_1         1.6    0.2   
    #> x_2         2.6    0.2   
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 2.2    0.2   
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

The tidymodels fit contains the Stan fit.

``` r
print(fit_tm_stan$fit, digits = 2)
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      y ~ x_1 + x_2
    #>  observations: 100
    #>  predictors:   3
    #> ------
    #>             Median MAD_SD
    #> (Intercept) 1.02   0.22  
    #> x_1         1.58   0.25  
    #> x_2         2.59   0.20  
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 2.20   0.16  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

### Fitting Stan models in R using brms

#### Fit and display using brms

This will take longer as the model is not pre-compiled as in
`stan_glm()`.

``` r
set.seed(552)

fit_brm <- brm(y ~ x_1 + x_2, data = data, refresh = 0)
```

``` r
print(fit_brm, digits = 2)
```

    #>  Family: gaussian 
    #>   Links: mu = identity; sigma = identity 
    #> Formula: y ~ x_1 + x_2 
    #>    Data: data (Number of observations: 100) 
    #> Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    #>          total post-warmup samples = 4000
    #> 
    #> Population-Level Effects: 
    #>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> Intercept     1.01      0.22     0.57     1.43 1.00     4488     2991
    #> x_1           1.59      0.24     1.10     2.06 1.00     4226     2584
    #> x_2           2.60      0.21     2.19     3.01 1.00     4934     3112
    #> 
    #> Family Specific Parameters: 
    #>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> sigma     2.21      0.16     1.92     2.55 1.00     4100     3221
    #> 
    #> Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    #> and Tail_ESS are effective sample size measures, and Rhat is the potential
    #> scale reduction factor on split chains (at convergence, Rhat = 1).

Stan code generated by brms can be used to learn Stan or get a starting
point for a model which is not yet implemented in rstanarm or brms.

``` r
stancode(fit_brm)
```

    #> // generated with brms 2.14.4
    #> functions {
    #> }
    #> data {
    #>   int<lower=1> N;  // total number of observations
    #>   vector[N] Y;  // response variable
    #>   int<lower=1> K;  // number of population-level effects
    #>   matrix[N, K] X;  // population-level design matrix
    #>   int prior_only;  // should the likelihood be ignored?
    #> }
    #> transformed data {
    #>   int Kc = K - 1;
    #>   matrix[N, Kc] Xc;  // centered version of X without an intercept
    #>   vector[Kc] means_X;  // column means of X before centering
    #>   for (i in 2:K) {
    #>     means_X[i - 1] = mean(X[, i]);
    #>     Xc[, i - 1] = X[, i] - means_X[i - 1];
    #>   }
    #> }
    #> parameters {
    #>   vector[Kc] b;  // population-level effects
    #>   real Intercept;  // temporary intercept for centered predictors
    #>   real<lower=0> sigma;  // residual SD
    #> }
    #> transformed parameters {
    #> }
    #> model {
    #>   // likelihood including all constants
    #>   if (!prior_only) {
    #>     target += normal_id_glm_lpdf(Y | Xc, Intercept, b, sigma);
    #>   }
    #>   // priors including all constants
    #>   target += student_t_lpdf(Intercept | 3, 0.9, 3.3);
    #>   target += student_t_lpdf(sigma | 3, 0, 3.3)
    #>     - 1 * student_t_lccdf(0 | 3, 0, 3.3);
    #> }
    #> generated quantities {
    #>   // actual population-level intercept
    #>   real b_Intercept = Intercept - dot_product(means_X, b);
    #> }
