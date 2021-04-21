Regression and Other Stories: FakeKCV
================
Andrew Gelman, Jennifer Hill, Aki Vehtari
2021-04-20

-   [11 Assumptions, diagnostics, and model
    evaluation](#11-assumptions-diagnostics-and-model-evaluation)
    -   [11.8 Cross validation](#118-cross-validation)
        -   [Demonstration of K-fold cross validation using simulated
            data](#demonstration-of-k-fold-cross-validation-using-simulated-data)

Tidyverse version by Bill Behrman.

Demonstration of K-fold cross-validation using simulated data. See
Chapter 11 in Regression and Other Stories.

------------------------------------------------------------------------

``` r
# Packages
library(tidyverse)
library(rstanarm)

# Parameters
  # Common code
file_common <- here::here("_common.R")

#===============================================================================

# Run common code
source(file_common)
```

# 11 Assumptions, diagnostics, and model evaluation

## 11.8 Cross validation

### Demonstration of K-fold cross validation using simulated data

Simulated data. 60 × 30 matrix representing 30 predictors that are
random but not independent; rather, we draw them from a multivariate
normal distribution with correlations of 0.8.

``` r
set.seed(586)

n <- 60
k <- 30
var <- 1
cov <- 0.8
sigma <- matrix(cov, nrow = k, ncol = k)
diag(sigma) <- var
b <- c(c(-1, 1, 2), rep(0, k - 3))

data <-
  tibble(
    X = mvtnorm::rmvnorm(n, mean = rep(0, k), sigma = sigma),
    y = X %*% b + rnorm(n, mean = 0, sd = 2)
  )
```

#### Weakly informative prior

Fit linear model with weakly informative prior.

``` r
set.seed(792)

fit_1 <- 
  stan_glm(
    y ~ X,
    data = data,
    refresh = 0,
    prior = normal(location = 0, scale = 10)
  )

fit_1
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      y ~ X
    #>  observations: 60
    #>  predictors:   31
    #> ------
    #>             Median MAD_SD
    #> (Intercept)  0.0    0.4  
    #> X1          -1.4    1.2  
    #> X2           1.4    0.9  
    #> X3           1.1    1.0  
    #> X4           0.3    0.8  
    #> X5           1.1    1.1  
    #> X6           0.5    0.8  
    #> X7           0.4    0.9  
    #> X8          -1.4    1.1  
    #> X9          -2.0    1.0  
    #> X10         -0.4    1.0  
    #> X11          0.5    1.0  
    #> X12          1.7    1.0  
    #> X13          0.7    1.0  
    #> X14         -0.6    1.0  
    #> X15         -1.3    0.8  
    #> X16         -0.3    0.8  
    #> X17          0.4    0.9  
    #> X18          0.2    0.9  
    #> X19          0.6    0.8  
    #> X20          1.3    0.8  
    #> X21         -1.0    0.8  
    #> X22          0.1    1.0  
    #> X23         -0.2    0.8  
    #> X24         -0.2    1.0  
    #> X25         -0.1    0.8  
    #> X26          0.1    1.1  
    #> X27          0.1    1.0  
    #> X28         -1.0    0.9  
    #> X29          1.9    0.9  
    #> X30         -1.0    0.9  
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 2.3    0.3   
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

Perform LOO cross-validation.

``` r
loo_1 <- loo(fit_1)
```

    #> Warning: Found 17 observations with a pareto_k > 0.7. With this many problematic observations we recommend calling 'kfold' with argument 'K=10' to perform 10-fold cross-validation rather than LOO.

``` r
loo_1
```

    #> 
    #> Computed from 4000 by 60 log-likelihood matrix
    #> 
    #>          Estimate   SE
    #> elpd_loo   -156.1  5.6
    #> p_loo        28.5  3.6
    #> looic       312.3 11.2
    #> ------
    #> Monte Carlo SE of elpd_loo is NA.
    #> 
    #> Pareto k diagnostic values:
    #>                          Count Pct.    Min. n_eff
    #> (-Inf, 0.5]   (good)     17    28.3%   846       
    #>  (0.5, 0.7]   (ok)       26    43.3%   242       
    #>    (0.7, 1]   (bad)      15    25.0%   88        
    #>    (1, Inf)   (very bad)  2     3.3%   10        
    #> See help('pareto-k-diagnostic') for details.

In this case, Pareto smoothed importance sampling (PSIS) LOO fails, but
the diagnostic recognizes this with many high Pareto k values. We can
run slower, but more robust K-fold cross-validation.

``` r
kfold_1 <- kfold(fit_1, K = 10)

kfold_1
```

    #> 
    #> Based on 10-fold cross-validation
    #> 
    #>            Estimate   SE
    #> elpd_kfold   -161.4  6.1
    #> p_kfold          NA   NA
    #> kfoldic       322.9 12.2

#### An alternate weakly informative prior

The regularized horseshoe prior `hs()` is weakly informative, stating
that it is likely that only small number of predictors are relevant, but
we don’t know which ones.

Fit linear model with regularized horseshoe prior.

``` r
set.seed(792)

var_rel <- 2  # Prior guess for the number of relevant variables
global_scale <- var_rel / (k - var_rel) * 1 / sqrt(n)

fit_2 <- 
  stan_glm(
    y ~ X,
    data = data,
    refresh = 0,
    prior = 
      hs(
        df = 1,
        global_df = 1,
        global_scale = global_scale,
        slab_df = 7,
        slab_scale = 3
      )
  )

fit_2
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      y ~ X
    #>  observations: 60
    #>  predictors:   31
    #> ------
    #>             Median MAD_SD
    #> (Intercept) -0.2    0.3  
    #> X1           0.0    0.0  
    #> X2           0.0    0.1  
    #> X3           0.2    0.3  
    #> X4           0.0    0.0  
    #> X5           0.0    0.0  
    #> X6           0.0    0.0  
    #> X7           0.0    0.1  
    #> X8           0.0    0.0  
    #> X9           0.0    0.0  
    #> X10          0.0    0.0  
    #> X11          0.0    0.0  
    #> X12          0.0    0.0  
    #> X13          0.0    0.0  
    #> X14          0.0    0.1  
    #> X15          0.0    0.0  
    #> X16          0.0    0.0  
    #> X17          0.0    0.0  
    #> X18          0.0    0.1  
    #> X19          0.0    0.1  
    #> X20          0.0    0.0  
    #> X21          0.0    0.0  
    #> X22          0.0    0.0  
    #> X23          0.0    0.0  
    #> X24          0.0    0.0  
    #> X25          0.0    0.0  
    #> X26          0.0    0.0  
    #> X27          0.0    0.0  
    #> X28          0.0    0.0  
    #> X29          0.0    0.1  
    #> X30          0.0    0.0  
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 2.3    0.2   
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

Perform LOO cross-validation.

``` r
loo_2 <- loo(fit_2)

loo_2
```

    #> 
    #> Computed from 4000 by 60 log-likelihood matrix
    #> 
    #>          Estimate   SE
    #> elpd_loo   -139.0  5.1
    #> p_loo         7.0  1.1
    #> looic       278.0 10.1
    #> ------
    #> Monte Carlo SE of elpd_loo is 0.1.
    #> 
    #> Pareto k diagnostic values:
    #>                          Count Pct.    Min. n_eff
    #> (-Inf, 0.5]   (good)     58    96.7%   1128      
    #>  (0.5, 0.7]   (ok)        2     3.3%   2261      
    #>    (0.7, 1]   (bad)       0     0.0%   <NA>      
    #>    (1, Inf)   (very bad)  0     0.0%   <NA>      
    #> 
    #> All Pareto k estimates are ok (k < 0.7).
    #> See help('pareto-k-diagnostic') for details.

Perform K-fold cross-validation.

``` r
kfold_2 <- kfold(fit_2, K = 10)

kfold_2
```

    #> 
    #> Based on 10-fold cross-validation
    #> 
    #>            Estimate   SE
    #> elpd_kfold   -138.6  5.0
    #> p_kfold          NA   NA
    #> kfoldic       277.2 10.1

#### Comparison of models

``` r
loo_compare(loo_1, loo_2)
```

    #>       elpd_diff se_diff
    #> fit_2   0.0       0.0  
    #> fit_1 -17.1       4.1

``` r
loo_compare(kfold_1, kfold_2)
```

    #>       elpd_diff se_diff
    #> fit_2   0.0       0.0  
    #> fit_1 -22.8       4.4

As PSIS-LOO fails, PSIS-LOO comparison underestimates the difference
between the models. The Pareto k diagnostic correctly identified the
problem, and more robust K-fold cross-validation shows that by using a
better prior we can get better predictions.
