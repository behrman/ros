Regression and Other Stories: Logistic regression priors
================
Andrew Gelman, Jennifer Hill, Aki Vehtari
2021-04-20

-   [13 Logistic regression](#13-logistic-regression)
    -   [13.5 Maximum likelihood and Bayesion inference for logistic
        regression](#135-maximum-likelihood-and-bayesion-inference-for-logistic-regression)
        -   [Comparing maximum likelihood and Bayesian inference using a
            simulation
            study](#comparing-maximum-likelihood-and-bayesian-inference-using-a-simulation-study)

Tidyverse version by Bill Behrman.

Effect of priors in logistic regression. See Chapter 13 in Regression
and Other Stories.

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

# 13 Logistic regression

## 13.5 Maximum likelihood and Bayesion inference for logistic regression

### Comparing maximum likelihood and Bayesian inference using a simulation study

Define a function to run `glm()` and `stan_glm()` with simulated data.
The arguments are the number of simulated observations and the true
parameter values.

``` r
bayes_sim <- function(n, a = -2, b = 0.8) {
  data <- 
    tibble(
      x = runif(n, min = -1, max = 1),
      y = if_else(0 < rlogis(n, location = a + b * x, scale = 1), 1, 0)
    )
  
  fit_glm <- glm(y ~ x, family = binomial(link = "logit"), data = data)
  fit_stan <- 
    stan_glm(
      y ~ x,
      family = binomial(link = "logit"),
      data = data,
      refresh = 0,
      prior = normal(location = 0.5, scale = 0.5)
    )
  
  arm::display(fit_glm, digits = 1)
  cat("\n")
  print(fit_stan, digits = 1)
}
```

We next simulate for a range of sample sizes, each time focusing on
inference about b, for which a value of 0.8 was used to generate the
data.

#### n = 10

``` r
set.seed(363852)

bayes_sim(10)
```

    #> glm(formula = y ~ x, family = binomial(link = "logit"), data = data)
    #>             coef.est coef.se
    #> (Intercept) -1.9      1.2   
    #> x            1.5      1.7   
    #> ---
    #>   n = 10, k = 2
    #>   residual deviance = 8.9, null deviance = 10.0 (difference = 1.1)
    #> 
    #> stan_glm
    #>  family:       binomial [logit]
    #>  formula:      y ~ x
    #>  observations: 10
    #>  predictors:   2
    #> ------
    #>             Median MAD_SD
    #> (Intercept) -1.5    0.8  
    #> x            0.6    0.4  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

We shall focus on the coefficient of x, which represents the parameter
of interest in this hypothetical study. In the above simulation, `glm()`
gives a maximum likelihood estimate of 1.5, which is far from the
specified belief that b is likely to be in the range (0, 1) – but that
estimate also has a large standard error, indicating that the likelihood
provides little information in this n = 10 setting. In contrast, the
inference from `stan_glm()` relies heavily on the prior distribution:
the Bayes estimate of 0.6 is close to the prior mean of 0.5, being
pulled away by the data only slightly.

#### n = 100

``` r
bayes_sim(100)
```

    #> glm(formula = y ~ x, family = binomial(link = "logit"), data = data)
    #>             coef.est coef.se
    #> (Intercept) -2.7      0.4   
    #> x            1.2      0.7   
    #> ---
    #>   n = 100, k = 2
    #>   residual deviance = 47.7, null deviance = 50.7 (difference = 3.0)
    #> 
    #> stan_glm
    #>  family:       binomial [logit]
    #>  formula:      y ~ x
    #>  observations: 100
    #>  predictors:   2
    #> ------
    #>             Median MAD_SD
    #> (Intercept) -2.6    0.4  
    #> x            0.7    0.4  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

The maximum likelihood estimate is again extreme, but less so than
before, and the Bayes estimate is again pulled toward the prior mean of
0.5, but less so than before. This is just one realization of the
process, and another random simulation will give different results, but
it illustrates the general pattern of the Bayesian posterior estimate
being a compromise between data and prior.

#### n = 1000

``` r
bayes_sim(1000)
```

    #> glm(formula = y ~ x, family = binomial(link = "logit"), data = data)
    #>             coef.est coef.se
    #> (Intercept) -2.1      0.1   
    #> x            1.0      0.2   
    #> ---
    #>   n = 1000, k = 2
    #>   residual deviance = 723.8, null deviance = 757.4 (difference = 33.6)
    #> 
    #> stan_glm
    #>  family:       binomial [logit]
    #>  formula:      y ~ x
    #>  observations: 1000
    #>  predictors:   2
    #> ------
    #>             Median MAD_SD
    #> (Intercept) -2.0    0.1  
    #> x            0.9    0.2  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

The Bayes estimate is only slightly pooled toward the prior mean. In
this particular example, once n is as large as 1000, the prior
distribution doesn’t really make a difference.
