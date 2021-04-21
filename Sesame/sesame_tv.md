Regression and Other Stories: Sesame street
================
Andrew Gelman, Jennifer Hill, Aki Vehtari
2021-04-20

-   [18 Causal inference and randomized
    experiments](#18-causal-inference-and-randomized-experiments)
    -   [18.8 Exercises](#188-exercises)
-   [21 Additional topics in causal
    inference](#21-additional-topics-in-causal-inference)
    -   [21.1 Estimating causal effects indirectly using instrumental
        variables](#211-estimating-causal-effects-indirectly-using-instrumental-variables)
        -   [Instrumental variables estimate: Sesame
            Street](#instrumental-variables-estimate-sesame-street)
    -   [21.2 Instrumental variables in a regression
        framework](#212-instrumental-variables-in-a-regression-framework)
        -   [Two-stage least squares: Sesame
            Street](#two-stage-least-squares-sesame-street)
        -   [Adjusting for covariates in an instrument variables
            framework](#adjusting-for-covariates-in-an-instrument-variables-framework)
        -   [Standard errors for instrumental variables
            estimates](#standard-errors-for-instrumental-variables-estimates)
        -   [Performing two-stage least squares automatically using
            brms](#performing-two-stage-least-squares-automatically-using-brms)

Tidyverse version by Bill Behrman.

Causal analysis of Sesame Street experiment. See Chapters 18 and 21 in
Regression and Other Stories.

------------------------------------------------------------------------

``` r
# Packages
library(tidyverse)
library(brms)
library(rstanarm)

# Parameters
  # Seed
SEED <- 1234
  # Results of educational experiment
file_sesame <- here::here("Sesame/data/sesame.csv")
  # Common code
file_common <- here::here("_common.R")

#===============================================================================

# Run common code
source(file_common)
```

# 18 Causal inference and randomized experiments

## 18.8 Exercises

Data

``` r
sesame <- 
  file_sesame %>% 
  read_csv() %>% 
  mutate(
    site =
      case_when(
        `_Isite_2` == 1 ~ 2,
        `_Isite_3` == 1 ~ 3,
        `_Isite_4` == 1 ~ 4,
        `_Isite_5` == 1 ~ 5,
        TRUE ~ 1
      ) %>% 
      factor()
  ) %>% 
  relocate(site, .before = `_Isite_2`)

glimpse(sesame)
```

    #> Rows: 240
    #> Columns: 32
    #> $ rownames   <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, …
    #> $ id         <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, …
    #> $ sex        <dbl> 1, 2, 1, 1, 1, 2, 2, 1, 1, 2, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1,…
    #> $ age        <dbl> 66, 67, 56, 49, 69, 54, 47, 51, 69, 53, 58, 58, 49, 64, 58,…
    #> $ viewcat    <dbl> 1, 3, 3, 1, 4, 3, 3, 2, 4, 3, 2, 4, 1, 2, 2, 3, 2, 4, 3, 3,…
    #> $ setting    <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
    #> $ viewenc    <dbl> 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2,…
    #> $ prebody    <dbl> 16, 30, 22, 23, 32, 29, 23, 32, 27, 30, 25, 21, 28, 26, 23,…
    #> $ prelet     <dbl> 23, 26, 14, 11, 47, 26, 12, 48, 44, 38, 48, 25, 8, 11, 15, …
    #> $ preform    <dbl> 12, 9, 9, 10, 15, 10, 11, 19, 18, 17, 14, 13, 9, 15, 9, 17,…
    #> $ prenumb    <dbl> 40, 39, 9, 14, 51, 33, 13, 52, 42, 31, 38, 29, 13, 21, 16, …
    #> $ prerelat   <dbl> 14, 16, 9, 9, 17, 14, 11, 15, 15, 10, 16, 16, 8, 10, 9, 12,…
    #> $ preclasf   <dbl> 20, 22, 8, 13, 22, 14, 12, 23, 20, 17, 18, 21, 12, 15, 11, …
    #> $ postbody   <dbl> 18, 30, 21, 21, 32, 27, 22, 31, 32, 32, 26, 17, 20, 26, 28,…
    #> $ postlet    <dbl> 30, 37, 46, 14, 63, 36, 45, 47, 50, 52, 52, 29, 16, 28, 21,…
    #> $ postform   <dbl> 14, 17, 15, 13, 18, 14, 12, 18, 17, 19, 15, 15, 9, 15, 10, …
    #> $ postnumb   <dbl> 44, 39, 40, 19, 54, 39, 44, 51, 48, 52, 42, 40, 18, 35, 22,…
    #> $ postrelat  <dbl> 14, 14, 9, 8, 14, 16, 12, 17, 14, 17, 10, 10, 10, 16, 10, 1…
    #> $ postclasf  <dbl> 23, 22, 19, 15, 21, 24, 15, 23, 24, 24, 17, 19, 13, 14, 17,…
    #> $ peabody    <dbl> 62, 8, 32, 27, 71, 32, 28, 38, 49, 32, 43, 58, 39, 43, 56, …
    #> $ agecat     <dbl> 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1,…
    #> $ encour     <dbl> 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0,…
    #> $ site       <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    #> $ `_Isite_2` <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    #> $ `_Isite_3` <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    #> $ `_Isite_4` <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    #> $ `_Isite_5` <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    #> $ regular    <dbl> 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,…
    #> $ watched    <dbl> 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,…
    #> $ encouraged <dbl> 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0,…
    #> $ y          <dbl> 30, 37, 46, 14, 63, 36, 45, 47, 50, 52, 52, 29, 16, 28, 21,…
    #> $ pretest    <dbl> 23, 26, 14, 11, 47, 26, 12, 48, 44, 38, 48, 25, 8, 11, 15, …

Let’s check the derived variable `site`.

``` r
sesame %>% 
  count(site, across(starts_with("_Isite")))
```

    #> # A tibble: 5 x 6
    #>   site  `_Isite_2` `_Isite_3` `_Isite_4` `_Isite_5`     n
    #>   <fct>      <dbl>      <dbl>      <dbl>      <dbl> <int>
    #> 1 1              0          0          0          0    60
    #> 2 2              1          0          0          0    55
    #> 3 3              0          1          0          0    64
    #> 4 4              0          0          1          0    43
    #> 5 5              0          0          0          1    18

# 21 Additional topics in causal inference

## 21.1 Estimating causal effects indirectly using instrumental variables

### Instrumental variables estimate: Sesame Street

Let’s first look at the effect of encouraging children to watch Sesame
Street.

``` r
v <- 
  sesame %>% 
  count(encouraged, watched) %>% 
  group_by(encouraged) %>% 
  mutate(watched_prop = n / sum(n)) %>% 
  ungroup()

v
```

    #> # A tibble: 4 x 4
    #>   encouraged watched     n watched_prop
    #>        <dbl>   <dbl> <int>        <dbl>
    #> 1          0       0    40       0.455 
    #> 2          0       1    48       0.545 
    #> 3          1       0    14       0.0921
    #> 4          1       1   138       0.908

Without any encouragement, 55% of children still watched Sesame Street.
With encouragement, that percentage increased to 91%

We now calculate an estimate of the effect of watching Sesame Street for
the children who received encouragement and who in turn actually watched
the show, the compliers.

We first estimate the percentage of children actually induced to watch
Sesame Street by the intervention, which is the coefficient on the
instrument (`encouraged`), in the following regression:

``` r
set.seed(264)

fit_1a <- stan_glm(watched ~ encouraged, data = sesame, refresh = 0)

print(fit_1a, digits = 2)
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      watched ~ encouraged
    #>  observations: 240
    #>  predictors:   2
    #> ------
    #>             Median MAD_SD
    #> (Intercept) 0.55   0.04  
    #> encouraged  0.36   0.05  
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 0.38   0.02  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

The estimated coefficient of `encouraged` here is 0.36. This is simply
the difference in the proportion of children who watched Sesame Street
when encouraged vs. when not encouraged:

``` r
v %>% 
  filter(watched == 1) %>% 
  summarize(
    watched_diff = watched_prop[encouraged == 1] - watched_prop[encouraged == 0]
  )
```

    #> # A tibble: 1 x 1
    #>   watched_diff
    #>          <dbl>
    #> 1        0.362

We then compute the intent-to-treat estimate, obtained in this case
using the regression of the outcome (the *post*-treatment measurement of
the *letter* recognition task, `postlet`) on the instrument:

``` r
set.seed(264)

fit_1b <- stan_glm(postlet ~ encouraged, data = sesame, refresh = 0)

print(fit_1b, digits = 2)
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      postlet ~ encouraged
    #>  observations: 240
    #>  predictors:   2
    #> ------
    #>             Median MAD_SD
    #> (Intercept) 24.92   1.42 
    #> encouraged   2.90   1.82 
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 13.36   0.60 
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

The estimated coefficient of `encouraged` in this regression is 2.9,
which we then “inflate” by dividing by the percentage of children
affected by the intervention:

``` r
wald_est <- coef(fit_1b)[["encouraged"]] / coef(fit_1a)[["encouraged"]]

wald_est
```

    #> [1] 7.95

This ratio is sometimes called the *Wald estimate*.

## 21.2 Instrumental variables in a regression framework

### Two-stage least squares: Sesame Street

The first step is to regress the “treatment” variable – an indicator for
regular watching (`watched`) – on the randomized instrument,
encouragement to watch (`encouraged`). This is the same as `fit_1a`
above, but we run again here and relabel to indicate that in this
framework it is our first-stage model.

``` r
set.seed(264)

fit_2a <- stan_glm(watched ~ encouraged, data = sesame, refresh = 0)

print(fit_2a, digits = 2)
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      watched ~ encouraged
    #>  observations: 240
    #>  predictors:   2
    #> ------
    #>             Median MAD_SD
    #> (Intercept) 0.55   0.04  
    #> encouraged  0.36   0.05  
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 0.38   0.02  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

Add the predicted values for `watched`.

``` r
sesame <- 
  sesame %>% 
  mutate(watched_pred_2a = predict(fit_2a))

summary(sesame$watched_pred_2a)
```

    #>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    #>   0.546   0.546   0.910   0.777   0.910   0.910

The second step is to regress the letter recognition outcome on the
predicted values for `watched`.

``` r
set.seed(264)

fit_2b <- stan_glm(postlet ~ watched_pred_2a, data = sesame, refresh = 0)

print(fit_2b, digits = 2)
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      postlet ~ watched_pred_2a
    #>  observations: 240
    #>  predictors:   2
    #> ------
    #>                 Median MAD_SD
    #> (Intercept)     20.58   3.96 
    #> watched_pred_2a  7.95   5.01 
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 13.36   0.60 
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

Here the coefficient of `watched_pred_2a` is the effect of watching
Sesame Street on letter recognition for those who would watch if
encouraged but not otherwise (compliers). This second-stage regression
does not give the correct standard error, however, as we discuss below.

### Adjusting for covariates in an instrument variables framework

The randomization for this particular experiment took place within sites
and settings; it is therefore appropriate to adjust for these covariates
in estimating the treatment effect. Additionally, pre-treatment scores
are available that are highly predictive of post-test scores.

The first step of two-stage approach adjusting for these covariates:

``` r
set.seed(264)

fit_3a <- 
  stan_glm(
    watched ~ encouraged + prelet + site + setting, 
    data = sesame, 
    refresh = 0
  )

print(fit_3a, digits = 2)
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      watched ~ encouraged + prelet + site + setting
    #>  observations: 240
    #>  predictors:   8
    #> ------
    #>             Median MAD_SD
    #> (Intercept)  0.66   0.11 
    #> encouraged   0.34   0.05 
    #> prelet       0.01   0.00 
    #> site2        0.03   0.07 
    #> site3       -0.12   0.06 
    #> site4       -0.34   0.07 
    #> site5       -0.30   0.10 
    #> setting     -0.05   0.05 
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 0.35   0.02  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

Add the predicted values for `watched`.

``` r
sesame <- 
  sesame %>% 
  mutate(watched_pred_3a = predict(fit_3a))

summary(sesame$watched_pred_3a)
```

    #>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    #>   0.216   0.638   0.806   0.775   0.973   1.147

The second step is to regress the letter recognition outcome on the
predicted values for `watched`.

``` r
set.seed(264)

fit_3b <-
  stan_glm(
    postlet ~ watched_pred_3a + prelet + site + setting, 
    data = sesame, 
    refresh = 0
  )

print(fit_3b, digits = 2)
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      postlet ~ watched_pred_3a + prelet + site + setting
    #>  observations: 240
    #>  predictors:   8
    #> ------
    #>                 Median MAD_SD
    #> (Intercept)      1.21   4.90 
    #> watched_pred_3a 14.08   4.12 
    #> prelet           0.70   0.08 
    #> site2            8.41   1.91 
    #> site3           -3.86   1.82 
    #> site4            0.98   2.55 
    #> site5            2.88   3.01 
    #> setting          1.65   1.50 
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 9.70   0.46  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

The estimated effect of watching Sesame Street on the compliers is about
14.1 points on the letter recognition test. Again, we do not trust this
standard error and will discuss next how to appropriately adjust it for
the two stages of estimation.

### Standard errors for instrumental variables estimates

We show here how to adjust the standard error to account for the
uncertainty in both stages of the model.

``` r
resid <- residuals(fit_3b)
x_adj <- model.matrix(fit_3b)
x_adj[, "watched_pred_3a"] <- sesame$watched
resid_adj <- sesame$postlet - x_adj %*% coef(fit_3b)
se_adj <- 
  se(fit_3b)[["watched_pred_3a"]] * sqrt(sum(resid^2) / sum(resid_adj^2))

se_adj
```

    #> [1] 4.29

In this example, the resulting standard error of 4.29 is slightly larger
than the unadjusted standard error of 4.12.

### Performing two-stage least squares automatically using brms

It is also possible to perform a Bayesian version of the two-stage least
squares model directly using brms. Here is how to estimate the effect of
regularly watching Sesame Street on post-treatment letter recognition
using encouragement as an instrument:

``` r
set.seed(264)

fit_brm_1 <- 
  brm(
    formula = bf(watched ~ encouraged) + bf(postlet ~ watched),
    data = sesame,
    refresh = 0,
    seed = SEED
  )
```

``` r
fit_brm_1
```

    #>  Family: MV(gaussian, gaussian) 
    #>   Links: mu = identity; sigma = identity
    #>          mu = identity; sigma = identity 
    #> Formula: watched ~ encouraged 
    #>          postlet ~ watched 
    #>    Data: sesame (Number of observations: 240) 
    #> Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    #>          total post-warmup samples = 4000
    #> 
    #> Population-Level Effects: 
    #>                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> watched_Intercept      0.55      0.04     0.47     0.63 1.00     4710     3005
    #> postlet_Intercept     20.54      3.88    13.27    28.58 1.00     2100     2038
    #> watched_encouraged     0.36      0.05     0.25     0.46 1.00     4476     2649
    #> postlet_watched        7.97      4.88    -2.01    17.08 1.00     2074     1850
    #> 
    #> Family Specific Parameters: 
    #>               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> sigma_watched     0.38      0.02     0.35     0.42 1.00     4569     2616
    #> sigma_postlet    12.67      0.72    11.45    14.22 1.00     2713     2204
    #> 
    #> Residual Correlations: 
    #>                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    #> rescor(watched,postlet)     0.17      0.15    -0.13     0.45 1.00     1960
    #>                         Tail_ESS
    #> rescor(watched,postlet)     1782
    #> 
    #> Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    #> and Tail_ESS are effective sample size measures, and Rhat is the potential
    #> scale reduction factor on split chains (at convergence, Rhat = 1).

The resulting estimate is the coefficient `postlet_watched`, which is
the instrumental variables estimate of `watched` on `postlet`; the
estimate is 8.0 with a standard error of 4.9.

To incorporate other pre-treatment variables as controls, we must
include them in both stages of the regression model; for example,

``` r
set.seed(264)

fit_brm_2 <- 
  brm(
    formula = 
      bf(watched ~ encouraged + prelet + setting + site) +
      bf(postlet ~ watched + prelet + setting + site),
    data = sesame,
    refresh = 0,
    seed = SEED
  )
```

``` r
fit_brm_2
```

    #>  Family: MV(gaussian, gaussian) 
    #>   Links: mu = identity; sigma = identity
    #>          mu = identity; sigma = identity 
    #> Formula: watched ~ encouraged + prelet + setting + site 
    #>          postlet ~ watched + prelet + setting + site 
    #>    Data: sesame (Number of observations: 240) 
    #> Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    #>          total post-warmup samples = 4000
    #> 
    #> Population-Level Effects: 
    #>                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> watched_Intercept      0.67      0.11     0.46     0.88 1.00     3413     2919
    #> postlet_Intercept      1.27      4.61    -8.34     9.83 1.00     1862     2427
    #> watched_encouraged     0.34      0.05     0.24     0.43 1.00     4354     2875
    #> watched_prelet         0.01      0.00    -0.00     0.01 1.00     6814     3278
    #> watched_setting       -0.06      0.05    -0.16     0.04 1.00     4252     2898
    #> watched_site2          0.03      0.07    -0.10     0.16 1.00     3267     2949
    #> watched_site3         -0.11      0.07    -0.24     0.02 1.00     3157     2642
    #> watched_site4         -0.34      0.07    -0.49    -0.20 1.00     3658     2951
    #> watched_site5         -0.29      0.10    -0.49    -0.10 1.00     3346     2950
    #> postlet_watched       13.95      3.87     6.38    21.99 1.00     1854     2275
    #> postlet_prelet         0.70      0.08     0.55     0.85 1.00     5049     3039
    #> postlet_setting        1.59      1.41    -1.11     4.44 1.00     3220     2947
    #> postlet_site2          8.43      1.81     4.85    11.95 1.00     3998     3027
    #> postlet_site3         -3.93      1.79    -7.44    -0.38 1.00     3208     2610
    #> postlet_site4          0.90      2.40    -3.75     5.60 1.00     2395     2710
    #> postlet_site5          2.75      2.83    -2.77     8.23 1.00     2591     2890
    #> 
    #> Family Specific Parameters: 
    #>               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> sigma_watched     0.36      0.02     0.33     0.39 1.00     6369     3046
    #> sigma_postlet     9.44      0.53     8.51    10.58 1.00     3043     2262
    #> 
    #> Residual Correlations: 
    #>                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    #> rescor(watched,postlet)    -0.18      0.15    -0.47     0.12 1.00     1864
    #>                         Tail_ESS
    #> rescor(watched,postlet)     2174
    #> 
    #> Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    #> and Tail_ESS are effective sample size measures, and Rhat is the potential
    #> scale reduction factor on split chains (at convergence, Rhat = 1).

The resulting estimate is the coefficient `postlet_watched`, which is
the instrumental variables estimate of `watched` on `postlet`; the
estimate is 14.0 with a standard error of 3.9.
