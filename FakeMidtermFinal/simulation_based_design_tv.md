Regression and Other Stories: Fake dataset of a randomized experiment on
student grades
================
Andrew Gelman, Jennifer Hill, Aki Vehtari
2021-04-20

-   [16 Design and sample size
    decisions](#16-design-and-sample-size-decisions)
    -   [16.6 Design analysis using fake-data
        simulation](#166-design-analysis-using-fake-data-simulation)
        -   [Simulating a randomized
            experiment](#simulating-a-randomized-experiment)
        -   [Including a pre-treatment
            predictor](#including-a-pre-treatment-predictor)
        -   [Simulating an experiment with selection
            bias](#simulating-an-experiment-with-selection-bias)

Tidyverse version by Bill Behrman.

Fake dataset of a randomized experiment on student grades. See Chapter
16 in Regression and Other Stories.

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

# 16 Design and sample size decisions

## 16.6 Design analysis using fake-data simulation

### Simulating a randomized experiment

Data

``` r
set.seed(862)

sim_1 <- function(n = 100) {
  y_if_control <- rnorm(n, mean = 60, sd = 20)
  y_if_treated <- y_if_control + 5
  tibble(
    z = rep(0:1, n / 2) %>% sample(),
    y = if_else(z == 1, y_if_treated, y_if_control)
  )
}

data_1a1 <- sim_1()
```

Having simulated the data, we can now compare treated to control
outcomes and compute the standard error for the difference.

``` r
data_1a1 %>% 
  summarize(
    diff = mean(y[z == 1]) - mean(y[z == 0]),
    diff_se = 
      sqrt(sd(y[z == 0])^2 / sum(z == 0) + sd(y[z == 1])^2 / sum(z == 1))
  )
```

    #> # A tibble: 1 x 2
    #>    diff diff_se
    #>   <dbl>   <dbl>
    #> 1  4.66    3.80

Equivalently, we can run the regression:

``` r
set.seed(619)

fit_1a1 <- stan_glm(y ~ z, data = data_1a1, refresh = 0)

fit_1a1
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      y ~ z
    #>  observations: 100
    #>  predictors:   2
    #> ------
    #>             Median MAD_SD
    #> (Intercept) 59.1    2.8  
    #> z            4.6    3.9  
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 19.0    1.3  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

To give a sense of why it would be a mistake to focus on the point
estimate, we repeat the above steps for a new batch of 100 students
simulated from the model. Here is the result:

``` r
set.seed(827)

data_1a2 <- sim_1()
```

``` r
set.seed(619)

fit_1a2 <- stan_glm(y ~ z, data = data_1a2, refresh = 0)

fit_1a2
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      y ~ z
    #>  observations: 100
    #>  predictors:   2
    #> ------
    #>             Median MAD_SD
    #> (Intercept) 56.3    2.6  
    #> z           12.2    3.7  
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 17.8    1.3  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

A naive read of this table would be that the design with 100 students is
just fine, as the estimate is well over two standard errors away from
zero. But that conclusion would be a mistake, as the coefficient
estimate here is too noisy to be useful.

### Including a pre-treatment predictor

Add a pre-test variable simulated independently of the potential
outcomes for the final test score.

``` r
set.seed(134)

data_1b <- 
  data_1a1 %>% 
  mutate(x = rnorm(n(), mean = 50, sd = 20))
```

We can then adjust for pre-test in our regression:

``` r
set.seed(619)

fit_1b <- stan_glm(y ~ z + x, data = data_1b, refresh = 0)

fit_1b
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      y ~ z + x
    #>  observations: 100
    #>  predictors:   3
    #> ------
    #>             Median MAD_SD
    #> (Intercept) 62.2    5.0  
    #> z            4.4    3.9  
    #> x           -0.1    0.1  
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 19.1    1.3  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

Because the pre-test variable was simulated independently of the
potential outcomes for the final test score, the standard error for the
coefficient of `z` wasn’t reduced.

To perform a realistic simulation, we will now simulate both test scores
in a correlated way.

``` r
set.seed(822)

n <- 100

true_ability <- rnorm(n, mean = 50, sd = 16)
y_if_control <- true_ability + rnorm(n, mean = 0, sd = 12) + 10
y_if_treated <- y_if_control + 5

data_2 <- 
  tibble(
    x = true_ability + rnorm(n, mean = 0, sd = 12),
    z = rep(0:1, n / 2) %>% sample(),
    y = if_else(z == 1, y_if_treated, y_if_control)
  ) 
```

The simple comparison is equivalent to a regression on the treatment
indicator:

``` r
set.seed(619)

fit_2a <- stan_glm(y ~ z, data = data_2, refresh = 0)

fit_2a
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      y ~ z
    #>  observations: 100
    #>  predictors:   2
    #> ------
    #>             Median MAD_SD
    #> (Intercept) 59.7    3.2  
    #> z            6.4    4.6  
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 22.7    1.6  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

And the estimate adjusting for pre-test:

``` r
set.seed(619)

fit_2b <- stan_glm(y ~ z + x, data = data_2, refresh = 0)

fit_2b
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      y ~ z + x
    #>  observations: 100
    #>  predictors:   3
    #> ------
    #>             Median MAD_SD
    #> (Intercept) 20.6    5.0  
    #> z            7.4    3.5  
    #> x            0.8    0.1  
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 17.1    1.2  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

In this case, with a strong dependence between pre-test and post-test,
this adjustment has reduced the residual standard deviation by about a
quarter.

### Simulating an experiment with selection bias

Unbiased treatment assignments.

``` r
curve <- tibble(x = c(0, 100), prob = 0.5)

data_2 %>% 
  ggplot(aes(x)) +
  stat_ydensity(
    aes(y = z, group = z),
    width = 0.25,
    draw_quantiles = c(0.25, 0.5, 0.75),
    scale = "count"
  ) +
  geom_line(aes(y = prob), data = curve) +
  coord_cartesian(ylim = c(-0.125, 1.125)) +
  scale_y_continuous(breaks = scales::breaks_width(0.25)) +
  labs(
    title = "Unbiased treatment assignments",
    subtitle = "Violin plots represent density for treatment and control",
    x = "Pre-test score",
    y = "Probability that z = 1"
  )
```

<img src="simulation_based_design_tv_files/figure-gfm/unnamed-chunk-12-1.png" width="100%" />

We will now simulate bias in the treatment assignment.

``` r
set.seed(277)

sim_3 <- function(n = 100) {
  true_ability <- rnorm(n, mean = 50, sd = 16)
  y_if_control <- true_ability + rnorm(n, mean = 0, sd = 12) + 10
  y_if_treated <- y_if_control + 5
  tibble(
    x = true_ability + rnorm(n, mean = 0, sd = 12),
    z = rbinom(n, size = 1, prob = plogis(-(x - 50) / 20)),
    y = if_else(z == 1, y_if_treated, y_if_control)
  )
}

data_3 <- sim_3()
```

Biased treatment assignments.

``` r
curve <- 
  tibble(
    x = seq_range(c(0, 100)),
    prob = plogis(-(x - 50) / 20)
  )
  
data_3 %>% 
  ggplot(aes(x)) +
  stat_ydensity(
    aes(y = z, group = z),
    width = 0.25,
    draw_quantiles = c(0.25, 0.5, 0.75),
    scale = "count"
  ) +
  geom_line(aes(y = prob), data = curve) +
  coord_cartesian(ylim = c(-0.125, 1.125)) +
  scale_y_continuous(breaks = scales::breaks_width(0.25)) +
  labs(
    title = "Biased treatment assignments",
    subtitle = "Violin plots represent density for treatment and control",
    x = "Pre-test score",
    y = "Probability that z = 1"
  )
```

<img src="simulation_based_design_tv_files/figure-gfm/unnamed-chunk-14-1.png" width="100%" />

A function to simulate the data with biased treatment assignments and
perform the simple comparison and the regression adjusting for pre-test:

``` r
experiment <- function(n = 100) {
  data_3 <- sim_3(n)
  fit_3a <- stan_glm(y ~ z, data = data_3, refresh = 0)
  fit_3b <- stan_glm(y ~ z + x, data = data_3, refresh = 0)
  tibble(
    simple = coef(fit_3a)[["z"]],
    simple_se = se(fit_3a)[["z"]],
    adjusted = coef(fit_3b)[["z"]],
    adjusted_se = se(fit_3b)[["z"]] 
  )
}
```

``` r
n_sims <- 50
```

Run the simulation 50 times.

``` r
set.seed(844)

results_mean <- 
  seq_len(n_sims) %>% 
  map_dfr(~ experiment()) %>% 
  summarize(across(everything(), mean))

matrix(
  as.double(results_mean),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(c("Simple", "Adjusted"), c("Estimate", "SE"))
)
```

             Estimate   SE
    Simple      -5.44 3.86
    Adjusted     5.13 3.38

The true parameter value here is 5.0, so in this case the simple
comparison is horribly biased – no surprise if you reflect upon the big
differences between treatment and control groups. In contrast, the bias
of the adjusted estimate is low.
