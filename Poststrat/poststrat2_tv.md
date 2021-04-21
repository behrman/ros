Regression and Other Stories: Poststratification 2
================
Andrew Gelman, Jennifer Hill, Aki Vehtari
2021-04-20

-   [17 Poststratification and missing-data
    imputation](#17-poststratification-and-missing-data-imputation)
    -   [17.2 Fake-data simulation for regression and
        poststratification](#172-fake-data-simulation-for-regression-and-poststratification)
        -   [Creating the artificial
            world](#creating-the-artificial-world)
        -   [Performing regression and
            poststratification](#performing-regression-and-poststratification)

Tidyverse version by Bill Behrman.

Demonstrate poststratification with simulated census and poll data. See
Chapter 17 in Regression and Other Stories.

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

# 17 Poststratification and missing-data imputation

## 17.2 Fake-data simulation for regression and poststratification

### Creating the artificial world

Simulation parameters:

-   `pop_prop`: Proportion of the population
-   `response`: Response rate relative to `response_baseline`
-   `coef`: Coefficient of formula for probability of “Yes” response to
    survey

``` r
pop <- 250e6
response_baseline <- 0.1
coef_intercept <- 0.6

params <- 
  tribble(
    ~var,        ~value,     ~pop_prop, ~response, ~coef,
    "sex",       "Female",        0.52,       1.0,     0,
    "sex",       "Male",          0.48,       0.8,  -0.2,
    "age",       "18 - 29",       0.20,       1.0,     0,
    "age",       "30 - 44",       0.25,       1.2,  -0.2,
    "age",       "45 - 64",       0.30,       1.6,  -0.3,
    "age",       "65+",           0.25,       2.5,  -0.4,
    "ethnicity", "White",         0.70,       1.0,     0,
    "ethnicity", "Black",         0.10,       0.8,   0.6,
    "ethnicity", "Hispanic",      0.10,       0.7,   0.3,
    "ethnicity", "Other",         0.10,       0.6,   0.3
  )
```

Function to return simulation parameter.

``` r
param <- function(var_, value_, param) {
  params %>% 
    filter(var == var_, value == value_) %>% 
    pull({{param}})
}

param("sex", "Female", pop_prop)
```

    #> [1] 0.52

Poststratification cells with assumed population, response rate, and
probability of “Yes” response to survey.

``` r
poststrat <- 
  expand_grid(
    sex = c("Female", "Male"),
    age = c("18 - 29", "30 - 44", "45 - 64", "65+"),
    ethnicity = c("White", "Black", "Hispanic", "Other")
  ) %>% 
  mutate(
    across(c(sex, age, ethnicity), fct_inorder),
    n = 
      pmap_dbl(
        list(sex, age, ethnicity),
        ~ pop * param("sex", ..1, pop_prop) * param("age", ..2, pop_prop) *
          param("ethnicity", ..3, pop_prop)
      ),
    response =
      pmap_dbl(
        list(sex, age, ethnicity),
        ~ response_baseline * param("sex", ..1, response) *
          param("age", ..2, response) * param("ethnicity", ..3, response)
      ),
    yes_prob =
      pmap_dbl(
        list(sex, age, ethnicity),
        ~ plogis(
          coef_intercept + param("sex", ..1, coef) + param("age", ..2, coef) +
            param("ethnicity", ..3, coef)
        )
      )
  )
```

We then sample from the assumed population with the assumed response
rate.

``` r
set.seed(457)

n_people <- 1000

people <- 
  sample(
    nrow(poststrat),
    size = n_people,
    replace = TRUE, 
    prob = poststrat$n * poststrat$response
  )
```

Check that each cell was sampled.

``` r
setequal(seq_len(nrow(poststrat)), people)
```

    #> [1] TRUE

Add proportion of population and proportion of sample for each cell in
poststratification table.

``` r
poststrat <- 
  poststrat %>% 
  mutate(
    n_prop = n / sum(n),
    cell = row_number()
  ) %>% 
  left_join(
    tibble(cell = people) %>% count(cell, name = "n_sample"),
    by = "cell"
  ) %>% 
  mutate(sample_prop = n_sample / n_people) %>% 
  select(!c(cell, n_sample))

poststrat %>% 
  knitr::kable()
```

| sex    | age     | ethnicity |        n | response | yes\_prob | n\_prop | sample\_prop |
|:-------|:--------|:----------|---------:|---------:|----------:|--------:|-------------:|
| Female | 18 - 29 | White     | 18200000 |    0.100 |     0.646 |   0.073 |        0.049 |
| Female | 18 - 29 | Black     |  2600000 |    0.080 |     0.769 |   0.010 |        0.005 |
| Female | 18 - 29 | Hispanic  |  2600000 |    0.070 |     0.711 |   0.010 |        0.002 |
| Female | 18 - 29 | Other     |  2600000 |    0.060 |     0.711 |   0.010 |        0.003 |
| Female | 30 - 44 | White     | 22750000 |    0.120 |     0.599 |   0.091 |        0.096 |
| Female | 30 - 44 | Black     |  3250000 |    0.096 |     0.731 |   0.013 |        0.011 |
| Female | 30 - 44 | Hispanic  |  3250000 |    0.084 |     0.668 |   0.013 |        0.010 |
| Female | 30 - 44 | Other     |  3250000 |    0.072 |     0.668 |   0.013 |        0.008 |
| Female | 45 - 64 | White     | 27300000 |    0.160 |     0.574 |   0.109 |        0.146 |
| Female | 45 - 64 | Black     |  3900000 |    0.128 |     0.711 |   0.016 |        0.015 |
| Female | 45 - 64 | Hispanic  |  3900000 |    0.112 |     0.646 |   0.016 |        0.017 |
| Female | 45 - 64 | Other     |  3900000 |    0.096 |     0.646 |   0.016 |        0.014 |
| Female | 65+     | White     | 22750000 |    0.250 |     0.550 |   0.091 |        0.169 |
| Female | 65+     | Black     |  3250000 |    0.200 |     0.690 |   0.013 |        0.020 |
| Female | 65+     | Hispanic  |  3250000 |    0.175 |     0.622 |   0.013 |        0.015 |
| Female | 65+     | Other     |  3250000 |    0.150 |     0.622 |   0.013 |        0.006 |
| Male   | 18 - 29 | White     | 16800000 |    0.080 |     0.599 |   0.067 |        0.036 |
| Male   | 18 - 29 | Black     |  2400000 |    0.064 |     0.731 |   0.010 |        0.009 |
| Male   | 18 - 29 | Hispanic  |  2400000 |    0.056 |     0.668 |   0.010 |        0.003 |
| Male   | 18 - 29 | Other     |  2400000 |    0.048 |     0.668 |   0.010 |        0.005 |
| Male   | 30 - 44 | White     | 21000000 |    0.096 |     0.550 |   0.084 |        0.068 |
| Male   | 30 - 44 | Black     |  3000000 |    0.077 |     0.690 |   0.012 |        0.005 |
| Male   | 30 - 44 | Hispanic  |  3000000 |    0.067 |     0.622 |   0.012 |        0.003 |
| Male   | 30 - 44 | Other     |  3000000 |    0.058 |     0.622 |   0.012 |        0.005 |
| Male   | 45 - 64 | White     | 25200000 |    0.128 |     0.525 |   0.101 |        0.080 |
| Male   | 45 - 64 | Black     |  3600000 |    0.102 |     0.668 |   0.014 |        0.008 |
| Male   | 45 - 64 | Hispanic  |  3600000 |    0.090 |     0.599 |   0.014 |        0.010 |
| Male   | 45 - 64 | Other     |  3600000 |    0.077 |     0.599 |   0.014 |        0.009 |
| Male   | 65+     | White     | 21000000 |    0.200 |     0.500 |   0.084 |        0.145 |
| Male   | 65+     | Black     |  3000000 |    0.160 |     0.646 |   0.012 |        0.016 |
| Male   | 65+     | Hispanic  |  3000000 |    0.140 |     0.574 |   0.012 |        0.004 |
| Male   | 65+     | Other     |  3000000 |    0.120 |     0.574 |   0.012 |        0.008 |

Simulate survey data.

``` r
set.seed(435)

data <- 
  poststrat %>% 
  slice(people) %>% 
  mutate(y = rbinom(n(), size = 1, prob = yes_prob)) %>% 
  select(y, sex, age, ethnicity)
```

### Performing regression and poststratification

First, we fit a logistic regression, predicting the survey response
given sex, age, and ethnicity, with no interaction:

``` r
set.seed(907)

fit <- 
  stan_glm(
    y ~ sex + age + ethnicity,
    family = binomial(link = "logit"),
    data = data,
    refresh = 0
  )

fit
```

    #> stan_glm
    #>  family:       binomial [logit]
    #>  formula:      y ~ sex + age + ethnicity
    #>  observations: 1000
    #>  predictors:   8
    #> ------
    #>                   Median MAD_SD
    #> (Intercept)        0.6    0.2  
    #> sexMale           -0.2    0.1  
    #> age30 - 44        -0.3    0.2  
    #> age45 - 64        -0.4    0.2  
    #> age65+            -0.3    0.2  
    #> ethnicityBlack     0.9    0.3  
    #> ethnicityHispanic  0.7    0.3  
    #> ethnicityOther    -0.1    0.3  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

Estimate the proportion of “Yes” responses for each cell in the
poststratification table.

``` r
poststrat <- 
  poststrat %>% 
  mutate(yes_pred = predict(fit, type = "response", newdata = .))

poststrat %>% 
  select(sex, age, ethnicity, n, n_prop, yes_prob, yes_pred) %>% 
  knitr::kable()
```

| sex    | age     | ethnicity |        n | n\_prop | yes\_prob | yes\_pred |
|:-------|:--------|:----------|---------:|--------:|----------:|----------:|
| Female | 18 - 29 | White     | 18200000 |   0.073 |     0.646 |     0.639 |
| Female | 18 - 29 | Black     |  2600000 |   0.010 |     0.769 |     0.803 |
| Female | 18 - 29 | Hispanic  |  2600000 |   0.010 |     0.711 |     0.772 |
| Female | 18 - 29 | Other     |  2600000 |   0.010 |     0.711 |     0.624 |
| Female | 30 - 44 | White     | 22750000 |   0.091 |     0.599 |     0.576 |
| Female | 30 - 44 | Black     |  3250000 |   0.013 |     0.731 |     0.759 |
| Female | 30 - 44 | Hispanic  |  3250000 |   0.013 |     0.668 |     0.723 |
| Female | 30 - 44 | Other     |  3250000 |   0.013 |     0.668 |     0.561 |
| Female | 45 - 64 | White     | 27300000 |   0.109 |     0.574 |     0.550 |
| Female | 45 - 64 | Black     |  3900000 |   0.016 |     0.711 |     0.739 |
| Female | 45 - 64 | Hispanic  |  3900000 |   0.016 |     0.646 |     0.702 |
| Female | 45 - 64 | Other     |  3900000 |   0.016 |     0.646 |     0.535 |
| Female | 65+     | White     | 22750000 |   0.091 |     0.550 |     0.568 |
| Female | 65+     | Black     |  3250000 |   0.013 |     0.690 |     0.754 |
| Female | 65+     | Hispanic  |  3250000 |   0.013 |     0.622 |     0.717 |
| Female | 65+     | Other     |  3250000 |   0.013 |     0.622 |     0.554 |
| Male   | 18 - 29 | White     | 16800000 |   0.067 |     0.599 |     0.589 |
| Male   | 18 - 29 | Black     |  2400000 |   0.010 |     0.731 |     0.768 |
| Male   | 18 - 29 | Hispanic  |  2400000 |   0.010 |     0.668 |     0.733 |
| Male   | 18 - 29 | Other     |  2400000 |   0.010 |     0.668 |     0.574 |
| Male   | 30 - 44 | White     | 21000000 |   0.084 |     0.550 |     0.523 |
| Male   | 30 - 44 | Black     |  3000000 |   0.012 |     0.690 |     0.718 |
| Male   | 30 - 44 | Hispanic  |  3000000 |   0.012 |     0.622 |     0.679 |
| Male   | 30 - 44 | Other     |  3000000 |   0.012 |     0.622 |     0.509 |
| Male   | 45 - 64 | White     | 25200000 |   0.101 |     0.525 |     0.497 |
| Male   | 45 - 64 | Black     |  3600000 |   0.014 |     0.668 |     0.697 |
| Male   | 45 - 64 | Hispanic  |  3600000 |   0.014 |     0.599 |     0.656 |
| Male   | 45 - 64 | Other     |  3600000 |   0.014 |     0.599 |     0.483 |
| Male   | 65+     | White     | 21000000 |   0.084 |     0.500 |     0.515 |
| Male   | 65+     | Black     |  3000000 |   0.012 |     0.646 |     0.712 |
| Male   | 65+     | Hispanic  |  3000000 |   0.012 |     0.574 |     0.673 |
| Male   | 65+     | Other     |  3000000 |   0.012 |     0.574 |     0.501 |

Finally, we poststratify to estimate the proportion of the entire
population that would answer “Yes” to the survey:

``` r
poststrat %>% 
  summarize(yes_pop = sum(yes_pred * n_prop))
```

    #> # A tibble: 1 x 1
    #>   yes_pop
    #>     <dbl>
    #> 1   0.586

The above gives us the point estimate; to get inferential uncertainty,
we can work with the matrix of posterior simulations:

``` r
tibble(
  yes_pop = posterior_epred(fit, newdata = poststrat) %*% poststrat$n_prop
) %>% 
  summarize(across(yes_pop, list(mean = mean, sd = sd)))
```

    #> # A tibble: 1 x 2
    #>   yes_pop_mean yes_pop_sd
    #>          <dbl>      <dbl>
    #> 1        0.586     0.0170

The true proportion of the population that would answer “Yes” to the
survey, based upon the assumed probabilities of a “Yes” response for
each cell, is:

``` r
poststrat %>% 
  summarize(yes_pop_true = sum(yes_prob * n_prop))
```

    #> # A tibble: 1 x 1
    #>   yes_pop_true
    #>          <dbl>
    #> 1        0.593
