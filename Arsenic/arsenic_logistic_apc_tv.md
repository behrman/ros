Regression and Other Stories: Arsenic
================
Andrew Gelman, Jennifer Hill, Aki Vehtari
2021-04-20

-   [14 Working with logistic
    regression](#14-working-with-logistic-regression)
    -   [14.4 Average predictive comparisons on the probability
        scale](#144-average-predictive-comparisons-on-the-probability-scale)
        -   [Demonstration with the well-switching
            example](#demonstration-with-the-well-switching-example)
        -   [Average predictive comparisons in the presence of
            interactions](#average-predictive-comparisons-in-the-presence-of-interactions)

Tidyverse version by Bill Behrman.

Average predictive comparisons for a logistic regression model: wells in
Bangladesh. See Chapter 14 in Regression and Other Stories.

------------------------------------------------------------------------

``` r
# Packages
library(tidyverse)
library(rstanarm)

# Parameters
  # Data on arsenic in unsafe wells in Bangladesh
file_wells <- here::here("Arsenic/data/wells.csv")
  # Common code
file_common <- here::here("_common.R")

#===============================================================================

# Run common code
source(file_common)
```

# 14 Working with logistic regression

## 14.4 Average predictive comparisons on the probability scale

### Demonstration with the well-switching example

Data

``` r
wells <- read_csv(file_wells)

summary(wells)
```

    #>      switch         arsenic          dist        dist100         assoc      
    #>  Min.   :0.000   Min.   :0.51   Min.   :  0   Min.   :0.00   Min.   :0.000  
    #>  1st Qu.:0.000   1st Qu.:0.82   1st Qu.: 21   1st Qu.:0.21   1st Qu.:0.000  
    #>  Median :1.000   Median :1.30   Median : 37   Median :0.37   Median :0.000  
    #>  Mean   :0.575   Mean   :1.66   Mean   : 48   Mean   :0.48   Mean   :0.423  
    #>  3rd Qu.:1.000   3rd Qu.:2.20   3rd Qu.: 64   3rd Qu.:0.64   3rd Qu.:1.000  
    #>  Max.   :1.000   Max.   :9.65   Max.   :340   Max.   :3.40   Max.   :1.000  
    #>       educ           educ4     
    #>  Min.   : 0.00   Min.   :0.00  
    #>  1st Qu.: 0.00   1st Qu.:0.00  
    #>  Median : 5.00   Median :1.25  
    #>  Mean   : 4.83   Mean   :1.21  
    #>  3rd Qu.: 8.00   3rd Qu.:2.00  
    #>  Max.   :17.00   Max.   :4.25

The variables are:

-   `switch`: Outcome variable:
    -   1 if household switched to a new well
    -   0 if household continued using its own well
-   `arsenic`: Arsenic level of respondent’s well
-   `dist`: Distance (in meters) to the closest known safe well
-   `dist100` = `dist / 100`
-   `assoc`: Whether any members of the household are active in
    community organizations
-   `educ`: Education level of the head of household
-   `educ4` = `educ / 4`

Fit a model using scaled distance, arsenic level, and education of head
of household.

``` r
set.seed(733)

fit_7 <- 
  stan_glm(
    switch ~ dist100 + arsenic + educ4,
    family = binomial(link = "logit"),
    data = wells,
    refresh = 0
  )

print(fit_7, digits = 2)
```

    #> stan_glm
    #>  family:       binomial [logit]
    #>  formula:      switch ~ dist100 + arsenic + educ4
    #>  observations: 3020
    #>  predictors:   4
    #> ------
    #>             Median MAD_SD
    #> (Intercept) -0.21   0.09 
    #> dist100     -0.90   0.10 
    #> arsenic      0.47   0.04 
    #> educ4        0.17   0.04 
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

#### Average predictive difference in probability of switching

Model coefficients.

``` r
b <- coef(fit_7)
names(b)
```

    #> [1] "(Intercept)" "dist100"     "arsenic"     "educ4"

Compare households that are next to or 100 meters from the nearest safe
well.

``` r
dist100_lo <- 0
dist100_hi <- 1

dist100_apd <- 
  wells %>% 
  mutate(
    delta = 
      plogis(b[1] + b[2] * dist100_hi + b[3] * arsenic + b[4] * educ4) -
      plogis(b[1] + b[2] * dist100_lo + b[3] * arsenic + b[4] * educ4)
  ) %>% 
  pull(delta) %>% 
  mean()

dist100_apd
```

    #> [1] -0.205

The result is -0.21, implying that, on average in the data, households
that are 100 meters from the nearest safe well are 21% less likely to
switch, compared to households that are right next to the nearest safe
well, at the same arsenic and education levels.

#### Comparing probabilities of switching for households differing in arsenic levels

Compare households with existing arsenic levels of 0.5 or 1.0.

``` r
arsenic_lo <- 0.5
arsenic_hi <- 1.0

arsenic_apd <- 
  wells %>% 
  mutate(
    delta = 
      plogis(b[1] + b[2] * dist100 + b[3] * arsenic_hi + b[4] * educ4) -
      plogis(b[1] + b[2] * dist100 + b[3] * arsenic_lo + b[4] * educ4)
  ) %>% 
  pull(delta) %>% 
  mean()

arsenic_apd
```

    #> [1] 0.0566

The result is 0.057 – so this corresponds to a 5.7% difference in the
probability of switching.

#### Average predictive difference in probability of switching, comparing households with 0 and 12 years of education

Compare households with 0 or 12 years of education.

``` r
educ4_lo <- 0
educ4_hi <- 3

educ4_apd <- 
  wells %>% 
  mutate(
    delta = 
      plogis(b[1] + b[2] * dist100 + b[3] * arsenic + b[4] * educ4_hi) -
      plogis(b[1] + b[2] * dist100 + b[3] * arsenic + b[4] * educ4_lo)
  ) %>% 
  pull(delta) %>% 
  mean()

educ4_apd
```

    #> [1] 0.116

This comes to 0.12, a difference of 12%.

### Average predictive comparisons in the presence of interactions

Center variables.

``` r
wells <- 
  wells %>% 
  mutate(
    arsenic_c = arsenic - mean(arsenic),
    dist100_c = dist100 - mean(dist100),
    educ4_c = educ4 - mean(educ4)
  )
```

Fit a model using scaled distance, arsenic level, education of head of
household, and interactions with education.

``` r
set.seed(733)

fit_8 <- 
  stan_glm(
    switch ~ 
      dist100_c + arsenic_c + educ4_c + dist100_c:educ4_c + arsenic_c:educ4_c,
    family = binomial(link = "logit"),
    data = wells,
    refresh = 0
  )

print(fit_8, digits = 2)
```

    #> stan_glm
    #>  family:       binomial [logit]
    #>  formula:      switch ~ dist100_c + arsenic_c + educ4_c + dist100_c:educ4_c + 
    #>     arsenic_c:educ4_c
    #>  observations: 3020
    #>  predictors:   6
    #> ------
    #>                   Median MAD_SD
    #> (Intercept)        0.35   0.04 
    #> dist100_c         -0.92   0.10 
    #> arsenic_c          0.49   0.04 
    #> educ4_c            0.19   0.04 
    #> dist100_c:educ4_c  0.33   0.10 
    #> arsenic_c:educ4_c  0.08   0.04 
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

Model coefficients.

``` r
b <- coef(fit_8)
names(b)
```

    #> [1] "(Intercept)"       "dist100_c"         "arsenic_c"        
    #> [4] "educ4_c"           "dist100_c:educ4_c" "arsenic_c:educ4_c"

Compare households that are next to or 100 meters from the nearest safe
well.

``` r
dist100_lo <- 0
dist100_hi <- 1
dist100_c_lo <- dist100_lo - mean(wells$dist100)
dist100_c_hi <- dist100_hi - mean(wells$dist100)

dist100_apd <- 
  wells %>% 
  mutate(
    delta = 
      plogis(
        b[1] + b[2] * dist100_c_hi + b[3] * arsenic_c + b[4] * educ4_c +
          b[5] * dist100_c_hi * educ4_c + b[6] * arsenic_c * educ4_c
      ) -
      plogis(
        b[1] + b[2] * dist100_c_lo + b[3] * arsenic_c + b[4] * educ4_c +
          b[5] * dist100_c_lo * educ4_c + b[6] * arsenic_c * educ4_c
      )
  ) %>% 
  pull(delta) %>% 
  mean()

dist100_apd
```

    #> [1] -0.211

This comes to -0.21, a difference of 21%.
