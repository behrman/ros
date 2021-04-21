Regression and Other Stories: Poststratification
================
Andrew Gelman, Jennifer Hill, Aki Vehtari
2021-04-20

-   [17 Poststratification and missing-data
    imputation](#17-poststratification-and-missing-data-imputation)
    -   [17.1 Poststratification: using regression to generalize to a
        new
        population](#171-poststratification-using-regression-to-generalize-to-a-new-population)
        -   [Adjusting for a single
            factor](#adjusting-for-a-single-factor)

Tidyverse version by Bill Behrman.

Poststratification after estimation. See Chapter 17 in Regression and
Other Stories.

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

## 17.1 Poststratification: using regression to generalize to a new population

### Adjusting for a single factor

The CBS News poll conducted from 12 – 16 October 2016 reported
that,among likely voters who preferred one of the two major-party
candidates, 45% intended to vote for Donald Trump and 55% for Hillary
Clinton. Of these respondents, 33% reported Republican party
identification, 40% affiliated themselves with Democrats, and 27% did
not declare a major-party affiliation.

Sources:

-   [CBS poll: Clinton’s lead over Trump widens with three weeks to
    go](http://www.cbsnews.com/news/cbs-poll-clintons-lead-over-trump-widens-with-three-weeks-to-go/).
    2016-10-17.
-   [CBS New Poll 10-17
    toplines](https://www.scribd.com/document/327938789/CBS-News-Poll-10-17-toplines).
    2016-10-17.

Data

``` r
exit_poll_2016 <- 
  tribble(
    ~party,       ~exit_pct,
    "Republican",        33,
    "Democrat",          36,
    "Independent",       31
  )

poll_n <- 
  tribble(
    ~party,         ~n,
    "Republican",  254,
    "Democrat",    282,
    "Independent", 242
  )

poll_stats <- 
  tribble(
    ~party,        ~candidate, ~vote_pct,
    "Republican",  "Trump",           77,
    "Republican",  "Clinton",          8,
    "Republican",  NA,                15,
    "Democrat",    "Trump",            5,
    "Democrat",    "Clinton",         89,
    "Democrat",    NA,                 6,
    "Independent", "Trump",           36,
    "Independent", "Clinton",         38,
    "Independent", NA,                26
  )
```

Check that survey results add up to 100%.

``` r
poll_stats %>% 
  group_by(party) %>% 
  summarize(vote_pct = sum(vote_pct))
```

    #> # A tibble: 3 x 2
    #>   party       vote_pct
    #> * <chr>          <dbl>
    #> 1 Democrat         100
    #> 2 Independent      100
    #> 3 Republican       100

Create dataset with above survey response statistics.

``` r
poll <- 
  poll_stats %>% 
  left_join(poll_n, by = "party") %>% 
  mutate(vote = round(n * vote_pct / 100)) %>% 
  group_by(party) %>% 
  mutate(
    vote = 
      if_else(!is.na(candidate), vote, first(n) - sum(vote[!is.na(candidate)]))
  ) %>% 
  ungroup() %>% 
  mutate(vote = map2(candidate, vote, rep)) %>% 
  select(party, vote) %>% 
  unnest(vote)

poll
```

    #> # A tibble: 778 x 2
    #>    party      vote 
    #>    <chr>      <chr>
    #>  1 Republican Trump
    #>  2 Republican Trump
    #>  3 Republican Trump
    #>  4 Republican Trump
    #>  5 Republican Trump
    #>  6 Republican Trump
    #>  7 Republican Trump
    #>  8 Republican Trump
    #>  9 Republican Trump
    #> 10 Republican Trump
    #> # … with 768 more rows

Check that `poll` has above survey response statistics.

``` r
poll %>% 
  count(party) %>% 
  all.equal(poll_n %>% arrange(party))
```

    #> [1] TRUE

``` r
poll %>% 
  count(party, vote) %>% 
  group_by(party) %>% 
  mutate(vote_pct = round(100 * n / sum(n))) %>% 
  ungroup() %>% 
  select(party, candidate = vote, vote_pct) %>% 
  arrange(party, candidate) %>% 
  all.equal(poll_stats %>% arrange(party, candidate))
```

    #> [1] TRUE

#### Poststratification using a weighted average

``` r
poll_stats %>% 
  drop_na(candidate) %>% 
  group_by(party) %>% 
  mutate(vote_prop = vote_pct / sum(vote_pct)) %>% 
  ungroup() %>% 
  filter(candidate == "Trump") %>% 
  left_join(exit_poll_2016, by = "party") %>% 
  summarize(trump_prop = weighted.mean(vote_prop, exit_pct))
```

    #> # A tibble: 1 x 1
    #>   trump_prop
    #>        <dbl>
    #> 1      0.469

#### What if we had weighted using the distribution of party identification in the sample?

``` r
mean(poll$vote == "Trump", na.rm = TRUE)
```

    #> [1] 0.45

#### Poststratification using regression prediction

Fit linear model to adjust for just one factor, party identification.

``` r
set.seed(660)

poll <- 
  poll %>% 
  mutate(vote_trump = vote == "Trump")

fit_1 <- stan_glm(vote_trump ~ party, data = poll, refresh = 0)

print(fit_1, digits = 2)
```

    #> stan_glm
    #>  family:       gaussian [identity]
    #>  formula:      vote_trump ~ party
    #>  observations: 660
    #>  predictors:   3
    #> ------
    #>                  Median MAD_SD
    #> (Intercept)      0.05   0.02  
    #> partyIndependent 0.43   0.03  
    #> partyRepublican  0.85   0.03  
    #> 
    #> Auxiliary parameter(s):
    #>       Median MAD_SD
    #> sigma 0.34   0.01  
    #> 
    #> ------
    #> * For help interpreting the printed output see ?print.stanreg
    #> * For info on the priors used see ?prior_summary.stanreg

We set up a poststratification matrix.

``` r
poststrat_1 <- 
  exit_poll_2016 %>% 
  transmute(
    party,
    prop = exit_pct / sum(exit_pct)
  )

poststrat_1
```

    #> # A tibble: 3 x 2
    #>   party        prop
    #>   <chr>       <dbl>
    #> 1 Republican   0.33
    #> 2 Democrat     0.36
    #> 3 Independent  0.31

Use the fitted model to make predictions for each row of the
poststratification matrix.

``` r
epred_1 <- posterior_epred(fit_1, newdata = poststrat_1)

dim(epred_1)
```

    #> [1] 4000    3

The result is a 4000 x 3 matrix representing the posterior uncertainty
in the proportion of Trump supporters in each of the three strata.

We sum these to get the population average.

``` r
poststrat_est <- tibble(trump_prop = epred_1 %*% poststrat_1$prop)

poststrat_est %>% 
  summarize(across(trump_prop, list(mean = mean, mad = mad)))
```

    #> # A tibble: 1 x 2
    #>   trump_prop_mean trump_prop_mad
    #>             <dbl>          <dbl>
    #> 1           0.469         0.0132

Suppose, for example, we would like to say that political polls can
easily be off by 2 percentage points, merely due to nonresponse and
changes of opinion. We could then add this uncertainty to our
prediction:

``` r
set.seed(274)

poll_error <- 0.02

poststrat_est %>% 
  mutate(trump_prop = trump_prop + rnorm(n(), mean = 0, sd = poll_error)) %>% 
  summarize(across(trump_prop, list(mean = mean, mad = mad)))
```

    #> # A tibble: 1 x 2
    #>   trump_prop_mean trump_prop_mad
    #>             <dbl>          <dbl>
    #> 1           0.469         0.0244

This new estimate has the same mean but a larger standard error.
