Regression and Other Stories - Tidyverse Examples
================

-   [Examples by chapters](#examples-by-chapters)
    -   [1 Introduction](#1-introduction)
    -   [2 Data and measurement](#2-data-and-measurement)
    -   [3 Some basic methods in mathematics and
        probability](#3-some-basic-methods-in-mathematics-and-probability)
    -   [4 Generative models and statistical
        inference](#4-generative-models-and-statistical-inference)
    -   [5 Simulation](#5-simulation)
    -   [6 Background on regression
        modeling](#6-background-on-regression-modeling)
    -   [7 Linear regression with a single
        predictor](#7-linear-regression-with-a-single-predictor)
    -   [8 Fitting regression models](#8-fitting-regression-models)
    -   [9 Prediction and Bayesian
        inference](#9-prediction-and-bayesian-inference)
    -   [10 Linear regression with multiple
        predictors](#10-linear-regression-with-multiple-predictors)
    -   [11 Assumptions, diagnostics, and model
        evaluation](#11-assumptions-diagnostics-and-model-evaluation)
    -   [12 Transformations](#12-transformations)
    -   [13 Logistic regression](#13-logistic-regression)
    -   [14 Working with logistic
        regression](#14-working-with-logistic-regression)
    -   [15 Other generalized linear
        models](#15-other-generalized-linear-models)
    -   [16 Design and sample size
        decisions](#16-design-and-sample-size-decisions)
    -   [17 Poststratification and missing-data
        imputation](#17-poststratification-and-missing-data-imputation)
    -   [18 Causal inference basics and randomized
        experiments](#18-causal-inference-basics-and-randomized-experiments)
    -   [19 Causal inference using regression on the treatment
        variable](#19-causal-inference-using-regression-on-the-treatment-variable)
    -   [20 Observational studies with all confounders assumed to be
        measured](#20-observational-studies-with-all-confounders-assumed-to-be-measured)
    -   [21 More advanced topics in causal
        inference](#21-more-advanced-topics-in-causal-inference)
    -   [22 Advanced regression and multilevel
        models](#22-advanced-regression-and-multilevel-models)
    -   [A Computing in R](#a-computing-in-r)
-   [Examples alphabetically](#examples-alphabetically)

This repository contains [Tidyverse](https://www.tidyverse.org/)
implementations of examples from *Regression and Other Stories* by
[Andrew Gelman](http://www.stat.columbia.edu/~gelman/), [Jennifer
Hill](https://steinhardt.nyu.edu/people/jennifer-hill), and [Aki
Vehtari](https://users.aalto.fi/~ave/) (2020).

-   [Book website](http://www.stat.columbia.edu/~gelman/regression/)
-   Original code provided with book
    -   [Code
        website](https://avehtari.github.io/ROS-Examples/examples.html)
    -   [GitHub repository](https://github.com/avehtari/ROS-Examples)

Tidyverse version by [Bill Behrman](https://datalab.stanford.edu/bill).

------------------------------------------------------------------------

## Examples by chapters

### 1 Introduction

-   [ElectionsEconomy/](ElectionsEconomy/)
    -   [hibbs\_tv.md](ElectionsEconomy/hibbs_tv.md) - Predicting
        presidential vote share from the economy
-   [ElectricCompany/](ElectricCompany/)
    -   [electric\_tv.md](ElectricCompany/electric_tv.md) - Analysis of
        “Electric Company” data
-   [Peacekeeping/](Peacekeeping/)
    -   [peace\_tv.md](Peacekeeping/peace_tv.md) - Outcomes after civil
        war in countries with and without United Nations peacekeeping
-   [SimpleCausal/](SimpleCausal/)
    -   [causal\_tv.md](SimpleCausal/causal_tv.md) - Simple graphs
        illustrating regression for causal inference
-   [Helicopters/](Helicopters/)
    -   [helicopters\_tv.md](Helicopters/helicopters_tv.md) - Example
        data file for helicopter flying time exercise

### 2 Data and measurement

-   [HDI/](HDI/)
    -   [hdi\_tv.md](HDI/hdi_tv.md) - Human Development Index - Looking
        at data in different ways
-   [Pew/](Pew/)
    -   [pew\_tv.md](Pew/pew_tv.md) - Miscellaneous analyses using raw
        Pew data
-   [HealthExpenditure/](HealthExpenditure/)
    -   [healthexpenditure\_tv.md](HealthExpenditure/healthexpenditure_tv.md) -
        Discovery through graphs of data and models
-   [Names/](Names/)
    -   [lastletters\_tv.md](Names/lastletters_tv.md) - Last letters -
        Distributions of last letters of names of American babies
-   [Congress/](Congress/)
    -   [congress\_plots\_tv.md](Congress/congress_plots_tv.md) -
        Predictive uncertainty for congressional elections
-   [AgePeriodCohort/](AgePeriodCohort/)
    -   [births\_tv.md](AgePeriodCohort/births_tv.md) - Age adjustment

### 3 Some basic methods in mathematics and probability

-   [Mile/](Mile/)
    -   [mile\_tv.md](Mile/mile_tv.md) - Trend of record times in the
        mile run
-   [Metabolic/](Metabolic/)
    -   [metabolic\_tv.md](Metabolic/metabolic_tv.md) - How to interpret
        a power law or log-log regression
-   [CentralLimitTheorem/](CentralLimitTheorem/)
    -   [heightweight\_tv.md](CentralLimitTheorem/heightweight_tv.md) -
        Illustrate central limit theorem and normal distribution
-   [Stents/](Stents/)
    -   [stents\_tv.md](Stents/stents_tv.md) - Stents - comparing
        distributions

### 4 Generative models and statistical inference

-   [Coverage/](Coverage/)
    -   [coverage\_tv.md](Coverage/coverage_tv.md) - Example of coverage
-   [Death/](Death/)
    -   [polls\_tv.md](Death/polls_tv.md) - Proportion of American
        adults supporting the death penalty
-   [Coop/](Coop/)
    -   [riverbay\_tv.md](Coop/riverbay_tv.md) - Example of hypothesis
        testing
-   [Girls/](Girls/)

### 5 Simulation

-   [ProbabilitySimulation/](ProbabilitySimulation/)
    -   [probsim\_tv.md](ProbabilitySimulation/probsim_tv.md) -
        Simulation of probability models
-   [Earnings/](Earnings/)
    -   [earnings\_bootstrap\_tv.md](Earnings/earnings_bootstrap_tv.md) -
        Bootstrapping to simulate the sampling distribution

### 6 Background on regression modeling

-   [Simplest/](Simplest/)
    -   [simplest\_tv.md](Simplest/simplest_tv.md) - Linear regression
        with a single predictor
-   [Earnings/](Earnings/)
    -   [earnings\_regression\_tv.md](Earnings/earnings_regression_tv.md) -
        Predict respondents’ yearly earnings using survey data from 1990
-   [PearsonLee/](PearsonLee/)
    -   [heights\_tv.md](PearsonLee/heights_tv.md) - The heredity of
        height. Published in 1903 by Karl Pearson and Alice Lee.
-   [FakeMidtermFinal/](FakeMidtermFinal/)
    -   [simulation\_tv.md](FakeMidtermFinal/simulation_tv.md) - Fake
        dataset of 1,000 students’ scores on a midterm and final exam

### 7 Linear regression with a single predictor

-   [ElectionsEconomy/](ElectionsEconomy/)
    -   [hibbs\_tv.md](ElectionsEconomy/hibbs_tv.md) - Predicting
        presidential vote share from the economy
    -   [hibbs\_coverage\_tv.md](ElectionsEconomy/hibbs_coverage_tv.md) -
        Checking the coverage of intervals
-   [Simplest/](Simplest/)
    -   [simplest\_tv.md](Simplest/simplest_tv.md) - Linear regression
        with a single predictor

### 8 Fitting regression models

-   [ElectionsEconomy/](ElectionsEconomy/)
    -   [hills\_tv.md](ElectionsEconomy/hills_tv.md) - Present
        uncertainty in parameter estimates
    -   [hibbs\_tv.md](ElectionsEconomy/hibbs_tv.md) - Predicting
        presidential vote share from the economy
-   [Influence/](Influence/)
    -   [influence\_tv.md](Influence/influence_tv.md) - Influence of
        individual points in a fitted regression

### 9 Prediction and Bayesian inference

-   [ElectionsEconomy/](ElectionsEconomy/)
    -   [hibbs\_tv.md](ElectionsEconomy/hibbs_tv.md) - Predicting
        presidential vote share from the economy
    -   [bayes\_tv.md](ElectionsEconomy/bayes_tv.md) - Demonstration of
        Bayesian information aggregation
-   [Earnings/](Earnings/)
    -   [height\_and\_weight\_tv.md](Earnings/height_and_weight_tv.md) -
        Predict weight
-   [SexRatio/](SexRatio/)
    -   [sexratio\_tv.md](SexRatio/sexratio_tv.md) - Example where an
        informative prior makes a difference

### 10 Linear regression with multiple predictors

-   [KidIQ/](KidIQ/)
    -   [kidiq\_tv.md](KidIQ/kidiq_tv.md) - Linear regression with
        multiple predictors
-   [Earnings/](Earnings/)
    -   [height\_and\_weight\_tv.md](Earnings/height_and_weight_tv.md) -
        Predict weight
-   [Congress/](Congress/)
    -   [congress\_tv.md](Congress/congress_tv.md) - Predictive
        uncertainty for congressional elections
-   [NES/](NES/)
    -   [nes\_linear\_tv.md](NES/nes_linear_tv.md) - Fitting the same
        regression to many datasets
-   [Beauty/](Beauty/)
    -   [beauty\_tv.md](Beauty/beauty_tv.md) - Student evaluations of
        instructors’ beauty and teaching quality

### 11 Assumptions, diagnostics, and model evaluation

-   [KidIQ/](KidIQ/)
    -   [kidiq\_tv.md](KidIQ/kidiq_tv.md) - Linear regression with
        multiple predictors
-   [Residuals/](Residuals/)
    -   [residuals\_tv.md](Residuals/residuals_tv.md) - Plotting the
        data and fitted model
-   [Introclass/](Introclass/)
    -   [residual\_plots\_tv.md](Introclass/residual_plots_tv.md) - Plot
        residuals vs. predicted values, or residuals vs. observed
        values?
-   [Newcomb/](Newcomb/)
    -   [newcomb\_tv.md](Newcomb/newcomb_tv.md) - Posterior predictive
        checking of Normal model for Newcomb’s speed of light data
-   [Unemployment/](Unemployment/)
    -   [unemployment\_tv.md](Unemployment/unemployment_tv.md) - Time
        series fit and posterior predictive model checking for
        unemployment series
-   [Rsquared/](Rsquared/)
    -   [rsquared\_tv.md](Rsquared/rsquared_tv.md) - Bayesian R^2
-   [CrossValidation/](CrossValidation/)
    -   [crossvalidation\_tv.md](CrossValidation/crossvalidation_tv.md) -
        Demonstration of cross validation
-   [FakeKCV/](FakeKCV/)
    -   [fake\_kcv\_tv.md](FakeKCV/fake_kcv_tv.md) - Demonstration of
        *K*-fold cross-validation using simulated data
-   [Pyth/](Pyth/)

### 12 Transformations

-   [KidIQ/](KidIQ/)
    -   [kidiq\_tv.md](KidIQ/kidiq_tv.md) - Linear regression with
        multiple predictors
-   [Earnings/](Earnings/)
    -   [earnings\_regression\_tv.md](Earnings/earnings_regression_tv.md) -
        Predict respondents’ yearly earnings using survey data from 1990
-   [Gay/](Gay/)
    -   [gay\_simple\_tv.md](Gay/gay_simple_tv.md) - Simple models
        (linear and discretized age) and political attitudes as a
        function of age
-   [Mesquite/](Mesquite/)
    -   [mesquite\_tv.md](Mesquite/mesquite_tv.md) - Predicting the
        yields of mesquite bushes
-   [Student/](Student/)
    -   [student\_tv.md](Student/student_tv.md) - Models for regression
        coefficients
-   [Pollution/](Pollution/)
    -   [pollution\_tv.md](Pollution/pollution_tv.md) - Pollution data

### 13 Logistic regression

-   [NES/](NES/)
    -   [nes\_logistic\_tv.md](NES/nes_logistic_tv.md) - Logistic
        regression, identifiability, and separation
-   [LogisticPriors/](LogisticPriors/)
    -   [logistic\_priors\_tv.md](LogisticPriors/logistic_priors_tv.md) -
        Effect of priors in logistic regression
-   [Arsenic/](Arsenic/)
    -   [arsenic\_logistic\_building\_tv.md](Arsenic/arsenic_logistic_building_tv.md) -
        Building a logistic regression model: wells in Bangladesh
-   [Rodents/](Rodents/)

### 14 Working with logistic regression

-   [LogitGraphs/](LogitGraphs/)
    -   [logitgraphs\_tv.md](LogitGraphs/logitgraphs_tv.md) - Different
        ways of displaying logistic regression
-   [Arsenic/](Arsenic/)
    -   [arsenic\_logistic\_building\_tv.md](Arsenic/arsenic_logistic_building_tv.md) -
        Building a logistic regression model: wells in Bangladesh
    -   [arsenic\_logistic\_apc\_tv.md](Arsenic/arsenic_logistic_apc_tv.md) -
        Average predictive comparisons for a logistic regression model:
        wells in Bangladesh
    -   [arsenic\_logistic\_residuals\_tv.md](Arsenic/arsenic_logistic_residuals_tv.md) -
        Residual plots for a logistic regression model: wells in
        Bangladesh
-   [NES/](NES/)
    -   [nes\_logistic\_tv.md](NES/nes_logistic_tv.md) - Logistic
        regression, identifiability, and separation

### 15 Other generalized linear models

-   [PoissonExample/](PoissonExample/)
    -   [poisson\_regression\_tv.md](PoissonExample/poisson_regression_tv.md) -
        Demonstrate Poisson regression with simulated data
-   [Roaches/](Roaches/)
    -   [roaches\_tv.md](Roaches/roaches_tv.md) - Analyze the effect of
        integrated pest management on reducing cockroach levels in urban
        apartments
-   [Storable/](Storable/)
    -   [storable\_tv.md](Storable/storable_tv.md) - Ordered categorical
        data analysis with a study from experimental economics, on the
        topic of “storable votes”
-   [Robit/](Robit/)
    -   [robit\_tv.md](Robit/robit_tv.md) - Comparison of robit and
        logit models for binary data
-   [Earnings/](Earnings/)
    -   [earnings\_compound\_tv.md](Earnings/earnings_compound_tv.md) -
        Compound discrete-continuous model
-   [RiskyBehavior/](RiskyBehavior/)
    -   [risky\_tv.md](RiskyBehavior/risky_tv.md) Risky behavior data
-   [NES/](NES/)
-   [Lalonde/](Lalonde/)
-   [Congress/](Congress/)
-   [AcademyAwards/](AcademyAwards/)

### 16 Design and sample size decisions

-   [SampleSize/](SampleSize/)
    -   [simulation\_tv.md](SampleSize/simulation_tv.md) - Sample size
        simulation
-   [FakeMidtermFinal/](FakeMidtermFinal/)
    -   [simulation\_based\_design\_tv.md](FakeMidtermFinal/simulation_based_design_tv.md) -
        Fake dataset of a randomized experiment on student grades
-   [ElectricCompany/](ElectricCompany/)
    -   [electric\_tv.md](ElectricCompany/electric_tv.md) - Analysis of
        “Electric Company” data

### 17 Poststratification and missing-data imputation

-   [Poststrat/](Poststrat/)
    -   [poststrat\_tv.md](Poststrat/poststrat_tv.md) -
        Poststratification after estimation
    -   [poststrat2\_tv.md](Poststrat/poststrat2_tv.md) -
        Poststratification after estimation
-   [Imputation/](Imputation/)
    -   [imputation\_tv.md](Imputation/imputation_tv.md) -
        Regression-based imputation for the Social Indicators Survey

### 18 Causal inference basics and randomized experiments

-   [Sesame/](Sesame/)
    -   [sesame\_tv.md](Sesame/sesame_tv.md) - Causal analysis of Sesame
        Street experiment

### 19 Causal inference using regression on the treatment variable

-   [ElectricCompany/](ElectricCompany/)
    -   [electric\_tv.md](ElectricCompany/electric_tv.md) - Analysis of
        “Electric Company” data
-   [Incentives/](Incentives/)
    -   [incentives\_tv.md](Incentives/incentives_tv.md) - Simple
        analysis of incentives data
-   [Cows/](Cows/)

### 20 Observational studies with all confounders assumed to be measured

-   [ElectricCompany/](ElectricCompany/)
    -   [electric\_tv.md](ElectricCompany/electric_tv.md) - Analysis of
        “Electric Company” data
-   [Childcare/](Childcare/)
    -   [childcare\_tv.md](Childcare/childcare_tv.md) - Infant Health
        and Development Program (IHDP) example
-   [Lalonde/](Lalonde/)

### 21 More advanced topics in causal inference

-   [Sesame/](Sesame/)
    -   [sesame\_tv.md](Sesame/sesame_tv.md) - Causal analysis of Sesame
        Street experiment
-   [ChileSchools/](ChileSchools/)
    -   [chile\_schools\_tv.md](ChileSchools/chile_schools_tv.md) -
        ChileSchools example.
-   [Bypass/](Bypass/)

### 22 Advanced regression and multilevel models

-   [Golf/](Golf/)
    -   [golf\_tv.md](Golf/golf_tv.md) - Gold putting accuracy: Fitting
        a nonlinear model using Stan
-   [Gay/](Gay/)
    -   [gay\_tv.md](Gay/gay_tv.md) - Nonlinear models (LOESS and
        spline) and political attitudes as a function of age
-   [ElectionsEconomy/](ElectionsEconomy/)
    -   [hibbs\_tv.md](ElectionsEconomy/hibbs_tv.md) - Predicting
        presidential vote share from the economy
-   [Scalability/](Scalability/)
    -   [scalability\_tv.md](Scalability/scalability_tv.md) -
        Demonstrate computation speed with 100,000 observations

### A Computing in R

-   [Coins/](Coins/)
-   [Mile/](Mile/)
    -   [mile\_tv.md](Mile/mile_tv.md) - Trend of record times in the
        mile run
-   [Earnings/](Earnings/)
    -   [earnings\_data\_tv.md](Earnings/earnings_data_tv.md) - Read in
        and prepare earnings data
-   [Parabola/](Parabola/)
    -   [parabola\_tv.md](Parabola/parabola_tv.md) - Demonstration of
        using Stan for optimization
-   [Restaurant/](Restaurant/)
    -   [restaurant\_tv.md](Restaurant/restaurant_tv.md) - Demonstration
        of using Stan for optimization
-   [DifferentSoftware/](DifferentSoftware/)
    -   [linear\_tv.md](DifferentSoftware/linear_tv.md) - Linear
        regression using different software options

------------------------------------------------------------------------

## Examples alphabetically

-   [AcademyAwards/](AcademyAwards/)
-   [AgePeriodCohort/](AgePeriodCohort/)
    -   [births\_tv.md](AgePeriodCohort/births_tv.md) - Age adjustment
-   [Arsenic/](Arsenic/)
    -   [arsenic\_logistic\_building\_tv.md](Arsenic/arsenic_logistic_building_tv.md) -
        Building a logistic regression model: wells in Bangladesh
    -   [arsenic\_logistic\_apc\_tv.md](Arsenic/arsenic_logistic_apc_tv.md) -
        Average predictive comparisons for a logistic regression model:
        wells in Bangladesh
    -   [arsenic\_logistic\_residuals\_tv.md](Arsenic/arsenic_logistic_residuals_tv.md) -
        Residual plots for a logistic regression model: wells in
        Bangladesh
-   [Beauty/](Beauty/)
    -   [beauty\_tv.md](Beauty/beauty_tv.md) - Student evaluations of
        instructors’ beauty and teaching quality
-   [Bypass/](Bypass/)
-   [CentralLimitTheorem/](CentralLimitTheorem/)
    -   [heightweight\_tv.md](CentralLimitTheorem/heightweight_tv.md) -
        Illustrate central limit theorem and normal distribution
-   [Childcare/](Childcare/)
    -   [childcare\_tv.md](Childcare/childcare_tv.md) - Infant Health
        and Development Program (IHDP) example
-   [ChileSchools/](ChileSchools/)
    -   [chile\_schools\_tv.md](ChileSchools/chile_schools_tv.md) -
        ChileSchools example.
-   [Coins/](Coins/)
-   [Congress/](Congress/)
    -   [congress\_tv.md](Congress/congress_tv.md) - Predictive
        uncertainty for congressional elections
    -   [congress\_plots\_tv.md](Congress/congress_plots_tv.md) -
        Predictive uncertainty for congressional elections
-   [Coop/](Coop/)
    -   [riverbay\_tv.md](Coop/riverbay_tv.md) - Example of hypothesis
        testing
-   [Coverage/](Coverage/)
    -   [coverage\_tv.md](Coverage/coverage_tv.md) - Example of coverage
-   [Cows/](Cows/)
-   [CrossValidation/](CrossValidation/)
    -   [crossvalidation\_tv.md](CrossValidation/crossvalidation_tv.md) -
        Demonstration of cross validation
-   [Death/](Death/)
    -   [polls\_tv.md](Death/polls_tv.md) - Proportion of American
        adults supporting the death penalty
-   [DifferentSoftware/](DifferentSoftware/)
    -   [linear\_tv.md](DifferentSoftware/linear_tv.md) - Linear
        regression using different software options
-   [Earnings/](Earnings/)
    -   [earnings\_bootstrap\_tv.md](Earnings/earnings_bootstrap_tv.md) -
        Bootstrapping to simulate the sampling distribution
    -   [earnings\_compound\_tv.md](Earnings/earnings_compound_tv.md) -
        Compound discrete-continuous model
    -   [earnings\_regression\_tv.md](Earnings/earnings_regression_tv.md) -
        Predict respondents’ yearly earnings using survey data from 1990
    -   [height\_and\_weight\_tv.md](Earnings/height_and_weight_tv.md) -
        Predict weight
    -   [earnings\_data\_tv.md](Earnings/earnings_data_tv.md) - Read in
        and prepare earnings data
-   [ElectionsEconomy/](ElectionsEconomy/)
    -   [bayes\_tv.md](ElectionsEconomy/bayes_tv.md) - Demonstration of
        Bayesian information aggregation
    -   [hibbs\_coverage\_tv.md](ElectionsEconomy/hibbs_coverage_tv.md) -
        Checking the model-fitting procedure using fake-data simulation.
    -   [hibbs\_tv.md](ElectionsEconomy/hibbs_tv.md) - Predicting
        presidential vote share from the economy
    -   [hills\_tv.md](ElectionsEconomy/hills_tv.md) - Present
        uncertainty in parameter estimates
-   [ElectricCompany/](ElectricCompany/)
    -   [electric\_tv.md](ElectricCompany/electric_tv.md) - Analysis of
        “Electric Company” data
-   [FakeKCV/](FakeKCV/)
    -   [fake\_kcv\_tv.md](FakeKCV/fake_kcv_tv.md) - Demonstration of
        *K*-fold cross-validation using simulated data
-   [FakeMidtermFinal/](FakeMidtermFinal/)
    -   [simulation\_tv.md](FakeMidtermFinal/simulation_tv.md) - Fake
        dataset of 1,000 students’ scores on a midterm and final exam
    -   [simulation\_based\_design\_tv.md](FakeMidtermFinal/simulation_based_design_tv.md) -
        Fake dataset of a randomized experiment on student grades
-   [Gay/](Gay/)
    -   [gay\_simple\_tv.md](Gay/gay_simple_tv.md) - Simple models
        (linear and discretized age) and political attitudes as a
        function of age
    -   [gay\_tv.md](Gay/gay_tv.md) - Nonlinear models (LOESS and
        spline) and political attitudes as a function of age
-   [Girls/](Girls/)
-   [Golf/](Golf/)
    -   [golf\_tv.md](Golf/golf_tv.md) - Gold putting accuracy: Fitting
        a nonlinear model using Stan
-   [HDI/](HDI/)
    -   [hdi\_tv.md](HDI/hdi_tv.md) - Human Development Index - Looking
        at data in different ways
-   [HealthExpenditure/](HealthExpenditure/)
    -   [healthexpenditure\_tv.md](HealthExpenditure/healthexpenditure_tv.md) -
        Discovery through graphs of data and models
-   [Helicopters/](Helicopters/)
    -   [helicopters\_tv.md](Helicopters/helicopters_tv.md) - Example
        data file for helicopter flying time exercise
-   [Imputation/](Imputation/)
    -   [imputation\_tv.md](Imputation/imputation_tv.md) -
        Regression-based imputation for the Social Indicators Survey
-   [Incentives/](Incentives/)
    -   [incentives\_tv.md](Incentives/incentives_tv.md) - Simple
        analysis of incentives data
-   [Influence/](Influence/)
    -   [influence\_tv.md](Influence/influence_tv.md) - Influence of
        individual points in a fitted regression
-   [Introclass/](Introclass/)
    -   [residual\_plots\_tv.md](Introclass/residual_plots_tv.md) - Plot
        residuals vs. predicted values, or residuals vs. observed
        values?
-   [KidIQ/](KidIQ/)
    -   [kidiq\_tv.md](KidIQ/kidiq_tv.md) - Linear regression with
        multiple predictors
-   [Lalonde/](Lalonde/)
-   [LogisticPriors/](LogisticPriors/)
    -   [logistic\_priors\_tv.md](LogisticPriors/logistic_priors_tv.md) -
        Effect of priors in logistic regression
-   [LogitGraphs/](LogitGraphs/)
    -   [logitgraphs\_tv.md](LogitGraphs/logitgraphs_tv.md) - Different
        ways of displaying logistic regression
-   [Mesquite/](Mesquite/)
    -   [mesquite\_tv.md](Mesquite/mesquite_tv.md) - Predicting the
        yields of mesquite bushes
-   [Metabolic/](Metabolic/)
    -   [metabolic\_tv.md](Metabolic/metabolic_tv.md) - How to interpret
        a power law or log-log regression
-   [Mile/](Mile/)
    -   [mile\_tv.md](Mile/mile_tv.md) - Trend of record times in the
        mile run
-   [Names/](Names/)
    -   [lastletters\_tv.md](Names/lastletters_tv.md) - Last letters -
        Distributions of last letters of names of American babies
-   [NES/](NES/)
    -   [nes\_linear\_tv.md](NES/nes_linear_tv.md) - Fitting the same
        regression to many datasets
    -   [nes\_logistic\_tv.md](NES/nes_logistic_tv.md) - Logistic
        regression, identifiability, and separation
-   [Newcomb/](Newcomb/)
    -   [newcomb\_tv.md](Newcomb/newcomb_tv.md) - Posterior predictive
        checking of Normal model for Newcomb’s speed of light data
-   [Parabola/](Parabola/)
    -   [parabola\_tv.md](Parabola/parabola_tv.md) - Demonstration of
        using Stan for optimization
-   [Peacekeeping/](Peacekeeping/)
    -   [peace\_tv.md](Peacekeeping/peace_tv.md) - Outcomes after civil
        war in countries with and without United Nations peacekeeping
-   [PearsonLee/](PearsonLee/)
    -   [heights\_tv.md](PearsonLee/heights_tv.md) - The heredity of
        height. Published in 1903 by Karl Pearson and Alice Lee.
-   [Pew/](Pew/)
    -   [pew\_tv.md](Pew/pew_tv.md) - Miscellaneous analyses using raw
        Pew data
-   [PoissonExample/](PoissonExample/)
    -   [poisson\_regression\_tv.md](PoissonExample/poisson_regression_tv.md) -
        Demonstrate Poisson regression with simulated data
-   [Pollution/](Pollution/)
    -   [pollution\_tv.md](Pollution/pollution_tv.md) - Pollution data
-   [Poststrat/](Poststrat/)
    -   [poststrat\_tv.md](Poststrat/poststrat_tv.md) -
        Poststratification after estimation
    -   [poststrat2\_tv.md](Poststrat/poststrat2_tv.md) -
        Poststratification after estimation
-   [ProbabilitySimulation/](ProbabilitySimulation/)
    -   [probsim\_tv.md](ProbabilitySimulation/probsim_tv.md) -
        Simulation of probability models
-   [Pyth/](Pyth/)
-   [Residuals/](Residuals/)
    -   [residuals\_tv.md](Residuals/residuals_tv.md) - Plotting the
        data and fitted model
-   [Restaurant/](Restaurant/)
    -   [restaurant\_tv.md](Restaurant/restaurant_tv.md) - Demonstration
        of using Stan for optimization
-   [RiskyBehavior/](RiskyBehavior/)
    -   [risky\_tv.md](RiskyBehavior/risky_tv.md) Risky behavior data
-   [Roaches/](Roaches/)
    -   [roaches\_tv.md](Roaches/roaches_tv.md) - Analyze the effect of
        integrated pest management on reducing cockroach levels in urban
        apartments
-   [Robit/](Robit/)
    -   [robit\_tv.md](Robit/robit_tv.md) - Comparison of robit and
        logit models for binary data
-   [Rodents/](Rodents/)
-   [Rsquared/](Rsquared/)
    -   [rsquared\_tv.md](Rsquared/rsquared_tv.md) - Bayesian R^2
-   [SampleSize/](SampleSize/)
    -   [simulation\_tv.md](SampleSize/simulation_tv.md) - Sample size
        simulation
-   [Scalability/](Scalability/)
    -   [scalability\_tv.md](Scalability/scalability_tv.md) -
        Demonstrate computation speed with 100,000 observations
-   [Sesame/](Sesame/)
    -   [sesame\_tv.md](Sesame/sesame_tv.md) - Causal analysis of Sesame
        Street experiment
-   [SexRatio/](SexRatio/)
    -   [sexratio\_tv.md](SexRatio/sexratio_tv.md) - Example where an
        informative prior makes a difference
-   [SimpleCausal/](SimpleCausal/)
    -   [causal\_tv.md](SimpleCausal/causal_tv.md) - Simple graphs
        illustrating regression for causal inference
-   [Simplest/](Simplest/)
    -   [simplest\_tv.md](Simplest/simplest_tv.md) - Linear regression
        with a single predictor
-   [Stents/](Stents/)
    -   [stents\_tv.md](Stents/stents_tv.md) - Stents - comparing
        distributions
-   [Storable/](Storable/)
    -   [storable\_tv.md](Storable/storable_tv.md) - Ordered categorical
        data analysis with a study from experimental economics, on the
        topic of “storable votes”
-   [Student/](Student/)
    -   [student\_tv.md](Student/student_tv.md) - Models for regression
        coefficients
-   [Unemployment/](Unemployment/)
    -   [unemployment\_tv.md](Unemployment/unemployment_tv.md) - Time
        series fit and posterior predictive model checking for
        unemployment series
