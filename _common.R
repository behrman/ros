
# Parameters

# Set seed for random number generation
set.seed(42)

# General options
options(
  digits = 3,
  dplyr.print_max = 10,
  dplyr.print_min = 10,
  dplyr.summarise.inform = FALSE
)

# knitr options
knitr::opts_chunk$set(
  comment = "#>",
  collapse = FALSE,
  fig.asp = 0.618,  # 1 / phi
  fig.show = "hold",
  out.width = "100%"
)

#===============================================================================

# Get samples from stanfit object
as_tibble.stanfit <- function(x, ...) {
  as_tibble(as.data.frame(x, ...))
}

# Predictive intervals for fit model
predictive_intervals <- function(.data, fit, probs = c(0.5, 0.9)) {
  .data %>%
    mutate(.pred = predict(fit, newdata = .)) %>%
    bind_cols(
      map_dfc(
        probs, 
        ~ predictive_interval(fit, prob = ., newdata = .data) %>%
          as_tibble()
      )
    )
}

# Sequence of evenly spaced points spanning the range of x
seq_range <- function(x, n = 201) {
  seq(from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE), length.out = n)
}
