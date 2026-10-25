## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
set.seed(343)

## -----------------------------------------------------------------------------
library(DeclareDesign)

## -----------------------------------------------------------------------------
simple_random_assignment <- declare_assignment(Z = simple_ra(N = N, prob = 0.6))

## -----------------------------------------------------------------------------
units <- tibble::tibble(ID = 1:6)

simple_random_assignment(units)

simple_random_assignment(units)

## -----------------------------------------------------------------------------
n_units <- 100

declaration <-
  declare_model(
    N = n_units,
    U = rnorm(N),
    potential_outcomes(Y ~ 0.2 * Z + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1) - mean(Y_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, .method = difference_in_means, inquiry = "ATE")

## -----------------------------------------------------------------------------
head(draw_data(declaration))

## -----------------------------------------------------------------------------
draw_estimands(declaration)

draw_estimates(declaration)

## ----include = FALSE----------------------------------------------------------
set.seed(343)

## -----------------------------------------------------------------------------
diagnosis <- diagnose_design(declaration, sims = 500)

diagnosis

## ----include = FALSE----------------------------------------------------------
set.seed(343)

## -----------------------------------------------------------------------------
designs <- redesign(declaration, n_units = c(100, 200, 300, 400, 500))

diagnose_design(designs, sims = 500)

