context("Demo")

test_that("demo runs", {

  ## ------------------------------------------------------------------------
  my_population <-
    declare_model(
      N = 1000,
      income = rnorm(N),
      age = sample(18:95, N, replace = T)
    )

  pop <- my_population()
  expect_equal(nrow(pop), 1000)
  expect_equal(colnames(pop), c("ID", "income", "age"))

  ## ------------------------------------------------------------------------
  my_population_nested <- declare_model(
    districts = add_level(N = 25, urban = sample(0:1, N, replace = TRUE)),
    villages = add_level(N = 10, altitude = rnorm(N)),
    individuals = add_level(
      N = sample(100:200, size = 250, replace = TRUE),
      income = rnorm(N),
      age = sample(18:95, N, replace = TRUE)
    )
  )


  ## ------------------------------------------------------------------------
  region_data <- data.frame(capital = c(1, 0, 0, 0, 0))
  pop_level_data <- declare_model(
    regions = add_level(N = 2, gdp = runif(N)),
    cities = add_level(N = 2, subways = rnorm(N, mean = 5))
  )

  head(pop_level_data())

  ## ------------------------------------------------------------------------
  country_data <- data.frame(
    cow_code = c(504, 15, 100, 90),
    polity_iv = c(-9, 7, -1, 3)
  )
  pop_data <- declare_model(data = country_data)

  head(pop_data())

  ## ------------------------------------------------------------------------
  pop_data_bootstrap <- declare_model(
    data = country_data, handler = fabricatr::resample_data
  )

  head(pop_data_bootstrap())

  ## ------------------------------------------------------------------------
  my_potential_outcomes <- declare_potential_outcomes(
    formula = Y ~ .25 * Z + .01 * age * Z
  )
  pop_pos <- my_potential_outcomes(pop)
  head(pop_pos)

  ## ------------------------------------------------------------------------
  my_potential_outcomes <- declare_potential_outcomes(
    formula = Y ~ .25 * Z + .01 * age * Z,
    conditions = 1:4
  )
  head(my_potential_outcomes(pop))

  ## ------------------------------------------------------------------------
  my_potential_outcomes <-
    declare_potential_outcomes(
      Y_Z_0 = .05,
      Y_Z_1 = .30 + .01 * age
    )

  head(my_potential_outcomes(pop))

  ## ------------------------------------------------------------------------
  my_sampling <- declare_sampling(S = complete_rs(N, n = 250))
  smp <- my_sampling(pop_pos)
  nrow(smp)

  ## ------------------------------------------------------------------------
  my_assignment <- declare_assignment(Z = complete_ra(N, m = 25))
  smp <- my_assignment(smp)
  table(smp$Z)
  head(smp)

  ## ------------------------------------------------------------------------
  my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
  my_inquiry(pop_pos)

  ## ------------------------------------------------------------------------
  my_measurement <- declare_measurement(Y = reveal_outcomes(Y ~ Z)) 
  smp <- my_measurement(smp)
  my_estimator_dim <- declare_estimator(Y ~ Z, inquiry = my_inquiry)
  est <- my_estimator_dim(smp)
  expect_equal(dim(est), c(1, 11))
  expect_equal(colnames(est), c("estimator", "term", "estimate", "std.error", "statistic", 
                                "p.value", "conf.low", "conf.high", "df", "outcome", "inquiry"))
  
  ## ------------------------------------------------------------------------
  my_estimator_lm <-
    declare_estimator(Y ~ Z,
      .method = estimatr::lm_robust,
      term = "Z",
      inquiry = my_inquiry
    )

  est <- my_estimator_lm(smp)
  expect_equal(dim(est), c(1, 11))
  expect_equal(colnames(est), c("term", "estimator", "estimate", "std.error", "statistic", 
                                "p.value", "conf.low", "conf.high", "df", "outcome", "inquiry"))

  ## ------------------------------------------------------------------------
  design <- my_population +
    my_potential_outcomes +
    my_inquiry +
    my_sampling +
    my_assignment +
    my_measurement +
    my_estimator_dim

  ## ------------------------------------------------------------------------
  dat <- draw_data(design)
  head(dat)

  ## ------------------------------------------------------------------------
  draw_estimates(design)
  draw_estimands(design)

  ## ------------------------------------------------------------------------
  my_population_function <- function(N) {
    data.frame(u = rnorm(N))
  }

  my_population_custom <- declare_model(
    handler = my_population_function, N = 100
  )

  pop_custom <- my_population_custom()

  head(pop_custom)

  ## ------------------------------------------------------------------------
  my_potential_outcomes_function <-
    function(data) {
      data$Y_Z_0 <- with(data, u)
      data$Y_Z_1 <- with(data, 0.25 + u)
      data
    }
  my_potential_outcomes_custom <- declare_potential_outcomes(
    handler = my_potential_outcomes_function
  )

  pop_pos_custom <- my_potential_outcomes_custom(pop_custom)

  head(pop_pos_custom[, c("u", "Y_Z_0", "Y_Z_1")])

  ## ------------------------------------------------------------------------
  my_sampling_function <- function(data) {
    data$S <- rbinom(
      n = nrow(data),
      size = 1,
      prob = 0.1
    )
    data[data$S == 1, ]
  }

  my_sampling_custom <- declare_sampling(
    handler = my_sampling_function
  )

  smp_custom <- my_sampling_custom(pop_pos)

  nrow(smp_custom)

  ## ------------------------------------------------------------------------
  my_assignment_function <- function(data) {
    data$Z <- rbinom(
      n = nrow(data),
      size = 1,
      prob = 0.5
    )
    data
  }
  my_assignment_custom <- declare_assignment(
    handler = my_assignment_function
  )

  table(my_assignment_custom(pop_pos)$Z)

  ## ------------------------------------------------------------------------
  my_inquiry_function <- function(data) {
    with(data, median(Y_Z_1 - Y_Z_0))
  }
  my_inquiry_custom <- declare_inquiry(
    handler = my_inquiry_function, label = "medianTE"
  )

  my_inquiry_custom(pop_pos)

  ## ------------------------------------------------------------------------
  my_mean <- function(data) {
    data.frame(estimate = with(data, mean(Y)))
  }

  my_estimator_custom <-
    declare_estimator(
      handler = label_estimator(my_mean),
      inquiry = my_inquiry
    )

  my_estimator_custom(smp)

  ## ------------------------------------------------------------------------
  m_arm_trial <- function(numb) {
    my_population <- declare_model(
      N = numb, income = rnorm(N), age = sample(18:95, N, replace = T)
    )

    my_potential_outcomes <- declare_potential_outcomes(
      formula = Y ~ .25 * Z + .01 * age * Z
    )
    my_sampling <- declare_sampling(S = complete_rs(N, n = 250))
    my_assignment <- declare_assignment(Z = complete_ra(N, m = 25))
    my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
    my_estimator_dim <- declare_estimator(Y ~ Z, inquiry = my_inquiry)
    my_design <- my_population +
      my_potential_outcomes +
      my_inquiry +
      my_sampling +
      my_assignment +
      my_measurement +
      my_estimator_dim
    return(my_design)
  }

  my_1000_design <- expand_design(designer = m_arm_trial, numb = 1000)
  head(draw_data(my_1000_design))

  ## ------------------------------------------------------------------------
  my_potential_outcomes_continuous <- declare_potential_outcomes(
    formula = Y ~ .25 * Z + .01 * age * Z, conditions = seq(0, 1, by = .1)
  )

  my_assignment_continuous <- declare_assignment(Z = complete_ra(N, conditions = seq(0, 1, by = .1)))

  my_design <- declare_model(my_population()) +
    my_potential_outcomes_continuous +
    my_assignment_continuous +
    my_measurement

  head(draw_data(my_design))

  ## ------------------------------------------------------------------------
  my_potential_outcomes_attrition <- declare_potential_outcomes(
    formula = R ~ rbinom(n = N, size = 1, prob = pnorm(Y_Z_0))
  )

  my_design <- declare_model(my_population()) +
    my_potential_outcomes +
    my_potential_outcomes_attrition +
    my_assignment +
    declare_reveal(outcome_variables = "R") +
    declare_reveal(attrition_variables = "R")

  head(draw_data(my_design)[, c("ID", "Y_Z_0", "Y_Z_1", "R_Z_0", "R_Z_1", "Z", "R", "Y")])

  ## ------------------------------------------------------------------------
  stochastic_population <- declare_model(
    N = sample(500:1000, 1), income = rnorm(N), age = sample(18:95, N, replace = TRUE)
  )

  c(
    nrow(stochastic_population()),
    nrow(stochastic_population()),
    nrow(stochastic_population())
  )

  ## ------------------------------------------------------------------------

  my_population_function <- function(N) {
    data.frame(u = rnorm(N))
  }

  ## set a variable used in the declaration
  my_N <- 1000
  my_population_custom <- declare_model(
    handler = my_population_function, N = my_N
  )

  my_potential_outcomes_function <-
    function(data) {
      data$Y_Z_0 <- with(data, u)
      data$Y_Z_1 <- with(data, 0.25 + u)
      data
    }
  my_potential_outcomes_custom <- declare_potential_outcomes(
    handler = my_potential_outcomes_function
  )

  ## remove all objects except your pop and PO functions
  rm(list = ls()[-which(ls() %in% c("my_potential_outcomes_custom", "my_population_custom"))])

  pop_pos_custom <- my_potential_outcomes_custom(my_population_custom())

  head(pop_pos_custom[, c("u", "Y_Z_0", "Y_Z_1")])
})

