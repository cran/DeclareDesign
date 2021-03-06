context("Design summary")

test_that("Basic design summary", {
  my_population <- declare_population(N = 500, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  my_sampling <- declare_sampling(legacy = FALSE, S = complete_rs(N, n = 250))

  my_assignment <- declare_assignment(legacy = FALSE, Z = complete_ra(N, m = 25))

  my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))

  my_estimator <- declare_estimator(Y ~ Z, inquiry = my_inquiry)

  declare_reveal <- declare_reveal()

  design <- my_population +
    my_potential_outcomes +
    my_sampling +
    my_inquiry +
    declare_step(dplyr::mutate, q = 5) +
    my_assignment +
    declare_reveal +
    my_estimator

  s <- summary(design)

  # First step
  expect_equal(s$N[[1]], "N = 500")
  expect_equal(s$call[[1]], attr(my_population, "call"))

  # Last step
  expect_equal(s$formulae[[8]], Y ~ Z)

  s_short <- summary(design, verbose = FALSE)

  expect_failure(expect_output(print(summary(design, verbose = FALSE)), "Formula"))
})


test_that("Add Quantitites and Alter Variables", {
  my_population <- declare_population(N = 500, noise = rnorm(N))
  my_inquiry <- declare_inquiry(foo = mean(noise))
  my_transform <- declare_population(noise = noise / 2)
  my_inquiry2 <- declare_inquiry(foo2 = mean(noise))


  design <- my_population +
    my_inquiry +
    my_transform +
    my_inquiry2

  # Adding Quantitites
  expect_output(
    print(design), "A single draw of the"
  )

  # Altering variables
  expect_output(
    print(design), "Altered variable: noise "
  )
})

test_that("str() works", {
  expect_output(str(declare_population(N = 50)), "design_step:\\t declare_population[(]N = 50[)] ")
})

test_that("summary, custom estimator handler, numeric value", {
  extra <- declare_estimator(
    handler = function(data)
      mean(data$extra)
  )
  d <- declare_population(sleep) + extra

  expect_output(print(d), "1.54")
})

test_that("summary, estimator formula print formula", {
  extra <- declare_estimator(extra ~ group)
  d <- declare_population(sleep) + extra
  expect_output(print(d), "extra ~ group")
})

test_that("summary, estimator print model", {
  d <- declare_population(sleep) + declare_estimator(extra ~ group, model = lm)
  expect_output(print(d), "Model:\\s*lm")
})
