context("declare design")

test_that("test the custom execution strategy", {

  # closes ticket #62

  design <- declare_model(sleep) + declare_estimator(extra ~ group)

  my_sleep <- sleep
  my_sleep$extra <- my_sleep$extra + 1 * (my_sleep$group == 1)

  exst <-
    execution_st(design,
      current_df = my_sleep,
      results = list(estimator = vector(mode = "list", length = 2)),
      2, 2
    )


  regular <- run_design(design)
  output <- run_design(exst)

  expect_equal(
    regular$estimate,
    output$estimate + 1
  )
  expect_true(!"estimand" %in% names(output)) # no inquiries
})


test_that("test error messages in run_design", {

  # closes ticket #12
  design <- declare_model(sleep) + declare_model(foo = bar)

  expect_error(run_design(design), "Error in step 2")
})

test_that("draw_data does not run inquiry/estimator", {

  # closes ticket #12
  design <- declare_model(sleep) +
    declare_inquiry(
      "Should not be run",
      handler = function(data, msg)
        stop(x)
    )

  expect_identical(draw_data(design), sleep)
})
