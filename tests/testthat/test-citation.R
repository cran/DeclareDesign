context("add design citation")

test_that("test with generated citation", {

  design <- declare_model(data = sleep) + declare_sampling(S = complete_rs(N, n = 10)) + declare_inquiry(Q = mean(S))

  design <- set_citation(design,
    author = "Lovelace, Ada",
    title = "Notes",
    year = 1953,
    description = "This is a text description of a design"
  )
  
  expect_output(print(design), "Lovelace")

  expect_output(cite <- cite_design(design), "Ada")

  expect_equal(
    cite,
    structure(list(structure(list(
      title = "Notes",
      author = structure(list(
        list(
          given = NULL, family = "Lovelace",
          role = NULL, email = NULL, comment = NULL
        ),
        list(
          given = NULL, family = "Ada",
          role = NULL, email = NULL, comment = NULL
        )
      ),
      class = "person"
      ),
      note = "This is a text description of a design",
      year = "1953"
    ), bibtype = "Unpublished")), class = "bibentry")
  )
})

test_that("test with user-specified text citation", {
  text <- "Set of authors (2017). My custom design."

  design <- declare_model(data = sleep) + declare_inquiries(Q = 5)

  design <- set_citation(design, citation = text)

  expect_output(cite <- cite_design(design), paste0('[1] "', text, '"'), fixed = TRUE)
  expect_equal(cite, text)
  
  expect_output(print(design), "Citation:")
})

