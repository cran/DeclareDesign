% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_designs.R
\name{compare_functions}
\alias{compare_functions}
\alias{compare_designs}
\alias{compare_design_code}
\alias{compare_design_summaries}
\alias{compare_design_data}
\alias{compare_design_estimates}
\alias{compare_design_inquiries}
\title{Compare two designs}
\usage{
compare_designs(
  design1,
  design2,
  format = "ansi8",
  pager = "off",
  context = -1L,
  rmd = FALSE
)

compare_design_code(
  design1,
  design2,
  format = "ansi256",
  mode = "sidebyside",
  pager = "off",
  context = -1L,
  rmd = FALSE
)

compare_design_summaries(
  design1,
  design2,
  format = "ansi256",
  mode = "sidebyside",
  pager = "off",
  context = -1L,
  rmd = FALSE
)

compare_design_data(
  design1,
  design2,
  format = "ansi256",
  mode = "sidebyside",
  pager = "off",
  context = -1L,
  rmd = FALSE
)

compare_design_estimates(
  design1,
  design2,
  format = "ansi256",
  mode = "auto",
  pager = "off",
  context = -1L,
  rmd = FALSE
)

compare_design_inquiries(
  design1,
  design2,
  format = "ansi256",
  mode = "sidebyside",
  pager = "off",
  context = -1L,
  rmd = FALSE
)
}
\arguments{
\item{design1}{A design object, typically created using the + operator}

\item{design2}{A design object, typically created using the + operator}

\item{format}{Format (in console or HTML) options from \code{diffobj::diffChr}}

\item{pager}{Pager option from \code{diffobj::diffChr}}

\item{context}{Context option from \code{diffobj::diffChr} which sets the number of lines around differences that are printed. By default, all lines of the two objects are shown. To show only the lines that are different, set \code{context = 0}; to get one line around differences for context, set to 1.}

\item{rmd}{Set to \code{TRUE} use in Rmarkdown HTML output. NB: will not work with LaTeX, Word, or other .Rmd outputs.}

\item{mode}{Mode options from \code{diffobj::diffChr}}
}
\description{
Compare two designs
}
\examples{

design1 <- declare_model(N = 100, u = rnorm(N), potential_outcomes(Y ~ Z + u)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(S = complete_rs(N, n = 75)) +
  declare_assignment(Z = complete_ra(N, m = 50)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")

  design2 <- declare_model(N = 200, U = rnorm(N),
                         potential_outcomes(Y ~ 0.5*Z + U)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(S = complete_rs(N, n = 100)) +
  declare_assignment(Z = complete_ra(N, m = 25)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, .method = lm_robust, inquiry = "ATE")
 
 if (require("diffobj")) {
 
   compare_designs(design1, design2)
   compare_design_code(design1, design2)
   compare_design_summaries(design1, design2)
   compare_design_data(design1, design2)
   compare_design_estimates(design1, design2)
   compare_design_inquiries(design1, design2)
 
 }

}
