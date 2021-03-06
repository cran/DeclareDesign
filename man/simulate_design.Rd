% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_design.R
\name{simulate_design}
\alias{simulate_design}
\alias{simulate_designs}
\title{Simulate a design}
\usage{
simulate_design(..., sims = 500)

simulate_designs(..., sims = 500)
}
\arguments{
\item{...}{A design created using the + operator, or a set of designs. You can also provide a single list of designs, for example one created by \code{\link{expand_design}}.}

\item{sims}{The number of simulations, defaulting to 500. If sims is a vector of the form c(10, 1, 2, 1) then different steps of a design will be simulated different numbers of times.}
}
\description{
Runs many simulations of a design and returns a simulations data.frame.
}
\details{
Different steps of a design may each be simulated different a number of times, as specified by sims. In this case simulations are grouped into "fans". The nested 
structure of simulations is recorded in the dataset using a set of variables named "step_x_draw." For example if sims = c(2,1,1,3) is passed to simulate_design, then there
will be two distinct draws of step 1, indicated in variable "step_1_draw" (with values 1 and 2) and there will be three draws for step 4 within each of the step 1 draws, recorded in "step_4_draw" (with values 1 to 6).
}
\examples{
my_model <- 
  declare_model(
    N = 500, 
    U = rnorm(N),
    Y_Z_0 = U, 
    Y_Z_1 = U + rnorm(N, mean = 2, sd = 2)
  )

my_assignment <- declare_assignment(Z = complete_ra(N), legacy = FALSE)

my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))

my_estimator <- declare_estimator(Y ~ Z, inquiry = my_inquiry)

my_reveal <- declare_measurement(Y = reveal_outcomes(Y ~ Z))

design <- my_model +
  my_inquiry +
  my_assignment +
  my_reveal +
  my_estimator

\dontrun{
simulations <- simulate_design(designs, sims = 2)
diagnosis <- diagnose_design(simulations_df = simulations)
}

\dontrun{
# A fixed population with simulations over assignment only
head(simulate_design(design, sims = c(1, 1, 1, 100, 1)))
}

}
