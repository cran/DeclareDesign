## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
set.seed(42)
library(DeclareDesign)
options(digits=2)

## ------------------------------------------------------------------------
# M: Model
custom_population <- function(N) {
  data.frame(u = rnorm(N))
}
custom_potential_outcomes <-
  function(data) {
    within(data,{
      Y_Z_0 <- u
      Y_Z_1 <- 0.25 + u
    })
  }

# I: Inquiry
custom_estimand <- function(data, label) {
  data.frame(estimand_label = label,
  estimand = with(data, median(Y_Z_1 - Y_Z_0)))
}

# D: Data Strategy
custom_sampling <- function(data) {
     data$S <- rbinom(n = nrow(data),
            size = 1,
            prob = 0.1)
     data[data$S == 1, ]
}

custom_assignment <- function(data) {
  data$Z <- rbinom(n = nrow(data),
         size = 1,
         prob = 0.5)
  data
}

custom_reveal <- function(data){
  within(data, Y <- Y_Z_1 * Z + Y_Z_0 * (1 - Z))
}

# A: Answer strategy
custom_estimator <- function(data){
  data.frame(estimate = with(data, mean(Y)))
}

## ------------------------------------------------------------------------
design <- 
  declare_population(handler = custom_population, N = 100) + 
  declare_potential_outcomes(handler = custom_potential_outcomes) + 
  declare_estimand(handler = custom_estimand, label = "medianTE") + 
  declare_sampling(handler = custom_sampling) + 
  declare_assignment(handler = custom_assignment) + 
  declare_reveal(handler = custom_reveal) + 
  declare_estimator(handler = tidy_estimator(custom_estimator), 
                    estimand = "medianTE")
head(draw_data(design))
run_design(design)

