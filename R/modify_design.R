# index of a step (specified by object, label or position)
find_step <- function(design, step, verb) {
  if (is.numeric(step) && step <= length(design) && step > 0) return(step)
  if (is.character(step)) {
    design <- names(design)
  }
  w <- vapply(design, identical, FALSE, step)

  w <- which(w)
  if (length(w) == 0) {
    stop("Could not find step to ", verb, " in design")
  }

  w[1]
}


#' Modify a design after the fact
#'
#' Insert, delete and replace steps in an (already declared) design object.
#'
#' @param design A design object, usually created using the + operator, \code{\link{expand_design}}, or the design library.
#'
#' @return A new design object.
#'
#' @importFrom rlang call_name quos quo_expr quo_is_missing
#' @name modify_design
#' @rdname modify_design
#'
#' @examples
#'
#'  my_model <- 
#'    declare_model(
#'      N = 100, 
#'      U = rnorm(N),
#'      Y_Z_0 = U,
#'      Y_Z_1 = U + rnorm(N, mean = 2, sd = 2)
#'    )
#'
#'  my_assignment <- declare_assignment(Z = complete_ra(N, m = 50), legacy = FALSE)
#'  my_assignment_2 <- declare_assignment(Z = complete_ra(N, m = 25), legacy = FALSE)
#'
#'  design <- my_model + my_assignment
#'
#'  design
NULL

#' @param before The step before which to add steps.
#' @param after The step after which to add steps.
#' @param new_step The new step; Either a function or a partial call.
#'
#' @details See \code{\link{modify_design}} for details.
#' @rdname modify_design
#' @examples
#'  
#'  \dontrun{
#'  insert_step(design, declare_step(dplyr::mutate, income = noise^2), 
#'              after = my_assignment)
#'  insert_step(design, declare_step(dplyr::mutate, income = noise^2), 
#'              before = my_assignment)
#'  
#'
#'  # If you are using a design created by a designer, for example from
#'  #   the DesignLibrary package, you will not have access to the step
#'  #   objects. Instead, you can always use the label of the step.
#'  
#'  # get the labels for the steps
#'  names(design)
#'  
#'  insert_step(design, 
#'    declare_sampling(S = complete_rs(N, n = 50), 
#'    legacy = FALSE),
#'    after = "my_pop")
#'  }
#'
#' @export
insert_step <- function(design, new_step, before, after) {
  check_design_class_single(design)
  
  if (missing(before)) before <- NULL
  if (missing(after)) after <- NULL
  insert_step_(design, new_step, before, after, enexpr(new_step))
}

insert_step_ <- function(design, new_step, before = NULL, after = NULL, new_step_expr) {
  check_design_class_single(design)
  
  if (is.null(after)) {
    if (is.null(before)) {
      stop("Must provide either before or after to add_step()")
    }
    after <- find_step(design, before, "insert before") - 1
  } else {
    after <- find_step(design, after, "insert after")
  }

  new_step <- wrap_step(new_step, new_step_expr)

  i <- seq_along(design) <= after
  steps <- c(design[i], new_step, design[!i], recursive = FALSE)

  construct_design(steps)
}

#' @param step The quoted label of the step to be deleted or replaced.
#'
#' @export
#' @rdname modify_design
#' @examples
#'
#'  delete_step(design, my_assignment)
delete_step <- function(design, step) {
  check_design_class_single(design)
  
  i <- find_step(design, step, "delete")
  construct_design(design[-i])
}

#' @export
#' @rdname modify_design
#' @examples
#'  replace_step(design, my_assignment, declare_step(dplyr::mutate, words = "income"))
replace_step <- function(design, step, new_step) {
  check_design_class_single(design)
  
  i <- find_step(design, step, "replace")
  new_step <- wrap_step(new_step, enexpr(new_step))
  design[i] <- new_step
  names(design)[i] <- names(new_step)

  construct_design(design)
}
