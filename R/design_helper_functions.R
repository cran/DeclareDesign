#' Explore your design
#'
#' @param design A design object, typically created using the + operator
#'
#' @examples
#' 
#' # Two-arm randomized experiment
#' design <-
#'   declare_model(
#'     N = 500,
#'     gender = rbinom(N, 1, 0.5),
#'     X = rep(c(0, 1), each = N / 2),
#'     U = rnorm(N, sd = 0.25),
#'     potential_outcomes(Y ~ 0.2 * Z + X + U)
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(S = complete_rs(N = N, n = 200)) +
#'   declare_assignment(Z = complete_ra(N = N, m = 100)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE")
#' 
#' # Use draw_data to create a dataset using a design
#' dat <- draw_data(design)
#' 
#' draw_data(design, data = dat, start = 2)
#' 
#' # Apply get_estimates
#' get_estimates(design, data = dat)
#'
#' @name post_design
NULL


# For fan-out execution, convert the vector representation to (end, n) pairs

check_sims <- function(design, sims) {
  n <- length(design)
  if (!is.data.frame(sims)) {
    if (length(sims) == n) {
      sims_full <- sims
    }
    else if (is.character(names(sims))) {
      sims_full <- rep(1, n)
      designs <- as.character(lapply(design, attr, "label"))
      i <- match(names(sims), designs)
      sims_full[i] <- sims
    } else if (length(sims) != n) {
      sims_full <- c(sims, rep(1, n))[1:n]
    }

    ret <- data.frame(end = 1:n, n = sims_full)
  }

  # Compress sequences of ones into one partial execution
  
  if(n > 1) {
    j <- 1
    for(i in 2:n){
      k <- ret[i, "n"]
      if(k > 1) {
        #keeper
        j <- j + 1
        ret[j,] <- c(i,k) 
      } else if(k == 1) {
        ret[j, "end"] <- i
      }
    }
    ret <- ret[1:j, , drop=FALSE]
  }
  
  ret
}

#' Run a design one time
#'
#' @param design a DeclareDesign object
#'
#' @examples 
#'
#' # Two-arm randomized experiment
#' design <-
#'   declare_model(
#'     N = 500,
#'     gender = rbinom(N, 1, 0.5),
#'     X = rep(c(0, 1), each = N / 2),
#'     U = rnorm(N, sd = 0.25),
#'     potential_outcomes(Y ~ 0.2 * Z + X + U)
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(S = complete_rs(N = N, n = 200)) +
#'   declare_assignment(Z = complete_ra(N = N, m = 100)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE")
#' 
#' # Use run_design to run a design object
#' run_design(design)
#'
#' @export
run_design <- function(design){
  ret <- simulate_single_design(design, sims = 1, low_simulations_warning = FALSE)
  ret$sim_ID <- NULL
  return(ret)
}

run_design_internal <- function(design, ...) UseMethod("run_design_internal", design)

next_step <- function(step, current_df, i) {
  tryCatch(
    nxt <- step(current_df),
    error = function(err) {
      stop(simpleError(sprintf("Error in step %d (%s):\n\t%s", i, attr(step, "label") %||% "", err)))
    }
  )
  nxt
}

#' @noRd
run_design_internal.default <- function(design, ...) {
  stop("Please only send design objects to run_design.")
}

#' @noRd
run_design_internal.design <- function(design, current_df = NULL, results = NULL, start = 1, end = length(design), ...) {
  if (!is.list(results)) {
    results <- list(
      inquiry = vector("list", length(design)),
      estimator = vector("list", length(design))
    )
  }

  for (i in seq(start, end)) {
    step <- design[[i]]

    causal_type <- attr(step, "causal_type")
    step_type <- attr(step, "step_type")

    # if it's a dgp
    if ("dgp" %in% causal_type) {
      current_df <- next_step(step, current_df, i)
    } else if (step_type %in% names(results)) {
      results[[step_type]][[i]] <- next_step(step, current_df, i)
    } else {
      NULL # skipping steps not in the requested results types
    }
  }

  if (i == length(design)) {
    if ("estimator" %in% names(results)) {
      results[["estimates_df"]] <- rbind_disjoint(results[["estimator"]])
      results[["estimator"]] <- NULL
    }
    if ("inquiry" %in% names(results)) {
      results[["inquiries_df"]] <- rbind_disjoint(results[["inquiry"]])
      results[["inquiry"]] <- NULL
    }
    if ("current_df" %in% names(results)) {
      results[["current_df"]] <- current_df
    }
    append(results, list(...))
    
  } else {
    execution_st(
      design = design,
      current_df = current_df,
      results = results,
      start = i + 1,
      end = length(design),
      ...
    )
  }
}

#' @noRd
run_design_internal.execution_st <- function(design, ...) do.call(run_design_internal.design, design)

# Build an execution strategy object
#
# @param design a design
# @param current_df a data.frame
# @param results a list of intermediate results
# @param start index of starting step
# @param end  index of ending step
execution_st <- function(design, current_df = NULL, results = NULL, start = 1, end = length(design), ...) {
  # An execution state are the arguments needed to run run_design
  structure(
    list(
      design = design,
      current_df = current_df,
      results = results,
      start = start,
      end = end,
      ...
    ),
    class = "execution_st"
  )
}


apply_on_design_dots <- function(FUN, ...) {
  designs <- dots_to_list_of_designs(...)

  elist <- lapply(designs, FUN)

  if (length(designs) > 1) {
    elist <- Map(cbind, design = names(elist), elist, stringsAsFactors = FALSE)
  }

  rbind_disjoint(elist)
}

dots_to_list_of_designs <- function(...) {
  dotqs <- enquos(...)
  
  if (length(dotqs) == 0){
    stop("Please provide at least one design.", call. = FALSE)
  }
  
  d1 <- eval_tidy(dotqs[[1]])

  ## Two cases:
  ## 1. send one or more design objects created by the + operator
  ## 2. send a single list of design objects e.g. created by expand_design
  ## Approach: unpack designs if a list of designs was sent as a single list object
  if (length(dotqs) == 1 &&
    is.list(d1) &&
    !inherits(d1, "design")) {
    designs <- d1
    names(designs) <- infer_names(designs)
  } else {
    names(dotqs) <- infer_names(dotqs)
    designs <- eval_tidy(quo(list(!!!dotqs)))
  }

  # do not allow users to send more than one object if any is not a design object
  check_design_class(designs)

  designs
}

#' Print code to recreate a design
#'
#' @examples
#'
#' # Two-arm randomized experiment
#' design <-
#'   declare_model(
#'     N = 500,
#'     gender = rbinom(N, 1, 0.5),
#'     X = rep(c(0, 1), each = N / 2),
#'     U = rnorm(N, sd = 0.25),
#'     potential_outcomes(Y ~ 0.2 * Z + X + U)
#'   ) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(S = complete_rs(N = N, n = 200)) +
#'   declare_assignment(Z = complete_ra(N = N, m = 100)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE")
#' 
#' print_code(design)
#' 
#' summary(design)
#'
#' @rdname post_design
#'
#' @export
print_code <- function(design) {
  
  check_design_class_single(design)
  
  # if there is not a code attribute, construct code via the calls for each step
  #   and the call for the declare step
  
  if (is.null(attributes(design)$code)) {
    clean_call <- function(call) {
      paste(sapply(deparse(call), trimws), collapse = " ")
    }
    
    # print each step
    
    for (i in seq_along(design)) {
      # only print steps that are not calls within the design call i.e. mutate(q = 5)
      if (inherits(attributes(design[[i]])$call, "call")) {
        cat(names(design)[i], "<-", clean_call(attributes(design[[i]])$call), "\n\n")
      }
    }
    
    # print the design declaration
    
    cat("my_design <-", clean_call(attributes(design)$call), "\n\n")
  } else {
    print(attributes(design)$code)
  }
}

#' Obtain the preferred citation for a design
#'
#' @param design a design object created using the + operator
#'
#' @param ... options for printing the citation if it is a BibTeX entry
#'
#' @export
cite_design <- function(design, ...) {
  
  check_design_class_single(design)
  
  citation <- attr(design, "citation")
  if (inherits(citation, "bibentry")) {
    print(citation, style = "bibtex", ... = ...)
  } else {
    print(citation, style = "text", ... = ...)
  }
}

#' @export
print.design_step <- function(x, ...) {
  print(attr(x, "call"))
}

#' @export
str.design_step <- function(object, ...) cat("design_step:\t", paste0(deparse(attr(object, "call"), width.cutoff = 500L), collapse = ""), "\n")

make_fan_counter <- function(fan) {
  k <- nrow(fan)
  ret <- matrix(0, 1, k)
  colnames(ret) <- sprintf("step_%d_draw", c(1, fan$end+1)[1:k])
  
  ret
}

# A wrapper around conduct design for fan-out execution strategies
fan_out <- function(design, fan) {
  st <- list(execution_st(design, fan=make_fan_counter(fan)))

  for (i in seq_len(nrow(fan))) {
    end <- fan[i, "end"]
    n <- fan[i, "n"]

    for (j in seq_along(st))
      st[[j]]$end <- end

    st <- st [ rep(seq_along(st), each = n) ]

    for (j in seq_along(st))
      st[[j]]$fan[i] <- j
    
    
    st <- future_lapply(seq_along(st), function(j) run_design_internal(st[[j]]), future.seed = NA, future.globals = "st")
  }
  
  st <- lapply(st, function(x){
    fan <- x$fan
    x$fan <- NULL
    lapply(x, function(x, z=nrow(x)) if(z > 0) cbind(x,fan) else x)
  })
  

  st
}
