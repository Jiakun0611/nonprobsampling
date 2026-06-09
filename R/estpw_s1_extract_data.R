#' Extract analysis data from a survey design object
#'
#' Remove design variables (cluster IDs, strata, FPC, probabilities, weights)
#' from a survey design object and attaches the sampling weights as a single
#' column, returning a plain data frame ready for modeling.
#'
#' Cluster and strata variables are identified from the design object's own
#' fields (`des$cluster`, `des$strata`) rather than the original call, so the
#' result is correct even when `des` has been subset after construction.
#' Weights, FPC, and probability variables are still read from `des$call`.
#' For replicate designs (`svyrep.design`), replicate weight columns are also
#' dropped.
#'
#' @param des A survey design object of class `survey.design2`
#'   or `svyrep.design`.
#' @param weight_name A single string giving the name of the weight column
#'   added to the output. Defaults to `"sp_wts"`. Must not already exist
#'   among the analysis variables after design variables are removed.
#'
#' @return A data frame containing the analysis variables from `des$variables`
#'   (design variables removed) plus one column named `weight_name` storing
#'   the sampling weights.
#'
#' @keywords internal
extract_analysis_data <- function(des, weight_name = "sp_wts") {

  if (!inherits(des, c("survey.design2", "svyrep.design"))) {
    stop("`des` must be a survey design object.", call. = FALSE)
  }

  vars <- des$variables

  if (is.null(vars) || !is.data.frame(vars)) {
    stop("`des$variables` is missing or not a data.frame.", call. = FALSE)
  }

  get_call_vars <- function(x) {
    if (is.null(x)) return(character(0))
    out <- tryCatch(all.vars(x), error = function(e) character(0))
    setdiff(out, "1")
  }

  # Use design fields for cluster/strata -- robust after subsetting
  cluster_vars <- if (!is.null(des$cluster)) {
    setdiff(names(des$cluster), "1")
  } else {
    get_call_vars(des$call$ids)
  }

  strata_vars <- if (!is.null(des$strata)) {
    setdiff(names(des$strata), "1")
  } else {
    get_call_vars(des$call$strata)
  }

  # Weights, FPC, probs have no direct name field -- parse from call
  call_vars <- unique(unlist(lapply(
    list(des$call$fpc, des$call$probs, des$call$weights),
    get_call_vars
  )))

  drop_vars <- unique(c(cluster_vars, strata_vars, call_vars))

  if (inherits(des, "svyrep.design")) {
    rep_cols <- tryCatch(colnames(des$repweights), error = function(e) NULL)
    if (!is.null(rep_cols)) {
      drop_vars <- unique(c(drop_vars, rep_cols))
    }
  }

  drop_vars <- intersect(drop_vars, names(vars))
  out <- vars[, !(names(vars) %in% drop_vars), drop = FALSE]

  if (weight_name %in% names(out)) {
    stop(sprintf(
      "Column '%s' already exists in the analysis data. Use a different `weight_name`.",
      weight_name
    ), call. = FALSE)
  }

  if (inherits(des, "svyrep.design")) {
    out[[weight_name]] <- as.numeric(weights(des, type = "sampling"))
  } else {
    out[[weight_name]] <- as.numeric(weights(des))
  }

  out
}
