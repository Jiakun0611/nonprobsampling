#' Construct block design matrices for multi-reference calibration
#'
#' @param vars_XC Character vector of all predictor variables (union across
#'   reference samples), used to build the `sc` design matrix `Xc`.
#' @param sc Data frame. The nonprobability sample.
#' @param sp_list List of reference sample data frames.
#' @param xcol List of character vectors: the variables contributed by each
#'   reference sample (as returned by `check_input_multi`).
#' @param wts_cols Character vector of survey-weight column names,
#'   one per element of `sp_list`.
#'
#' @return A list with components:
#'   \item{Xc}{Numeric design matrix for `sc`, with intercept prepended,
#'     of dimension \eqn{n_c \times p}.}
#'   \item{Xp_list}{List of numeric design matrices for each reference sample.
#'     The first block includes an intercept column; subsequent blocks do not.}
#'   \item{wts_list}{List of numeric weight vectors, one per reference sample.}
#'
#' @keywords internal
Xc_Xp_Construction <- function(vars_XC, sc, sp_list, xcol, wts_cols) {
  # -----------------------------
  # basic checks
  # -----------------------------
  if (!is.data.frame(sc)) {
    stop("`sc` must be a data.frame.", call. = FALSE)
  }

  if (!is.list(sp_list) || length(sp_list) == 0L) {
    stop("`sp_list` must be a non-empty list of reference samples.", call. = FALSE)
  }

  n_ref <- length(sp_list)

  if (!is.list(xcol) || length(xcol) != n_ref) {
    stop("`xcol` must be a list with the same length as `sp_list`.", call. = FALSE)
  }

  if (!is.character(wts_cols) || length(wts_cols) != n_ref) {
    stop("`wts_cols` must be a character vector with the same length as `sp_list`.", call. = FALSE)
  }

  # -----------------------------
  # build Xc
  # -----------------------------
  Xc <- add_intercept(vars_XC, data = sc, intercept = TRUE)
  check_design_identifiability(Xc, label = "Multi_Calibration")

  # -----------------------------
  # build Xp_list and wts_list
  # -----------------------------
  Xp_list  <- vector("list", n_ref)
  wts_list <- vector("list", n_ref)

  for (i in seq_len(n_ref)) {
    sp_i <- sp_list[[i]]

    if (!is.data.frame(sp_i)) {
      stop(sprintf("`sp_list[[%d]]` must be a data.frame.", i), call. = FALSE)
    }

    if (!(wts_cols[i] %in% names(sp_i))) {
      stop(
        sprintf("Weight column '%s' not found in `sp_list[[%d]]`.", wts_cols[i], i),
        call. = FALSE
      )
    }

    # intercept only in the first reference sample
    Xp_i <- add_intercept(
      vars      = xcol[[i]],
      data      = sp_i,
      intercept = (i == 1)
    )


    check_design_identifiability(Xp_i, label = "Multi_Calibration")


    w_i <- sp_i[[wts_cols[i]]]

    if (!is.numeric(w_i)) {
      stop(
        sprintf("Weight column '%s' in `sp_list[[%d]]` must be numeric.", wts_cols[i], i),
        call. = FALSE
      )
    }

    if (anyNA(w_i)) {
      stop(
        sprintf("Weight column '%s' in `sp_list[[%d]]` contains NA.", wts_cols[i], i),
        call. = FALSE
      )
    }

    if (any(!is.finite(w_i))) {
      stop(
        sprintf("Weight column '%s' in `sp_list[[%d]]` contains non-finite values.", wts_cols[i], i),
        call. = FALSE
      )
    }

    if (any(w_i <= 0)) {
      stop(
        sprintf("Weight column '%s' in `sp_list[[%d]]` must be strictly positive.", wts_cols[i], i),
        call. = FALSE
      )
    }

    Xp_list[[i]]  <- Xp_i
    wts_list[[i]] <- w_i
  }

  # -----------------------------
  # dimension consistency check
  # In multi-raking, columns of Xc should match the total number of columns
  # across all Xp blocks (first block contains intercept).
  # -----------------------------
  p_xc <- ncol(Xc)
  p_xp <- sum(vapply(Xp_list, ncol, integer(1)))

  if (p_xc != p_xp) {
    stop(
      paste0(
        "Column mismatch between `Xc` and combined `Xp_list`: ",
        "ncol(Xc) = ", p_xc, ", but sum(ncol(Xp_list[[i]])) = ", p_xp, ". ",
        "Please check `vars_XC` and `xcol` construction."
      ),
      call. = FALSE
    )
  }

  return(list(
    Xc       = Xc,
    Xp_list  = Xp_list,
    wts_list = wts_list
  ))
}
