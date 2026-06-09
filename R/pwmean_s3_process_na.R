#' Handle missing values in the outcome and domain variables
#'
#' Identifies rows with missing values in `y` (and `zcol` if supplied),
#' applies the chosen `na.action` strategy, and returns the complete-case
#' subsets of the outcome vector, covariate matrix, and pseudo-weight vector.
#' Also standardizes the domain variable via `standardize_zcol()`.
#'
#' @param sc_data A list returned by `prepare_sc_data()`, with components
#'   `sc`, `X`, `w`, and `idx_keep`.
#' @param y Single character string naming the outcome variable in `sc_data$sc`.
#' @param zcol Single character string naming the domain variable in
#'   `sc_data$sc`, or NULL for the overall mean.
#' @param na.action NA-handling function; one of `stats::na.omit` (default),
#'   `stats::na.exclude`, or `stats::na.fail`. `na.pass` is not supported.
#'
#' @return A list with components:
#'
#' - `Y`: numeric outcome vector (complete cases only).
#' - `X`: covariate matrix (complete cases only).
#' - `w`: pseudo-weight vector (complete cases only).
#' - `sc`: data frame (complete cases only).
#' - `y_name`: the value of `y`.
#' - `zcol`: the value of `zcol`.
#' - `domain`: list returned by `standardize_zcol()`.
#' - `na_info`: list with `na_action`, `n_omitted`, `n_used`,
#'   `omitted_raw`, and `kept_raw`.
#'
#' @keywords internal
process_na_yz <- function(sc_data, y, zcol = NULL, na.action = stats::na.omit) {

  #----------------------------------#
  # Step 1: unpack sc_data
  #----------------------------------#
  sc <- sc_data$sc
  X  <- sc_data$X
  w  <- sc_data$w
  idx_keep <- sc_data$idx_keep

  #----------------------------------#
  # Step 2: prepare raw y / z
  #----------------------------------#
  Y0 <- sc[[y]]

  if (is.null(zcol)) {
    ok <- !is.na(Y0)
  } else {
    Z0 <- sc[[zcol]]
    ok <- !is.na(Y0) & !is.na(Z0)
  }

  #----------------------------------#
  # Step 3: NA policy
  #----------------------------------#
  na_mode <- resolve_na_action(na.action)

  if (na_mode == "pass") {
    stop("`na.pass` is not supported at estimate stage.", call. = FALSE)
  }

  if (na_mode == "fail" && any(!ok)) {
    stop("Missing values detected in `y` or `zcol`.", call. = FALSE)
  }

  if (!any(ok))
    stop("No complete cases remain after removing missing values in 'y' and 'zcol'.",
         call. = FALSE)

  # both na.omit and na.exclude use complete cases for fitting
  sc_cc <- sc[ok, , drop = FALSE]
  Y     <- Y0[ok]
  X_cc  <- X[ok, , drop = FALSE]
  w_cc  <- w[ok]

  omitted_raw <- idx_keep[!ok]

  if (length(omitted_raw) > 0L) {
    na_obj <- omitted_raw
    class(na_obj) <- if (na_mode == "exclude") "exclude" else "omit"
  } else {
    na_obj <- NULL
  }

  #----------------------------------#
  # Step 4: standardize zcol after NA filtering
  #----------------------------------#
  domain_info <- standardize_zcol(data = sc_cc, zcol = zcol)

  #----------------------------------#
  # Step 5: return
  #----------------------------------#
  list(
    Y = Y,
    X = X_cc,
    w = w_cc,
    sc = sc_cc,
    y_name = y,
    zcol = zcol,
    domain = domain_info,
    na_info = list(
      na_action = na_obj,
      n_omitted = sum(!ok),
      n_used = length(Y),
      omitted_raw = omitted_raw,
      kept_raw = idx_keep[ok]
    )
  )
}
