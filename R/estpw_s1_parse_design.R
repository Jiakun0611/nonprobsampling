#' Parse the data list passed to est_pw
#'
#' Splits the input list into its two parts: the nonprobability sample
#' (`sc`) and one or more reference survey designs (`sp_des`).
#' For each design it calls `extract_analysis_data()` to remove design
#' variables and attach a `sp_wts` column, giving back a plain data
#' frame ready for modeling. If the list elements have no names, or if
#' any element name is an empty string (partially named list), default
#' names `"sp[[1]]"`, `"sp[[2]]"`, ... are assigned to all elements.
#'
#' @param data A named or unnamed list. The first element is the
#'   nonprobability sample as a data frame. Every remaining element is a
#'   survey design object (`survey.design2`, `survey.design`, or
#'   `svyrep.design`).
#'
#' @return A list with four elements:
#'
#' - `sc`: the nonprobability sample (first element of `data`).
#' - `sp_des`: named list of the survey design objects.
#' - `sp_vars`: named list of plain data frames, one per reference survey,
#'   with design variables removed and a `sp_wts` column added.
#' - `n_ref`: integer giving the number of reference surveys.
#'
#' @keywords internal
parse_ipwm_data <- function(data) {

  sc     <- data[[1]]
  sp_des <- data[-1]

  ref_names <- names(sp_des)
  if (is.null(ref_names) || any(!nzchar(ref_names))) {
    ref_names <- paste0("sp[[", seq_along(sp_des), "]]")
  }

  sp_vars <- lapply(seq_along(sp_des), function(i) {
    tryCatch(
      extract_analysis_data(sp_des[[i]]),
      error = function(e) {
        stop(
          sprintf("Failed to parse reference survey %d: %s", i, e$message),
          call. = FALSE
        )
      }
    )
  })

  names(sp_des)  <- ref_names
  names(sp_vars) <- ref_names

  list(
    sc      = sc,
    sp_des  = sp_des,
    sp_vars = sp_vars,
    n_ref   = length(sp_des)
  )
}
