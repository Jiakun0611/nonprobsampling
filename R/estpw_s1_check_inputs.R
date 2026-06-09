#' Validate IPWM build-stage inputs
#'
#' Performs structural validation of all arguments passed to
#' `est_pw()` before any computation begins. Checks cover
#' `verbose`, `sc_wname`, the `data` list structure,
#' the survey design objects, reference-sample row counts, `method`
#' consistency with the number of reference samples, and
#' `p_formula` variable availability.
#'
#' Specifically, this function checks that:
#'
#' - `verbose` is a single TRUE/FALSE value;
#' - `sc_wname` is a single non-empty character string that does
#'   not already exist as a column in `sc`;
#' - `data` is a list of the form `list(sc, sp1_design, sp2_design, ...)`;
#' - the first element of `data` is a non-empty data frame for
#'   `sc` with no duplicated column names;
#' - all remaining elements of `data` are valid survey design
#'   objects, either `survey.design2` or `svyrep.design`;
#' - each reference sample has at least one row;
#' - lonely PSU problems are detected for standard survey design objects;
#' - `method`, if supplied, is one of `"alp"`, `"clw"`,
#'   `"multi"`, `"cali"`, or `"calibration"`;
#' - `method = "multi"` is only used with multiple reference samples;
#' - one-reference methods are only used with a single reference sample;
#' - in the one-reference case, `p_formula` is either NULL or a
#'   one-sided formula; a two-sided formula raises an error; a warning
#'   is issued if `sp_order` differs from its default (`"size"`) or if
#'   `precali` is FALSE, since both are ignored in the one-reference case;
#' - in the multi-reference case, `sp_order` must be either
#'   `"size"` or `"given"`;
#' - in the multi-reference case, `precali` must be a single
#'   TRUE or FALSE value;
#' - in the multi-reference case, `p_formula`, if supplied, is a
#'   list of one-sided formulas with one formula per reference sample;
#'   any two-sided formula in the list raises an error;
#' - all variables used in each participation model formula are present
#'   in both `sc` and the corresponding reference sample.
#'
#' This function does not check outcome variables or domain variables. Those
#' are validated later at the estimation stage by
#' `check_ipwm_inputs_estimate()`.
#'
#' `method` is expected to be normalized early (`tolower` +
#' `trimws`, with `"cali"` already converted to `"calibration"`)
#' by the caller (`est_pw()`).
#'
#' @param data A list of input data objects. The first element must be a
#'   non-empty data frame with unique column names (`sc`). All remaining
#'   elements must be `"survey.design2"` or `"svyrep.design"`
#'   objects with at least one row in their analysis data.
#' @param p_formula Participation model formula. Must be one-sided (no
#'   left-hand side); a two-sided formula raises an error. For a single
#'   reference sample, supply a formula or NULL. For multiple reference
#'   samples, supply a list of one-sided formulas with one formula per
#'   reference sample, or NULL. When non-NULL, all variables in each formula
#'   must be present in both `sc` and the corresponding reference sample data.
#' @param method Pseudo-weighting method, or NULL. Expected to be
#'   pre-normalised by the caller. Supported values are `"alp"`,
#'   `"clw"`, `"multi"`, `"cali"`, and `"calibration"`.
#'   Must be consistent with the number of reference samples: `"multi"`
#'   requires more than one reference, while `"alp"`, `"clw"`, and
#'   `"calibration"` require exactly one.
#' @param sp_order Reference-sample ordering rule for the multi-reference case.
#'   Must be either `"size"` or `"given"` when multiple reference
#'   samples are provided. Ignored in the one-reference case; a warning is
#'   issued if a value other than `"size"` (the default) is supplied.
#' @param precali Single logical value indicating whether cumulative
#'   precalibration is used in the multi-reference case. Required when
#'   multiple reference samples are provided. Ignored in the one-reference
#'   case; a warning is issued if FALSE is supplied.
#' @param sc_wname Single non-empty character string giving the intended name
#'   of the pseudo-weight column. An error is raised if the name already
#'   exists in `sc`.
#' @param verbose Single TRUE/FALSE controlling diagnostic output. Validated
#'   before any data checks are performed.
#'
#' @return Invisibly returns TRUE if all checks pass. Otherwise, the
#'   function stops with an informative error message.
#'
#' @keywords internal
#'
#' @noRd
check_ipwm_inputs_build <- function(data, p_formula, method,
                                    sp_order = NULL, precali = NULL,
                                    sc_wname = "pseudo_wts",
                                    verbose  = FALSE) {

  #--------------------------------------------------#
  # verbose
  #--------------------------------------------------#
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be a single TRUE/FALSE value.", call. = FALSE)
  }

  #--------------------------------------------------#
  # sc_wname
  #--------------------------------------------------#
  if (!is.character(sc_wname) || length(sc_wname) != 1L ||
      is.na(sc_wname) || !nzchar(sc_wname)) {
    stop("`sc_wname` must be a single non-empty character string.", call. = FALSE)
  }

  #--------------------------------------------------#
  # data: basic structure
  #--------------------------------------------------#
  if (!is.list(data) || length(data) < 2L) {
    stop("`data` must be a list like list(sc, sp1.des, sp2.des, ...).",
         call. = FALSE)
  }

  sc <- data[[1]]
  sp_des <- data[-1]
  n_ref <- length(sp_des)

  if (!is.data.frame(sc)) {
    stop("The first element of `data` must be a data.frame for `sc`.",
         call. = FALSE)
  }

  if (nrow(sc) == 0L) {
    stop("`sc` has zero rows.", call. = FALSE)
  }

  if (anyDuplicated(names(sc))) {
    stop("`sc` has duplicated column names.", call. = FALSE)
  }

  if (sc_wname %in% names(sc)) {
    stop(
      sprintf("Column '%s' already exists in `sc`. Choose a different `sc_wname`.",
              sc_wname),
      call. = FALSE
    )
  }

  ok_des <- vapply(
    sp_des,
    function(x) inherits(x, c("survey.design2", "svyrep.design")),
    logical(1)
  )

  if (!all(ok_des)) {
    bad <- which(!ok_des)
    stop(
      sprintf(
        "Elements %s of `data` are not valid survey design objects.",
        paste(bad + 1L, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  # stop when lonely psu + getOption("survey.lonely.psu") = "fail"
  for (i in seq_along(sp_des)) {
    if (inherits(sp_des[[i]], "survey.design2")) {
      check_lonely_psu(
        sp_des[[i]],
        name = paste0("sp_des[[", i, "]]")
      )
    }
  }

  # extract analysis data from survey design objects
  sp <- lapply(sp_des, extract_analysis_data)

  # check for zero-row reference samples
  for (i in seq_along(sp)) {
    if (nrow(sp[[i]]) == 0L) {
      stop(sprintf("Reference sample %d has zero rows.", i), call. = FALSE)
    }
  }

  #--------------------------------------------------#
  # method
  #--------------------------------------------------#
  valid_methods <- c("alp", "clw", "multi", "cali", "calibration")

  if (!is.null(method)) {

    if (!is.character(method) || length(method) != 1L || is.na(method)) {
      stop("`method` must be a single character string or NULL.",
           call. = FALSE)
    }

    method_lc <- tolower(trimws(method))

    if (!(method_lc %in% valid_methods)) {
      stop(
        sprintf(
          paste0(
            "Invalid method '%s'.\n",
            "For one-reference settings, valid methods are: ",
            "'alp', 'clw', 'calibration' (or 'cali').\n",
            "For multi-reference settings, the valid method is: 'multi'."
          ),
          method
        ),
        call. = FALSE
      )
    }

    #--------------------------------------------------#
    # consistency check with number of references
    #--------------------------------------------------#
    if (n_ref == 1 && method_lc == "multi") {
      stop(
        "`method = 'multi'` is only allowed when multiple reference samples are provided.",
        call. = FALSE
      )
    }

    if (n_ref > 1 && method_lc %in% c("alp", "clw", "calibration")) {
      stop(
        paste0(
          "method = '", method, "' is only valid for a single reference sample. ",
          "Use `method = 'multi'` when multiple reference samples are provided."
        ),
        call. = FALSE
      )
    }
  }

  #--------------------------------------------------#
  # one-reference case
  #--------------------------------------------------#
  if (n_ref == 1L) {

    if (!is.null(sp_order) && sp_order != "size") {
      warning("`sp_order` is ignored in the one-reference case.", call. = FALSE)
    }

    if (!is.null(precali) && !isTRUE(precali)) {
      warning("`precali` is ignored in the one-reference case.", call. = FALSE)
    }

    if (!is.null(p_formula)) {
      if (!inherits(p_formula, "formula")) {
        stop("For one-reference case, `p_formula` must be a formula or NULL.",
             call. = FALSE)
      }

      if (length(p_formula) == 3L) {
        stop(
          "`p_formula` must be a one-sided formula (e.g., ~ x + y), not two-sided.",
          call. = FALSE
        )
      }

      check_formula_vars_exist(
        fml   = p_formula,
        sc    = sc,
        sp    = sp[[1]],
        label = "`p_formula`"
      )
    }

    return(invisible(TRUE))
  }

  #--------------------------------------------------#
  # multi-reference case
  #--------------------------------------------------#
  if (n_ref >= 2L) {

    if (!is.character(sp_order) || length(sp_order) != 1L || is.na(sp_order) ||
        !(sp_order %in% c("size", "given"))) {
      stop("`sp_order` must be one of 'size' or 'given'.",
           call. = FALSE)
    }

    if (!is.logical(precali) || length(precali) != 1L || is.na(precali)) {
      stop("`precali` must be a single TRUE/FALSE value.",
           call. = FALSE)
    }

    if (!is.null(p_formula)) {
      if (!is.list(p_formula) || length(p_formula) != n_ref) {
        stop(
          sprintf(
            "For multi-reference case, `p_formula` must be a list of %d formulas.",
            n_ref
          ),
          call. = FALSE
        )
      }

      ok_formula <- vapply(
        p_formula,
        function(x) inherits(x, "formula"),
        logical(1)
      )

      if (!all(ok_formula)) {
        stop("All elements of `p_formula` must be formulas.",
             call. = FALSE)
      }

      two_sided <- vapply(p_formula, function(x) length(x) == 3L, logical(1))
      if (any(two_sided)) {
        stop(
          "All formulas in `p_formula` must be one-sided (e.g., ~ x + y), not two-sided.",
          call. = FALSE
        )
      }

      for (i in seq_len(n_ref)) {
        check_formula_vars_exist(
          fml   = p_formula[[i]],
          sc    = sc,
          sp    = sp[[i]],
          label = sprintf("`p_formula[[%d]]`", i)
        )
      }
    }

    return(invisible(TRUE))
  }
}
