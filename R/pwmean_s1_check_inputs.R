#' Validate inputs at the estimation stage
#'
#' Checks that `build` is a valid `pw_fit` object with all required internal
#' fields, that `y` names a numeric column in the raw convenience sample, and
#' that `zcol` (if supplied) is a supported domain variable with at least two
#' non-missing levels in both the full sample and the build-stage complete cases.
#'
#' @param build A `pw_fit` object returned by the build step.
#' @param y Single character string naming the numeric outcome variable in
#'   `build$internal$raw_sc`.
#' @param zcol Single character string naming the domain variable in
#'   `build$internal$raw_sc`, or NULL for the overall mean.
#'
#' @return `invisible(TRUE)` on success; stops with an informative message
#'   on failure.
#'
#' @keywords internal
check_ipwm_inputs_estimate <- function(build, y, zcol = NULL) {

  #----------------------------------#
  # Step 1: required fields in build
  #----------------------------------#
  if (!inherits(build, "pw_fit")) {
    stop("'object' must be of class \"pw_fit\".", call. = FALSE)
  }

  if (is.null(build$internal$raw_sc)) {
    stop("build$internal$raw_sc is missing.", call. = FALSE)
  }

  if (is.null(build$internal$na$keep_sc)) {
    stop("build$internal$na$keep_sc is missing.", call. = FALSE)
  }

  if (is.null(build$pseudo_weights)) {
    stop("build$pseudo_weights is missing.", call. = FALSE)
  }

  if (is.null(build$internal$Xc)) {
    stop("build$internal$Xc is missing.", call. = FALSE)
  }

  if (is.null(build$method) || !is.character(build$method) || length(build$method) != 1L) {
    stop("build$method must be a single character string.", call. = FALSE)
  }

  if (is.null(build$internal$D)) {
    stop("build$internal$D is missing.", call. = FALSE)
  }

  if (is.null(build$internal$S_beta)) {
    stop("build$internal$S_beta is missing.", call. = FALSE)
  }

  sc <- build$internal$raw_sc

  #----------------------------------#
  # Step 2: check y
  #----------------------------------#
  if (!is.character(y) || length(y) != 1L || is.na(y) || !nzchar(y)) {
    stop("'y' must be a single non-empty character string.", call. = FALSE)
  }

  if (!(y %in% names(sc))) {
    stop(sprintf("Outcome variable '%s' not found in sc.", y), call. = FALSE)
  }

  if (!is.numeric(sc[[y]])) {
    stop(sprintf("Outcome variable '%s' must be numeric.", y), call. = FALSE)
  }

  #----------------------------------#
  # Step 3: check zcol
  #----------------------------------#
  if (!is.null(zcol)) {

    if (!is.character(zcol) || length(zcol) != 1L || is.na(zcol) || !nzchar(zcol)) {
      stop("'zcol' must be a single non-empty character string or NULL.", call. = FALSE)
    }

    if (!(zcol %in% names(sc))) {
      stop(sprintf("Domain variable '%s' not found in sc.", zcol), call. = FALSE)
    }

    z <- sc[[zcol]]

    # validate type and level count in raw_sc; use if/else if so execution
    # falls through to Step 4 (sc_keep level check) after a passing branch
    if (is.logical(z)) {
      if (length(unique(stats::na.omit(z))) < 2L) {
        stop(
          sprintf("Domain variable '%s' must contain both TRUE and FALSE.", zcol),
          call. = FALSE
        )
      }

    } else if (is.factor(z)) {
      nonmiss_levels <- levels(droplevels(z))
      if (length(nonmiss_levels) < 2L) {
        stop(
          sprintf(
            "Domain variable '%s' must contain at least two non-missing levels.",
            zcol
          ),
          call. = FALSE
        )
      }

    } else if (is.character(z)) {
      # trimws() and blank-to-NA follow what standardize_zcol() does before
      # building the factor, so the level count here matches what estimation sees
      z_trim <- trimws(z)
      z_trim[z_trim == ""] <- NA_character_
      nonmiss_levels <- unique(stats::na.omit(z_trim))

      if (length(nonmiss_levels) < 2L) {
        stop(
          sprintf(
            "Domain variable '%s' must contain at least two non-missing levels.",
            zcol
          ),
          call. = FALSE
        )
      }

    } else if (is.numeric(z)) {
      # allow numeric/integer only if binary (ignoring NA)
      uniq_vals <- sort(unique(stats::na.omit(z)))

      if (!all(uniq_vals %in% c(0, 1))) {
        stop(
          sprintf(
            paste0(
              "Domain variable '%s' is numeric but not binary. ",
              "Allowed numeric domain variables must contain only {0, 1} ",
              "after removing missing values. Found values: %s"
            ),
            zcol,
            paste(uniq_vals, collapse = ", ")
          ),
          call. = FALSE
        )
      }

      if (length(uniq_vals) < 2L) {
        stop(
          sprintf(
            "Domain variable '%s' must contain both 0 and 1.",
            zcol
          ),
          call. = FALSE
        )
      }

    } else {
      # unsupported type
      stop(
        sprintf(
          paste0(
            "Domain variable '%s' must be one of the following: ",
            "logical, binary numeric/integer (0/1), character, factor, or NULL."
          ),
          zcol
        ),
        call. = FALSE
      )
    }
  }

  #----------------------------------#
  # Step 4: check zcol levels in sc_keep
  #
  # Notice: Step 3 confirms zcol has >= 2 non-missing levels in raw_sc.
  # but dispatch_estimator in step 4 operates on sc_keep. If keep_sc
  # removes all rows belonging to a level, est$labels will differ from
  # naive$labels (built from raw_sc), causing assemble_output to error
  # with label mismatch message. We catch this here so the user gets
  # an  error at input-check step instead.
  #----------------------------------#
  if (!is.null(zcol)) {

    sc_keep <- sc[which(build$internal$na$keep_sc), , drop = FALSE]
    z_keep  <- sc_keep[[zcol]]

    # follow the same level-counting logic as Step 3 for each type
    n_levels <- if (is.factor(z_keep)) {
      length(levels(droplevels(z_keep)))
    } else if (is.character(z_keep)) {
      z_tr <- trimws(z_keep)
      z_tr[z_tr == ""] <- NA_character_
      length(unique(stats::na.omit(z_tr)))
    } else {
      # logical and numeric: count distinct non-missing values
      length(unique(stats::na.omit(z_keep)))
    }

    if (n_levels < 2L) {
      stop(
        sprintf(
          paste0(
            "Domain variable '%s' has fewer than two non-missing levels in ",
            "the build-stage complete cases. Build-stage NA removal may have ",
            "eliminated an entire level."
          ),
          zcol
        ),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}
