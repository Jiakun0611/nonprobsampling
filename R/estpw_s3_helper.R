#' Resolve na.action to a standard mode string
#'
#' Converts the `na.action` argument -- which may be a function, a character
#' string, or `NULL` -- into one of four canonical mode strings: `"omit"`,
#' `"exclude"`, `"fail"`, or `"pass"`. When `NULL` is supplied the function
#' reads `getOption("na.action")`, following the same convention as `lm()`.
#'
#' Accepted function inputs: `stats::na.omit`, `stats::na.exclude`,
#' `stats::na.fail`, `stats::na.pass`.
#'
#' Accepted string inputs (case-insensitive): `"na.omit"` / `"omit"`,
#' `"na.exclude"` / `"exclude"`, `"na.fail"` / `"fail"`,
#' `"na.pass"` / `"pass"`.
#'
#' @param na.action A function, a single character string, or `NULL`.
#'
#' @return A single character string: one of `"omit"`, `"exclude"`,
#'   `"fail"`, or `"pass"`.
#'
#' @keywords internal
#' @noRd
resolve_na_action <- function(na.action) {

  if (is.null(na.action)) {
    na.action <- getOption("na.action")
  }

  # 1) function input
  if (is.function(na.action)) {
    if (identical(na.action, stats::na.omit))    return("omit")
    if (identical(na.action, stats::na.exclude)) return("exclude")
    if (identical(na.action, stats::na.fail))    return("fail")
    if (identical(na.action, stats::na.pass))    return("pass")
    stop("Unsupported na.action function. Use na.omit/na.exclude/na.fail/na.pass.", call. = FALSE)
  }

  # 2) character input
  if (is.character(na.action) && length(na.action) == 1L) {
    x <- tolower(na.action)

    if (x %in% c("na.omit", "omit"))       return("omit")
    if (x %in% c("na.exclude", "exclude")) return("exclude")
    if (x %in% c("na.fail", "fail"))       return("fail")
    if (x %in% c("na.pass", "pass"))       return("pass")

    stop("Invalid na.action string. Use 'na.omit','na.exclude','na.fail','na.pass' (or omit/exclude/fail/pass).",
         call. = FALSE)
  }

  stop("Invalid na.action type. Use a function (na.omit/na.exclude/na.fail/na.pass), a string, or NULL.",
       call. = FALSE)
}


#' Core NA filtering for the IPWM build stage
#'
#' Identifies rows with missing values in the participation model variables,
#' applies the chosen NA handling mode to `sc` and `sp`, and constructs an
#' lm-style `na.action` attribute for `sc`. Called by `process_na_build()`.
#'
#' For the one-reference case `sp` is a plain data frame and `p_formula` is
#' a single formula; the same set of variables is used to check NAs in both
#' `sc` and `sp`. For the multi-reference case `sp` is a list of data frames
#' and `p_formula` is a list of formulas; `sc` is checked against the union
#' of all formula variables, while each `sp[[j]]` is checked only against the
#' variables in `p_formula[[j]]`.
#'
#' @param sc A data frame. The nonprobability sample.
#' @param sp A data frame (one-reference) or a named list of data frames
#'   (multi-reference). Reference survey analysis data.
#' @param p_formula A one-sided formula (one-reference) or a list of
#'   one-sided formulas (multi-reference).
#' @param na_mode Character string. One of `"omit"`, `"exclude"`,
#'   `"fail"`, or `"pass"`, as returned by `resolve_na_action()`.
#'
#' @return A list with five elements:
#' \describe{
#'   \item{`sc`}{Cleaned nonprobability sample (rows with NAs removed
#'     when `na_mode` is `"omit"`, `"exclude"`, or `"fail"`).}
#'   \item{`sp`}{Cleaned reference survey data frame(s).}
#'   \item{`keep_sc`}{Logical vector of length `nrow(sc)` indicating
#'     retained rows.}
#'   \item{`keep_sp`}{Logical vector (one-reference) or list of logical
#'     vectors (multi-reference) indicating retained rows in each `sp`.}
#'   \item{`na_action`}{An lm-style `na.action` attribute (class `"omit"`
#'     or `"exclude"`) recording the dropped row indices of `sc`, or
#'     `NULL` if no rows were dropped.}
#'   \item{`log`}{Character vector of per-variable NA detail messages,
#'     propagated to `log_messages` in `est_pw()`.}
#' }
#'
#' @keywords internal
#' @noRd
handle_na_for_ipwm <- function(sc, sp, p_formula, na_mode = c("omit", "exclude", "fail", "pass")) {

  na_mode <- match.arg(na_mode)
  log <- character(0)

  #----------------------------------------------------------
  # helper: NA summary logging -- returns messages, no <<-
  #----------------------------------------------------------
  .na_summary <- function(df, vars, label) {
    if (length(vars) == 0L) return(character(0))
    na_ct <- colSums(is.na(df[, vars, drop = FALSE]))
    na_ct <- na_ct[na_ct > 0]
    if (length(na_ct) == 0L) return(character(0))
    msgs <- c(
      "\nMissing value summary:\n",
      sprintf("%d NA detected in p_formula variables in %s:", sum(na_ct), label)
    )
    for (v in names(na_ct)) msgs <- c(msgs, sprintf("  - %s", v))
    msgs
  }

  #----------------------------------------------------------
  # 1. sc
  #----------------------------------------------------------
  if (is.list(p_formula)) {
    vars_sc <- unique(unlist(lapply(p_formula, all.vars)))
  } else {
    vars_sc <- all.vars(p_formula)
  }

  if (length(vars_sc) > 0) {
    log <- c(log, .na_summary(sc, vars_sc, "sc"))
    keep_sc <- stats::complete.cases(sc[, vars_sc, drop = FALSE])
  } else {
    keep_sc <- rep(TRUE, nrow(sc))
  }
  removed_sc <- sum(!keep_sc)

  if (na_mode == "fail" && removed_sc > 0) {
    stop("na.fail: NA found in p_formula variables in sc.", call. = FALSE)
  }

  if (na_mode == "pass") {
    keep_sc[] <- TRUE
  } else if (na_mode %in% c("omit", "exclude") && removed_sc > 0) {
    log <- c(log, sprintf("\nRemoved %d rows from sc due to NA in p_formula variables.\n", removed_sc))
  }

  sc_clean <- if (na_mode %in% c("omit", "exclude", "fail")) {
    sc[keep_sc, , drop = FALSE]
  } else {
    sc
  }

  #----------------------------------------------------------
  # 2. sp: one-reference
  #----------------------------------------------------------
  if (is.data.frame(sp)) {

    if (length(vars_sc) > 0) {
      log <- c(log, .na_summary(sp, vars_sc, "sp"))
      keep_sp <- stats::complete.cases(sp[, vars_sc, drop = FALSE])
    } else {
      keep_sp <- rep(TRUE, nrow(sp))
    }
    removed_sp <- sum(!keep_sp)

    if (na_mode == "fail" && removed_sp > 0) {
      stop("na.fail: NA found in p_formula variables in sp.", call. = FALSE)
    }

    if (na_mode == "pass") {
      keep_sp[] <- TRUE
    } else if (na_mode %in% c("omit", "exclude") && removed_sp > 0) {
      log <- c(log, sprintf("\nRemoved %d rows from sp due to NA in p_formula variables.\n", removed_sp))
    }

    sp_clean <- if (na_mode %in% c("omit", "exclude", "fail")) {
      sp[keep_sp, , drop = FALSE]
    } else {
      sp
    }

    keep_sp_out <- keep_sp

    #----------------------------------------------------------
    # 2. sp: multi-reference
    #----------------------------------------------------------
  } else if (is.list(sp) && all(vapply(sp, is.data.frame, logical(1)))) {

    sp_clean     <- vector("list", length(sp))
    keep_sp_list <- vector("list", length(sp))

    sp_names  <- names(sp)
    use_names <- if (!is.null(sp_names) && all(nzchar(sp_names))) {
      sp_names
    } else {
      paste0("sp[[", seq_along(sp), "]]")
    }

    for (j in seq_along(sp)) {

      if (!is.list(p_formula) || length(p_formula) < j) {
        stop("For multi-reference sp, p_formula must be a list with same length as sp.", call. = FALSE)
      }

      vars_j <- all.vars(p_formula[[j]])

      if (length(vars_j) > 0) {
        log <- c(log, .na_summary(sp[[j]], vars_j, use_names[j]))
        keep_spj <- stats::complete.cases(sp[[j]][, vars_j, drop = FALSE])
      } else {
        keep_spj <- rep(TRUE, nrow(sp[[j]]))
      }
      removed_spj <- sum(!keep_spj)

      if (na_mode == "fail" && removed_spj > 0) {
        stop(sprintf("na.fail: NA found in p_formula variables in %s.", use_names[j]), call. = FALSE)
      }

      if (na_mode == "pass") {
        keep_spj[] <- TRUE
      } else if (na_mode %in% c("omit", "exclude") && removed_spj > 0) {
        log <- c(log, sprintf("\nRemoved %d rows from %s due to NA in p_formula variables.\n",
                              removed_spj, use_names[j]))
      }

      sp_clean[[j]] <- if (na_mode %in% c("omit", "exclude", "fail")) {
        sp[[j]][keep_spj, , drop = FALSE]
      } else {
        sp[[j]]
      }
      keep_sp_list[[j]] <- keep_spj
    }

    names(sp_clean)  <- use_names
    keep_sp_out      <- keep_sp_list

  } else {
    stop("'sp' must be a data.frame (one reference) or a list of data.frames (multi reference).",
         call. = FALSE)
  }

  #----------------------------------------------------------
  # 3. lm-style na.action object for sc mapping
  #----------------------------------------------------------
  na_action_obj <- NULL
  if (na_mode %in% c("omit", "exclude")) {
    idx_drop <- which(!keep_sc)
    if (length(idx_drop) > 0) {
      cls           <- if (na_mode == "exclude") "exclude" else "omit"
      na_action_obj <- structure(idx_drop, class = cls)
    }
  }

  return(list(
    sc        = sc_clean,
    sp        = sp_clean,
    keep_sc   = keep_sc,
    keep_sp   = keep_sp_out,
    na_action = na_action_obj,
    log       = log
  ))
}



#' Summarise NA exclusions into a pw_na_summary object
#'
#' Computes per-dataset row counts (original, used, excluded) from the
#' `keep_sc` and `keep_sp` logical vectors produced by
#' `handle_na_for_ipwm()`. If `verbose = TRUE`, prints a one-line
#' message per dataset via `message()`. Returns `NULL` when no rows
#' were excluded in any dataset, or a `pw_na_summary` object otherwise.
#'
#' @param res The intermediate result list from `process_na_build()`,
#'   which must contain `keep_sc`, `keep_sp`, and `n_sp_orig`.
#' @param n_sc_orig Integer. Original row count of `sc` before NA removal.
#' @param n_ref Integer. Number of reference surveys.
#' @param verbose Logical. If `TRUE`, prints exclusion counts via
#'   `message()`.
#'
#' @return A `pw_na_summary` object (a list with elements `sc`, `sp`,
#'   and `n_ref`, classed `"pw_na_summary"`), or `NULL` if no rows were
#'   excluded.
#'
#' @keywords internal
#' @noRd
.report_na_exclusions <- function(res, n_sc_orig, n_ref, verbose = FALSE) {
  keep_sc   <- res$keep_sc
  keep_sp   <- res$keep_sp
  n_sp_orig <- res$n_sp_orig

  n_sc_used <- if (is.logical(keep_sc)) sum(keep_sc) else length(keep_sc)
  n_sc_excl <- n_sc_orig - n_sc_used
  sc_info   <- list(n_orig = n_sc_orig, n_used = n_sc_used, n_excluded = n_sc_excl)

  if (verbose) {
    if (n_sc_excl > 0) {
      message(sprintf(
        "sc: %d rows used, %d excluded due to missing participation model variables.",
        n_sc_used, n_sc_excl
      ))
    } else {
      message(sprintf("sc: %d rows, no exclusions.", n_sc_used))
    }
  }

  if (n_ref == 1) {
    n_sp_used <- if (is.logical(keep_sp)) sum(keep_sp) else n_sp_orig
    n_sp_excl <- n_sp_orig - n_sp_used
    sp_info   <- list(n_orig = n_sp_orig, n_used = n_sp_used, n_excluded = n_sp_excl)

    if (verbose) {
      if (n_sp_excl > 0) {
        message(sprintf(
          "sp: %d rows used, %d excluded due to missing participation model variables.",
          n_sp_used, n_sp_excl
        ))
      } else {
        message(sprintf("sp: %d rows, no exclusions.", n_sp_orig))
      }
    }
  } else {
    sp_info <- vector("list", length(keep_sp))
    for (.i in seq_along(keep_sp)) {
      n_sp_used_i <- if (is.logical(keep_sp[[.i]])) sum(keep_sp[[.i]]) else n_sp_orig[.i]
      n_sp_excl_i <- n_sp_orig[.i] - n_sp_used_i
      sp_info[[.i]] <- list(n_orig = n_sp_orig[.i], n_used = n_sp_used_i, n_excluded = n_sp_excl_i)

      if (verbose) {
        if (n_sp_excl_i > 0) {
          message(sprintf(
            "sp[[%d]]: %d rows used, %d excluded due to missing participation model variables.",
            .i, n_sp_used_i, n_sp_excl_i
          ))
        } else {
          message(sprintf("sp[[%d]]: %d rows, no exclusions.", .i, n_sp_orig[.i]))
        }
      }
    }
  }

  any_excl <- n_sc_excl > 0 || {
    if (n_ref == 1) {
      sp_info$n_excluded > 0
    } else {
      any(vapply(sp_info, function(x) x$n_excluded > 0, logical(1)))
    }
  }

  if (!any_excl) return(NULL)

  out <- list(sc = sc_info, sp = sp_info, n_ref = n_ref)
  class(out) <- "pw_na_summary"
  out
}



#' Print method for pw_na_summary
#'
#' Prints a formatted table showing the original row count, rows used,
#' and rows excluded due to missing participation model variables, for
#' each dataset (`sc` and each reference survey).
#'
#' @param x A `pw_na_summary` object returned by `.report_na_exclusions()`.
#' @param ... Further arguments passed to or from other methods (unused).
#'
#' @return Invisibly returns `x`.
#'
#' @method print pw_na_summary
#' @export
print.pw_na_summary <- function(x, ...) {
  cat("NA processing summary:\n")

  if (x$n_ref == 1) {
    rows <- list(sc = x$sc, sp = x$sp)
  } else {
    rows <- c(list(sc = x$sc), x$sp)
    sp_labels <- if (!is.null(names(x$sp)) && all(nzchar(names(x$sp)))) {
      names(x$sp)
    } else {
      paste0("sp[[", seq_along(x$sp), "]]")
    }
    names(rows) <- c("sc", sp_labels)
  }

  df <- do.call(rbind, lapply(rows, function(r) {
    data.frame(n_orig = r$n_orig, n_used = r$n_used, n_excluded = r$n_excluded)
  }))

  print(df)
  invisible(x)
}

