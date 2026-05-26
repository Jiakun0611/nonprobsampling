#' Auto-build a default participation model formula
#'
#' Constructs a one-sided participation model formula from the variables
#' shared between the nonprobability sample (`sc`) and each reference
#' survey, excluding the sampling weight column. Called by `est_pw()` when
#' `p_formula = NULL`.
#'
#' For each reference survey, the candidate covariates are the column names
#' present in both `sc` and the reference survey data frame after dropping
#' the sampling weight column named by `weight`. A one-sided formula of the
#' form `~ var1 + var2 + ...` is built from the remaining shared variables.
#' An error is raised if no shared covariates remain after excluding the
#' weight column.
#'
#' The function distinguishes the one-reference case from the multi-reference
#' case by the type of `sp`: a plain data frame triggers the one-reference
#' path; a list of data frames triggers the multi-reference path. Note that
#' in R a `data.frame` is also a `list`, so the `is.data.frame()` check is
#' performed first.
#'
#' @param sc A data frame. The nonprobability sample.
#' @param sp A data frame (one-reference case) or a named list of data frames
#'   (multi-reference case). Each data frame contains the analysis variables
#'   of one reference survey, with design variables removed and a sampling
#'   weight column appended (as produced by `extract_analysis_data()`).
#' @param weight A single character string (one-reference case) or a
#'   character vector of the same length as `sp` (multi-reference case)
#'   giving the name of the sampling weight column in each reference survey
#'   data frame. This column is excluded from the candidate covariate set.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`p_formula`}{A one-sided formula (one-reference case) or a named
#'     list of one-sided formulas (multi-reference case).}
#'   \item{`log_messages`}{A character vector of messages describing the
#'     auto-generated formula(s), used for downstream printing when
#'     `verbose = TRUE`.}
#' }
#'
#' @keywords internal
p_formula_construction <- function(sc, sp, weight) {

  log_messages <- character(0)

  build_one_formula <- function(sc, sp_i, weight_i, sp_name) {
    log_messages <- character(0)

    shared     <- intersect(colnames(sc), colnames(sp_i))
    drop_these <- weight_i
    vars       <- setdiff(shared, drop_these)

    if (length(vars) == 0L) {
      stop(
        sprintf("No shared covariates found to build default p_formula for %s.", sp_name),
        call. = FALSE
      )
    }

    fml <- as.formula(paste("~", paste(vars, collapse = " + ")))

    msg <- paste0(
      "Generated default p_formula for ", sp_name, ": ",
      paste(deparse(fml), collapse = ""),
      "\n"
    )

    log_messages <- c(log_messages, msg)

    return(list(
      p_formula    = fml,
      log_messages = log_messages
    ))
  }

  #-------------------------------#
  # one-reference case
  #-------------------------------#
  if (is.data.frame(sp)) {
    res <- build_one_formula(
      sc       = sc,
      sp_i     = sp,
      weight_i = weight,
      sp_name  = "reference survey"
    )

    if (!is.null(res$log_messages) && length(res$log_messages) > 0) {
      log_messages <- c(log_messages, res$log_messages)
    }

    return(list(
      p_formula    = res$p_formula,
      log_messages = log_messages
    ))
  }

  #-------------------------------#
  # multi-reference case
  #-------------------------------#
  if (is.list(sp)) {
    if (!is.character(weight) || length(weight) != length(sp)) {
      stop(
        "For multi-reference case, `weight` must be a character vector with same length as `sp`.",
        call. = FALSE
      )
    }

    p_formula_list <- vector("list", length(sp))

    for (i in seq_along(sp)) {
      sp_name <- if (!is.null(names(sp)) && nzchar(names(sp)[i])) {
        names(sp)[i]
      } else {
        paste0("sp", i)
      }

      res_i <- build_one_formula(
        sc       = sc,
        sp_i     = sp[[i]],
        weight_i = weight[i],
        sp_name  = sp_name
      )

      p_formula_list[[i]] <- res_i$p_formula

      if (!is.null(res_i$log_messages) && length(res_i$log_messages) > 0) {
        log_messages <- c(log_messages, res_i$log_messages)
      }
    }

    names(p_formula_list) <- if (!is.null(names(sp))) {
      names(sp)
    } else {
      paste0("sp", seq_along(sp))
    }

    return(list(
      p_formula    = p_formula_list,
      log_messages = log_messages
    ))
  }

  stop("`sp` must be either a data.frame or a list of data.frames.", call. = FALSE)
}
