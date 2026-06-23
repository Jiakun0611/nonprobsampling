#' Standardize a domain variable into a common internal format
#'
#' Converts `zcol` in `data` to a standard form used throughout the
#' estimation pipeline. Logical and binary numeric (0/1) variables become a
#' single integer indicator column; character variables are trimmed and
#' coerced to a factor; factor variables are dropped of unused levels and
#' expanded to one integer indicator column per level. The returned list
#' always has the same structure so downstream code can branch on `mode`
#' alone.
#'
#' @param data A data frame containing the column named by `zcol`.
#' @param zcol Single character string naming the domain variable in `data`,
#'   or NULL for the overall (no-domain) case.
#'
#' @return A list with components:
#'
#' - `mode`: one of `"overall"`, `"binary"`, or `"factor"`.
#' - `z_name`: the value of `zcol`, or NULL when `zcol` is NULL.
#' - `labels`: character vector of domain labels shown to the user.
#' - `indicators`: data frame of integer 0/1 indicator columns (one column
#'   per domain level), or NULL in the overall case.
#'
#' @keywords internal
standardize_zcol <- function(data, zcol = NULL) {
  #----------------------------------#
  # Step 1: overall case
  #----------------------------------#
  if (is.null(zcol)) {
    return(list(
      mode = "overall",      # one of: overall / binary / factor
      z_name = NULL,         # original variable name
      labels = "Overall",    # domain labels shown to user
      indicators = NULL      # data.frame of internal 0/1 indicators
    ))
  }

  #----------------------------------#
  # Step 2: basic input checks
  #----------------------------------#
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  if (!is.character(zcol) || length(zcol) != 1L) {
    stop("`zcol` must be NULL or a single character string.", call. = FALSE)
  }

  if (!zcol %in% names(data)) {
    stop("`zcol` was not found in `data`.", call. = FALSE)
  }

  z <- data[[zcol]]

  #----------------------------------#
  # Step 3: logical -> binary
  #----------------------------------#
  if (is.logical(z)) {
    ind_df <- data.frame(.z_domain = as.integer(z))

    return(list(
      mode = "binary",
      z_name = zcol,
      labels = paste0(zcol, " = 1"),
      indicators = ind_df
    ))
  }

  #----------------------------------#
  # Step 4: numeric/integer binary
  #----------------------------------#
  if (is.numeric(z) || is.integer(z)) {
    uz <- sort(unique(stats::na.omit(z)))

    if (length(uz) == 2L && all(uz %in% c(0, 1))) {
      ind_df <- data.frame(.z_domain = as.integer(z))

      return(list(
        mode = "binary",
        z_name = zcol,
        labels = paste0(zcol, " = 1"),
        indicators = ind_df
      ))
    }

    stop(
      paste0(
        "`zcol` is numeric/integer but not binary. ",
        "Allowed numeric domain variables must contain only 0/1 (ignoring NA)."
      ),
      call. = FALSE
    )
  }

  #----------------------------------#
  # Step 5: character -> factor
  #----------------------------------#
  if (is.character(z)) {
    z <- trimws(z)

    # treat empty string as missing
    z[z == ""] <- NA_character_

    z <- factor(z)
  }

  #----------------------------------#
  # Step 6: factor -> one indicator per level
  #----------------------------------#
  if (is.factor(z)) {
    z <- droplevels(z)
    levs <- levels(z)

    if (length(levs) < 2L) {
      stop(
        "`zcol` factor must contain at least two non-missing levels.",
        call. = FALSE
      )
    }

    ind_list <- lapply(levs, function(lv) as.integer(z == lv))
    ind_df <- as.data.frame(ind_list, stringsAsFactors = FALSE)

    # internal safe names only for internal use
    safe_names <- paste0(".z_", make.names(levs, unique = TRUE))
    names(ind_df) <- safe_names

    return(list(
      mode = "factor",
      z_name = zcol,
      labels = paste0(zcol, " = ", levs),
      indicators = ind_df
    ))
  }

  #----------------------------------#
  # Step 7: unsupported type
  #----------------------------------#
  stop(
    paste0(
      "`zcol` must be one of the following: ",
      "NULL, logical, binary numeric/integer (0/1), character, or factor."
    ),
    call. = FALSE
  )
}
