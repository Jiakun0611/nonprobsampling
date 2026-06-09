# -------------------------------------------------------------------
# Helper: check factor levels consistency between sc and sp_i
# -------------------------------------------------------------------
.check_factor_levels <- function(sc, sp_i, vars_in_formula, ref_label = "sp") {
  for (v in vars_in_formula) {
    if (is.factor(sc[[v]]) || is.factor(sp_i[[v]])) {
      lev_sc <- levels(as.factor(sc[[v]]))
      lev_sp <- levels(as.factor(sp_i[[v]]))
      if (!identical(lev_sc, lev_sp)) {
        stop(sprintf(
          "Factor levels of '%s' differ between sc and %s.\n  sc : %s\n  %s : %s",
          v,
          ref_label,
          paste(lev_sc, collapse = ", "),
          ref_label,
          paste(lev_sp, collapse = ", ")
        ), call. = FALSE)
      }
    }
  }
}
