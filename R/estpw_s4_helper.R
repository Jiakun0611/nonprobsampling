# -------------------------------------------------------------------
# Check categorical levels before constructing the model matrices.
# -------------------------------------------------------------------
.check_categorical_levels <- function(sc, sp_i, vars_in_formula, ref_label = "sp") {
  for (v in vars_in_formula) {
    sc_var <- sc[[v]]
    sp_var <- sp_i[[v]]
    has_factor <- is.factor(sc_var) || is.factor(sp_var)
    both_character <- is.character(sc_var) && is.character(sp_var)
    if (!has_factor && !both_character) next

    # model.matrix() converts character columns to factors.
    lev_sc <- levels(as.factor(sc_var))
    lev_sp <- levels(as.factor(sp_var))
    if (!identical(lev_sc, lev_sp)) {
      stop(sprintf(
        "%s levels of '%s' differ between sc and %s.\n  sc : %s\n  %s : %s",
        if (has_factor) "Factor" else "Character",
        v,
        ref_label,
        paste(lev_sc, collapse = ", "),
        ref_label,
        paste(lev_sp, collapse = ", ")
      ), call. = FALSE)
    }
  }
}
