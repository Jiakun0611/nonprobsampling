usethis::use_data( sp1, sp2, sp1_bootstrap, overwrite = TRUE, compress = "xz")
usethis::use_data( sc, overwrite = TRUE, compress = "xz")

# write documentation for data
usethis::use_r("sc")
usethis::use_r("sp1")
usethis::use_r("sp2")
usethis::use_r("sp1_bootstrap")
