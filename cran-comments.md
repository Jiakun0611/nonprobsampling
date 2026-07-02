## Resubmission

* Removed the top-level use of `on.exit()` from `inst/reproduce_sim_paper.R`.
* Restored user options explicitly at the end of the script using `options(old_options)`, following CRAN's policy for scripts.

The package provides pseudo-weighting methods for finite population inference
from nonprobability samples using auxiliary information from one or multiple
probability reference surveys.

## Test environments

* Local: Windows 10, R 4.4.2
* win-builder: R-devel and R-release
  (devtools::check_win_devel(), devtools::check_win_release())
* R-hub: Linux, macOS arm64, and Windows with R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

On win-builder R-release, the note is from CRAN incoming feasibility and is
expected for this resubmission.

The possibly misspelled words reported in DESCRIPTION are intentional:
CLW is a method abbreviation; Valliant is an author surname; et al. is a
standard citation abbreviation; nonprobability, Nonprobability,
precalibration, and prevalences are technical or standard terms used in the
survey sampling literature.

On win-builder R-devel, the only note is:
* checking for future file timestamps ... NOTE
  unable to verify current time

This appears to be caused by the check environment being unable to verify the
current time, rather than by future-dated files in the package source.

## Downstream dependencies

This is a new package, so there are no downstream dependencies.
