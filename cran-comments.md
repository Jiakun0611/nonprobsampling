## Submission

This is a resubmission of a new package.

In this version I have:

* Updated `NEWS.md` to summarize the changes made for this submission.
* Updated the simulation script in `inst/` to avoid changing the user's global
  settings.
* Added support for categorical outcomes in `pwmean()`, allowing prevalence
  estimation for each observed category.
* Revised documentation, examples, and terminology for consistency.

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

* This is a resubmission of a new package.

The possibly misspelled words reported in DESCRIPTION are intentional:
CLW is a method abbreviation; Valliant is an author surname; et al. is a
standard citation abbreviation; nonprobability and precalibration are technical
terms used in the survey sampling literature.

The URL https://www.gnu.org/licenses/gpl-3.0 in README.md was
flagged as a timeout error on the Win-builder server. The URL is valid
and accessible; this appears to be a transient connectivity issue on
the check server.

## Downstream dependencies

This is a new package, so there are no downstream dependencies.
