## Submission

ggPMX 1.3.3 addresses the MKL-related smoothing test failure reported by CRAN
by using a tolerance-aware comparison of numerical smoothing results. It also
includes fixes for Monolix time matching, simulated BLOQ response handling, and
the per-call `is.draft` argument in individual plots (#416), together with
documentation updates.

This resubmission also reduces test time on winbuilder.

## Test environment

Final source tarball checked locally on 2026-11-19: macOS 27.2.1,
aarch64-apple-darwin23, R 4.6.2.

The source package was built with `R CMD build --compact-vignettes=gs` and
checked with `R CMD check --as-cran`. The PDF user guide was regenerated and
successfully rebuilt during checking. Both PDF and HTML manual checks passed;
HTML validation used HTML Tidy 5.8.0. The documentation toolchain included
Pandoc 3.10, TeX Live 2026, and Ghostscript 10.08.0. Examples, including
`donttest` examples, also passed.

## R CMD check results

0 errors | 0 warnings | 1 note

The remaining incoming-feasibility note concerns `lixoftConnectors`, an
optional suggested package not available from mainstream repositories. It is
distributed with Monolix; installation instructions are linked in DESCRIPTION.
Checking used `_R_CHECK_FORCE_SUGGESTS_=false` because this dependency was
unavailable, so functionality requiring it was not validated locally.
