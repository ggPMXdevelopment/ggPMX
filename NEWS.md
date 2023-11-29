# ggPMX 1.2.11

* Fix bug so that reading monolix works on R devel (as requested by CRAN)

* Change defaults so that scientific notation is not enabled by default
  and title is also not added by default

* Monolix 2023 can now be read by ggPMX

# ggPMX 1.2.10

* Skip draft parameter template creation test on CRAN, on some systems
  it doesn't seem to work correctly.

# ggPMX 1.2.9

* Fix custom template so it works again

* Fix `nlmixr2`controller to correctly handle blq data

# ggPMX 1.2.8

* Always ensure the `isoberv` is "accepted" for input dataset (fixes #235)

* Copy the testing information in inst to a temporary directory for utils testing

# ggPMX 1.2.7

* Added ability to generate ChartData if lixoftConnectors is present

* Added `nlmixr2` support

* Added cwres family of plots for `nlmixr`/`nlmixr2` models

* Added `npd` family of plots for `nlmixr`/`nlmixr2`

* Added a `NEWS.md` file to track changes to the package.
