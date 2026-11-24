# Monolix demo smoke tests

This is a **manual, local-only integration suite**, separate from package tests.
It is not a GitHub Actions workflow and is not run by `testthat::test_local()`
or `R CMD check`. `.Rbuildignore` excludes the entire `tools` directory from
source packages, so CRAN does not receive this suite. Both entry points reject
CI/GitHub Actions and package-check environments as an additional safeguard.
The fast parser regression tests in `tests/testthat` remain ordinary package
tests: they do not invoke SSH or Monolix and should continue to run everywhere.

Run from the repository root on a trusted local machine with SSH access to
your licensed host. Configure the host and module names locally; the launcher
has no site-specific defaults:

```bash
export GGPMX_SSH_HOST=your-ssh-host
export GGPMX_REMOTE_MODULES="your-R-module your-Monolix-module"
export GGPMX_SCRATCH_DIR=/absolute/path/to/existing/scratch
bash tools/monolix/run-ssh.sh /absolute/path/to/new-results
```

`GGPMX_SCRATCH_DIR` (or Posit Assistant's `PA_SCRATCH_DIR`) must name an existing
scratch directory. The launcher uploads
**committed HEAD plus tracked changes under R/ and NEWS.md,
and the current harness** (not other working-tree changes),
installs ggPMX into an isolated remote library, and submits a serial LSF job
(one core, 4 GB, 20-minute limit). It downloads logs and results even after test
failure. Remote scratch files are retained for diagnosis. No vendor demos or
results are committed to this repository. Ordinary package tests and GitHub
Actions do not run this harness.

## Execution and results

The local launcher snapshots the source, transfers it over SSH, submits one LSF
job on the configured host, and downloads its results. The remote job installs that snapshot
into a private library, initializes Monolix, and processes each manifest case
serially. Every case gets its own copy of the demo; installed/vendor projects
are never edited. There is no dependency installation into the shared R library.

A run succeeds only when all checks pass. Inspect `job.log` and
`results/junit.xml` first, then each case's `warnings.txt`, `parameters.csv`,
`predictions.csv`, `controller.rds`, `dv-pred.png`, and `iwres-time.png`.
An exit status other than zero indicates a failed test or infrastructure issue.
Use a new local output directory for each run. Source commit and tracked package
patch are saved alongside the reports for traceability.

Remote runs use `~/.cache/ggPMX-monolix-tests` because login-node `/tmp` is not
shared with compute nodes. The SSH command must remain connected while the LSF
job runs. If interrupted, the job may continue: inspect it with `bjobs`/`bpeek`
and recover its `job.log` and `results` directory with `scp` rather than launching
a duplicate job.

The manifest selects theophylline, warfarin PK, and both endpoints of the
warfarin PK/PD demo. It can be extended with additional continuous-outcome
projects that have fitted results. This initial workflow does **not** refit
models or validate statistical model adequacy.

## Adding tests

Add rows to `demos.csv` to exercise more projects using the existing checks.
`name` must be a unique, filesystem-safe case name; `project` is relative to the
remote demo root; `endpoint` is optional (blank uses ggPMX's default selection).
Use separate rows to test different endpoints. A project currently needs a
same-named fitted-results folder and a sibling `data` directory.

Add shared controller/data checks in `helpers.R` and additional plots or
case-specific expectations in `test-demos.R`. Tests must stay under
`tools/monolix`, not `tests/testthat`, whenever they require Monolix or SSH.
Keep fast, engine-independent regression tests under `tests/testthat`.
New outcome types (for example censoring or categorical responses) need tailored
checks rather than weakening the finite continuous-response assertions for all
cases. Refit/scenario tests should be a separate explicit mode if added later.

The default `regenerate` mode copies each demo, preserves its original
`ChartsData`, initializes the explicitly selected Monolix installation, and
regenerates chart exports from existing fits. `exported` mode only reads existing
text exports and requires no running Monolix engine. Vendor demos that contain
only binary chart caches need `regenerate` mode.

Each case constructs a controller with `pmx_mlxtran()`, checks nonempty predictions,
subject IDs, finite observation/prediction/residual values, and parameter equality
against Monolix's `populationParameters.txt`. DV–PRED and IWRES–time plots must build
and contain the expected observation count and coordinates. PNGs, controller RDS,
CSV tables, warnings, JUnit XML, and session details support manual inspection.
These are data-integrity checks, not a claim that the model fits well.

Warnings are recorded rather than discarded or treated as automatic failures;
inspect `warnings.txt`. Errors or failed expectations make the job fail, while
remaining demos are still attempted. An initialization or dependency error aborts
before the demo tests.

Set `GGPMX_SSH_HOST` and `GGPMX_REMOTE_MODULES` in your local shell or an
untracked configuration outside the repository. Site R startup configuration is
needed: do not replace the R invocation with `--vanilla`. Set
`GGPMX_MONOLIX_MODE` locally to override the default `regenerate` mode.
For other demo or installation paths, run `run.R` directly on the remote host with
`GGPMX_MONOLIX_DEMOS`, `GGPMX_MONOLIX_HOME`, `GGPMX_HARNESS`, and a fresh
`GGPMX_MONOLIX_OUTPUT` set. `GGPMX_LIBRARY` optionally selects an isolated ggPMX
installation.

## Verified live run (Monolix 2024R1)

After fixing modern data-path and unnamed endpoint parsing, all four cases
passed: theophylline, warfarin PK, and both endpoints of `warfarinPKPD_project`.
The run produced 128 passing expectations with no errors, failures, or recorded
warnings. Vendor projects were not rewritten. Separate local regressions cover
legacy paths, modern paths, quoting, relative/absolute paths, and the bundled
theophylline controller.

To check the harness itself without Monolix:

```r
Sys.setenv(GGPMX_HARNESS_TEST_OUTPUT = "/path/to/existing/scratch")
testthat::test_file("tools/monolix/test-harness.R")
```

## Full diagnostic audit

`audit-controller.R` provides `audit_monolix_controller(project, output,
simulations = NULL)` for manual local review of an exported Monolix project.
It renders every registered diagnostic (including ggmatrix output and all
individual-fit pages), compares observations, predictions, and residuals with
Monolix exports, checks scatter coordinates, and exercises covariate
stratification, filtering, parameter formatting, and shrinkage. Failures are
recorded independently in `audit.csv`; they do not prevent other checks.
`index.html` links the resulting PNG gallery. A render PASS alone does not mean
that the corresponding data-integrity checks passed.

SAEM is exercised with an explicit modern-project configuration, because the
default modern template does not register its existing export. To audit VPC too,
export `computeChartsData(plot = "vpc", exportVPCSimulations = TRUE)` on a remote
copy, download `VisualPredictiveCheck/CONC_simulations.txt`, and supply it as
`simulations`. VPC uses explicit bins; its values need not equal Monolix's VPC
unless binning and percentile settings are also matched. This does not refit.

The initial theophylline audit exposed repeated/mixed random effects in eta QQ,
clipped valid subjects in lower eta-matrix panels, and a formatted parameter-table
error when the project has no SE/RSE estimates. After fixing these, all 35 audit
checks passed. Native GOF comparison additionally exposed transposed lower-matrix
scatter axes relative to their labels; those coordinates are now corrected too.
Dosing rows with missing concentrations still cause expected
point-removal warnings in individual-fit/VPC plots.

## Native Monolix GOF comparison

`export-native-gof.R` runs on your licensed host with `GGPMX_NATIVE_PROJECT` pointing to an
isolated fitted project and `GGPMX_NATIVE_OUTPUT` to a new directory. It uses
Monolix's plotting API (not a reimplementation), fixes individual estimates to
conditional modes, and saves PNGs plus actual plot-layer coordinates. It does not
refit the model. Download those outputs, then call
`compare_native_gof(ctr, native, output)` from `compare-native-gof.R` locally.

Compare numerical coordinates rather than screenshots: themes, regression
lines, smoothing, random-effect standardization, and VPC binning differ by
default. Six observation/residual scatter comparisons, three conditional-mode
eta comparisons, and the Cl/V eta-matrix panel are checked. The absolute
tolerance is `1e-4`, allowing the rounding in Monolix text exports relative to
the native plotting API's full precision. This is not a claim that every native
Monolix plot has an identical ggPMX equivalent.

For controlled comparisons, `GGPMX_NATIVE_SMOOTHING=off` (the default) disables
both the native spline and eta regression line; the local comparator disables
ggPMX smoothing too. `GGPMX_NATIVE_SMOOTHING=default` leaves native trend settings
unspecified while still matching conditional modes. Native exports include
`*-layers.csv` metadata to verify which trend layers were actually drawn.
`GGPMX_NATIVE_EXPORT_VPC=false` avoids re-exporting simulations when only trend
comparisons are needed.

`GGPMX_NATIVE_SMOOTHING=on` explicitly enables Monolix's spline and disables
the eta linear regression line. Compare these plots with ggPMX LOESS enabled,
labelling the methods separately: Monolix spline versus ggPMX LOESS. This view
is a qualitative trend comparison, not a numerical equivalence check.

On the joint warfarin demo, all 15 unsmoothed comparisons passed for each
endpoint. Separately, defaults differ: ggPMX adds LOESS to observation/residual
scatter plots and eta-matrix scatter panels; Monolix adds no trend to the former
and a straight regression line to eta correlation panels. The eta LOESS curve
can extend beyond ggPMX's fixed y scale (one fitted grid value in the Cl/V panel),
causing line-removal warnings despite all subject points being retained. These
default curves are not equivalent regression tests and their shapes should not
be compared as if they were produced by the same method.
