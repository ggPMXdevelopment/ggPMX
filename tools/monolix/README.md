# Monolix demo smoke tests

Run from the repository root on a trusted local machine with SSH access to
your licensed host. Configure the host and module names locally; the launcher
has no site-specific defaults:

```bash
export GGPMX_SSH_HOST=your-ssh-host
export GGPMX_REMOTE_MODULES="your-R-module your-Monolix-module"
bash tools/monolix/run-ssh.sh /absolute/path/to/new-results
```

`PA_SCRATCH_DIR` must name an existing scratch directory. The launcher uploads
**committed HEAD plus tracked changes under R/ and NEWS.md,
and the current harness** (not other working-tree changes),
installs ggPMX into an isolated remote library, and submits a serial LSF job
(one core, 4 GB, 20-minute limit). It downloads logs and results even after test
failure. Remote scratch files are retained for diagnosis. No vendor demos or
results are committed to this repository. Ordinary package tests and GitHub
Actions do not run this harness.

Remote runs use `~/.cache/ggPMX-monolix-tests` because login-node `/tmp` is not
shared with compute nodes. The SSH command must remain connected while the LSF
job runs. If interrupted, the job may continue: inspect it with `bjobs`/`bpeek`
and recover its `job.log` and `results` directory with `scp` rather than launching
a duplicate job.

The manifest selects theophylline, warfarin PK, and both endpoints of the
warfarin PK/PD demo. It can be extended with additional continuous-outcome
projects that have fitted results. This initial workflow does **not** refit
models or validate statistical model adequacy.

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
