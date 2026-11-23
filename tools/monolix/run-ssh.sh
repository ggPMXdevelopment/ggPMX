#!/usr/bin/env bash
set -euo pipefail

# Only trusted local code is sent to the licensed host. No CI credentials are used.
host=${GGPMX_SSH_HOST:?Set GGPMX_SSH_HOST to your licensed host}
modules=${GGPMX_REMOTE_MODULES:?Set GGPMX_REMOTE_MODULES to your site module names}
mode=${GGPMX_MONOLIX_MODE:-regenerate}
output=${1:?Usage: bash tools/monolix/run-ssh.sh NEW_LOCAL_OUTPUT_DIRECTORY}
scratch=${PA_SCRATCH_DIR:?Set PA_SCRATCH_DIR to a scratch directory}
[[ ! -e "$output" ]] || { echo "Output path already exists" >&2; exit 1; }
root=$(git rev-parse --show-toplevel)
staging=$(mktemp -d "$scratch/monolix.XXXXXX")
git archive HEAD | tar -xf - -C "$staging"
git diff --binary HEAD -- R NEWS.md > "$staging/working-tree.patch"
if [[ -s "$staging/working-tree.patch" ]]; then
  git -C "$staging" apply "$staging/working-tree.patch"
fi
mkdir -p "$staging/tools/monolix"
cp "$root"/tools/monolix/*.R "$root"/tools/monolix/*.csv "$root"/tools/monolix/*.sh "$root"/tools/monolix/*.md "$staging/tools/monolix/"
git rev-parse HEAD > "$staging/tools/monolix/source-commit.txt"
COPYFILE_DISABLE=1 tar --no-xattrs -czf "$staging/source.tar.gz" --exclude=source.tar.gz -C "$staging" .
# Compute nodes cannot see the login node's /tmp; use shared scratch instead.
remote=$(ssh -o BatchMode=yes "$host" 'base="$HOME/.cache/ggPMX-monolix-tests"; mkdir -p "$base"; mktemp -d "$base/run.XXXXXX"')
scp -q "$staging/source.tar.gz" "$host:$remote/source.tar.gz"
status=0
printf -v remote_command 'bash -s -- %q %q %q' "$remote" "$modules" "$mode"
ssh -o BatchMode=yes "$host" "$remote_command" <<'REMOTE' || status=$?
set -euo pipefail
work=$1
modules=$2
mode=$3
mkdir -p "$work/source" "$work/library"
tar -xzf "$work/source.tar.gz" -C "$work/source"
cat > "$work/job.sh" <<'JOB'
#!/usr/bin/env bash
set +eu
source /etc/profile
set -euo pipefail
export GGPMX_WORK="$1" GGPMX_MODULES="$2" GGPMX_MONOLIX_MODE="$3"
export GGPMX_LIBRARY="$1/library" GGPMX_HARNESS="$1/source/tools/monolix"
export GGPMX_MONOLIX_OUTPUT="$1/results"
export GGPMX_MONOLIX_DEMOS="$HOME/lixoft/monolix/monolix2024R1/demos"
export OMP_NUM_THREADS=1
module load $GGPMX_MODULES
# The site's R profile adds this library, but R CMD INSTALL skips that profile.
export R_LIBS="$GGPMX_LIBRARY:$EBROOTR/installed_pkg:${R_LIBS:-}"
R CMD INSTALL --no-docs --no-help -l "$GGPMX_LIBRARY" "$GGPMX_WORK/source"
R --no-save --no-restore --slave -f "$GGPMX_HARNESS/run.R"
JOB
printf -v job_command 'bash %q %q %q %q' "$work/job.sh" "$work" "$modules" "$mode"
bsub -K -n 1 -W 20 -M 4000 -R 'span[hosts=1]' -o "$work/job.log" "$job_command"
REMOTE
mkdir -p "$output"
scp -q "$host:$remote/job.log" "$output/" || status=1
scp -qr "$host:$remote/results" "$output/" || status=1
cp "$staging/tools/monolix/source-commit.txt" "$output/"
cp "$staging/working-tree.patch" "$output/"
exit "$status"
