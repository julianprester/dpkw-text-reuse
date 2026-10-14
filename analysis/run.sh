#!/usr/bin/env bash
# Run one or more pipeline scripts in the text-reuse-r container, from the project root.
# Usage: analysis/run.sh analysis/01_corpus.R [analysis/02_pairs.R ...]
set -euo pipefail
cd "$(dirname "$0")/.."
for s in "$@"; do
  echo ">>> $s"
  docker run --rm --user "$(id -u):$(id -g)" -e HOME=/tmp -v "$PWD":/proj -w /proj text-reuse-r Rscript "$s"
done
