#!/usr/bin/env bash
# Render precomputed manuscript assets, without host R or new estimation.
# Quarto automatically writes .gitignore inside Git projects. Hide Git only from
# its child process so this repository retains its explicit no-.gitignore convention.
set -euo pipefail
cd "$(dirname "$0")/.."
if (($# == 0)); then
  set -- html docx
fi
for format in "$@"; do
  case "$format" in
    html|docx) ;;
    *) printf 'Unsupported format: %s (use html or docx)\n' "$format" >&2; exit 2 ;;
  esac
done
trap 'rm -rf .quarto' EXIT
for format in "$@"; do
  GIT_DIR=/dev/null quarto render PAPER.qmd --to "$format" > "analysis/output/log_render_${format}.txt" 2>&1
  printf 'Rendered analysis/output/manuscript/PAPER.%s\n' "$format"
done
