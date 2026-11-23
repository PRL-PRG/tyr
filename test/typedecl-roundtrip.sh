#!/bin/bash
# Makes sure that the rstt REPL reads each signature of the `tyr --typedecl`
# FILEs. contractr sends these signatures to rstt.
# Usage: typedecl-roundtrip.sh RSTT FILE...
set -euo pipefail
rstt=$1
shift
queries=$(awk '/^#\|/ {
  sig = $0; sub(/^#\| *[^ ]+ : /, "", sig)
  printf "\"%d\": (%s) <= any ;;\n", NR, sig
}' "$@")
expected=$(printf '%s\n' "$queries" | grep -c ';;$' || true)
answers=$(printf '%s\n' "$queries" | "$rstt" | sed 's/\x1b\[[0-9;]*m//g')
if printf '%s\n' "$answers" | grep -q '\[Error\]'; then
  printf '%s\n' "$answers" | grep '\[Error\]' >&2
  exit 1
fi
got=$(printf '%s\n' "$answers" | grep -c ': true$' || true)
if [ "$got" != "$expected" ]; then
  echo "rstt answered $got of $expected queries" >&2
  exit 1
fi
