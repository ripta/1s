#!/usr/bin/env bash
#
# Run the gauntlet:         ./scripts/gauntlet.sh
# Update the golden files:  ./scripts/gauntlet.sh -u

set -euo pipefail

do:build() {
  local rv=0
  TIMEFORMAT="BUILD %2Rs"
  time make build-debug 2>&1 || rv=$?
  return $rv
}

update=
while getopts "u" arg; do
  case "${arg}" in
    u)
      update=1
      ;;
    *)
      ;;
  esac
done

if res=$(do:build); then
  echo "OK do:build"
else
  echo "$res"
  echo "ERROR do:build"
  exit 1
fi

for file in tests/*.1s; do
  out="${file/.1s/.out}"
  if [[ -n "$update" ]]; then
    echo "UPDATE $out"
    ./target/debug/1s -q prelude $file > $out
  elif [[ ! -f "$out" ]]; then
    echo "EMPTY $out"
    touch $out
  fi
  if diff -u $out <(./target/debug/1s -q prelude $file); then
    echo "OK $file"
  else
    echo "ERROR $file"
  fi
done
