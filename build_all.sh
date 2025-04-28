#!/usr/bin/env bash
set -euo pipefail

rust() {
  cargo build --release
}

yosys() {
  git submodule update --init --recursive
  ( cd yosys && make config-gcc && make && sudo make install )
}

rust >rust.log 2>&1 & pid1=$!
yosys >yosys.log 2>&1 & pid2=$!

wait "$pid1"
wait "$pid2"

echo "All done"