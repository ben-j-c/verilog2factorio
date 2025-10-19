#!/bin/bash

git submodule update --init --recursive

if [ ! $? -eq 0 ] ; then
	git clone --depth 1 --branch v0.52  https://github.com/YosysHQ/yosys.git
	cd yosys
	git submodule update --init --recursive
	cd ..
fi

set -euo pipefail

rust() {
  cargo build --release
}

yosys() {
  cd yosys
  make config-gcc 
  make -j`nproc`
}

rust >rust.log 2>&1 & pid1=$!
yosys | tee yosys.log & pid2=$!

echo "Building binaries"

wait "$pid1"
echo "V2F done"
wait "$pid2"
echo "Yosys done"

echo "All done"