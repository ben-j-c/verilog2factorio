#!/bin/bash

export V2F_ROOT=$(readlink -f $(dirname $(readlink -f "$BASH_SOURCE")))
echo "V2F_ROOT = $V2F_ROOT"

alias v2f="$V2F_ROOT/target/release/v2f"

