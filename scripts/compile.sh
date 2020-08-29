#!/usr/bin/env bash

SCRIPTS_DIR=$(dirname "$0")
cat "$1" | "$SCRIPTS_DIR"/../result/bin/toy-lang1 > "$2" && prettier --write "$2" && eslint --fix "$2"
