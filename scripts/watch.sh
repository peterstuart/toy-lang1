#!/usr/bin/env bash

SCRIPTS_DIR=$(dirname "$0")
ls "$1" | entr "$SCRIPTS_DIR"/compile.sh "$1" "$2"
