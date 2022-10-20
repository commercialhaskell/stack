#!/bin/sh
set -eu

stack build && stack run -- ls dependencies constraints > "$1"