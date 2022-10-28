#!/bin/sh
set -eu

stack run -- ls dependencies constraints > "$1"