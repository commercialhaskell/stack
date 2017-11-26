#!/usr/bin/env bash

set -eux

cd "$(dirname "$0")/../.."
hlint src/
hlint src/ --cpp-define=WINDOWS=1
hlint test/ --cpp-simple
