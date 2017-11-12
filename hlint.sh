#!/usr/bin/env bash

set -eux

hlint src/
hlint src/ --cpp-define=WINDOWS=1
hlint test/ --cpp-simple
