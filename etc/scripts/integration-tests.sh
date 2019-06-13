#!/bin/sh

exec stack build --flag stack:integration-tests stack --interleaved-output --exec stack-integration-test
