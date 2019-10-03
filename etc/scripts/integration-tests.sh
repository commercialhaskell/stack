#!/bin/sh

exec stack build --flag stack:integration-tests stack --exec stack-integration-test
