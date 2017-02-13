dockerfiles/stack-build
=======================

Build [fpco/stack-build](https://hub.docker.com/r/fpco/stack-build/) Docker
images. This script and its Dockerfiles are used for building images for LTS >=
8.0.

Usage
-----

    ./build.sh [--push] [--dry-run] lts-X.Y|lts-X|lts]"

Options
-------

`--help`: show this help

`--push`: push the image after building/tagging it

`--dry-run`: print the important commands that will be run, but don't actually
    run them

Argument
---------

If you specify and exact LTS version, that image will be built. If you only
specify the major version, then an image for the latest minor version for that
major version will be built. If you specify 'lts', an image for the latest LTS
snapshot is built.

This searches for a Dockerfile for the selected snapshot in
`lts-X.Y/Dockerfile`, and if one isn't found reuses the same image as built the
most recent `lts-X.Y/Dockerfile` found for earlier minor versions of the same
major version.
