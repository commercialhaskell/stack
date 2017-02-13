#!/usr/bin/env bash

set -eu
cd "$(dirname "$0")"

#
# Constants and defaults
#

DOCKER_REPO=fpco/stack-build
PUSH=false
DRY=false

#
# Functions
#

# Print usage information and exit with failure status. First argument is an
# error message.
usage() {
    echo "$0: $1" >&2
    echo
    echo "Usage: $0 [--push] [--dry-run] lts-X.Y|lts-X|lts]"
    echo "See README.md for more information."
    echo
    exit 1
}

# Print a command, and if --dry-run disabled also run it
dry() {
    echo ">>> $*"
    [[ $DRY = true ]] || "$@"
}

# Push an image if --push is enabled (otherwise do nothing)
push() {
    [[ $PUSH = false ]] || dry docker push "$1"
}

# Tag an image, and then push it if --push is enabled
tagpush() {
    dry docker tag "$1" "$2"
    push "$2"
}

#
# Parse command-line
#

LTS_SLUG_ARG=
while [[ $# -gt 0 ]]; do
    case "$1" in
        --push)
            PUSH=true
            shift
            ;;
        --dry-run)
            DRY=true
            shift
            ;;
        -*)
            usage "Unknown option: $1"
            ;;
        *)
            if [[ -n "$LTS_SLUG_ARG" ]]; then
                usage "Cannot specify multiple snaphots: $1"
            fi
            LTS_SLUG_ARG="$1"
            shift
            ;;
    esac
done

#
# Determine actual snapshot version from aliases
#

SNAPSHOTS="$(mktemp "lts-snapshots.json.XXXXXX")"
trap "rm -f \"$SNAPSHOTS\"" EXIT
wget -qO- https://www.stackage.org/download/lts-snapshots.json >"$SNAPSHOTS"

case "$LTS_SLUG_ARG" in
    "")
        usage "Missing argument: snapshot or alias"
        ;;
    lts-*.*)
        LTS_SLUG="$LTS_SLUG_ARG"
        ;;
    *)
        LTS_SLUG=$(jq -r ".[\"$LTS_SLUG_ARG\"]" "$SNAPSHOTS")
        if [[ -z "$LTS_SLUG" || "$LTS_SLUG" = "null" ]]; then
            echo "$0: Cannot find LTS version for slug: $LTS_SLUG_ARG" >&2
            exit 1
        fi
        ;;
esac

LTS_VERSION="${LTS_SLUG#lts-}"
LTS_MAJOR="${LTS_VERSION%.*}"
LTS_MINOR="${LTS_VERSION#*.}"

#
# Determine latest LTS version
#

LATEST_LTS_SLUG=$(jq -r ".[\"lts\"]" $SNAPSHOTS)
LATEST_LTS_VERSION="${LATEST_LTS_SLUG#lts-}"
LATEST_LTS_MAJOR="${LATEST_LTS_VERSION%.*}"
LATEST_LTS_MINOR="${LATEST_LTS_VERSION#*.}"

#
# Determine latest minor version of the selected major version
#

MAJOR_LATEST_LTS_SLUG=$(jq -r ".[\"lts-$LTS_MAJOR\"]" $SNAPSHOTS)
MAJOR_LATEST_LTS_VERSION="${MAJOR_LATEST_LTS_SLUG#lts-}"
MAJOR_LATEST_LTS_MAJOR="${MAJOR_LATEST_LTS_VERSION%.*}"
MAJOR_LATEST_LTS_MINOR="${MAJOR_LATEST_LTS_VERSION#*.}"

#
# Find the Dockerfile for the selected snapshot
#

if [[ -s "$LTS_SLUG/Dockerfile" ]]; then

    # If there is an exact match, build and push that image

    dry docker build -t "$DOCKER_REPO:$LTS_SLUG" --build-arg "LTS_SLUG=$LTS_SLUG" $LTS_SLUG
    push "$DOCKER_REPO:$LTS_SLUG"
else

    # If no exact match, find a dockerfile for any earlier minor version of the
    # selected major version, and just create a new tag rom version's image with the selected
    # minor version (assuming that nothing needs to change), and push it.

    minor=$(( LTS_MINOR - 1 ))
    while [[ ! -s "lts-$LTS_MAJOR.$minor/Dockerfile" && $minor -ge 0 ]]; do
        minor=$(( minor - 1 ))
    done
    if [[ $minor -lt 0 ]]; then
        echo "$0: Cannot find any Dockerfile for LTS major version" >&2
        exit 1
    fi
    dry docker pull "$DOCKER_REPO:lts-$LTS_MAJOR.$minor" || true
    tagpush "$DOCKER_REPO:lts-$LTS_MAJOR.$minor" "$DOCKER_REPO:$LTS_SLUG"
fi

#
# Create and push additional tags
#

# If we select the latest minor version for the selected major version, then
# also create and push an 'lts-X' tag.
if [[ "$MAJOR_LATEST_LTS_VERSION" = "$LTS_VERSION" ]]; then
    tagpush "$DOCKER_REPO:$LTS_SLUG" "$DOCKER_REPO:lts-$LTS_MAJOR"
fi

# If we selected the latest LTS snapshot, also create and push the 'lts' and 'latest' tags.
if [[ "$LATEST_LTS_VERSION" = "$LTS_VERSION" ]]; then
    tagpush "$DOCKER_REPO:$LTS_SLUG" "$DOCKER_REPO:lts"
    tagpush "$DOCKER_REPO:$LTS_SLUG" "$DOCKER_REPO:latest"
fi
