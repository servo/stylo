#!/bin/sh
# usage: init.sh <path/to/style>
set -eu

style=$1
mkdir -p cache
cd cache
export PATH="$PWD:$PATH"

step() {
    tput setaf 12
    >&2 printf '* %s\n' "$*"
    tput sgr0
}

step Downloading git-filter-repo if needed
if ! git filter-repo --version 2> /dev/null; then
    # https://github.com/glandium/git-cinnabar/blob/6cab9b3368f6a7a551940796beddfb8384ee7551/README.md#prebuilt-binaries
    curl -O https://raw.githubusercontent.com/newren/git-filter-repo/v2.38.0/git-filter-repo
    chmod +x git-filter-repo

    git filter-repo --version
fi

step Cloning upstream if needed
if ! [ -e upstream ]; then
    git clone --bare --single-branch https://github.com/mozilla/gecko-dev.git upstream
fi

step Updating upstream
branch=$(git -C upstream rev-parse --abbrev-ref HEAD)
git -C upstream fetch origin $branch:$branch

step Filtering upstream
# Cloning and filtering is much faster than git filter-repo --source --target.
# Using git clone here because we don’t care about refs/cinnabar/metadata.
git clone --bare upstream filtered
git -C filtered filter-repo --force --paths-from-file ../../style.paths
