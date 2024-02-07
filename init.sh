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

# https://firefox-source-docs.mozilla.org/setup/linux_build.html#bootstrap-a-copy-of-the-firefox-source-code
step Downloading git-cinnabar if needed
if ! git cinnabar --version 2> /dev/null; then
    # https://github.com/glandium/git-cinnabar/blob/6cab9b3368f6a7a551940796beddfb8384ee7551/README.md#prebuilt-binaries
    curl -O https://raw.githubusercontent.com/glandium/git-cinnabar/0.6.3/download.py
    python3 download.py

    # NixOS: patch the prebuilt git-cinnabar binary, because the other methods
    # (cargo and build manually) yield “Protocol "https" not supported” errors
    if [ -e /etc/NIXOS ]; then
        nix-shell ../shell.nix --run 'autoPatchelf git-cinnabar'
    fi

    git cinnabar --version
fi

step Downloading git-filter-repo if needed
if ! git filter-repo --version 2> /dev/null; then
    # https://github.com/glandium/git-cinnabar/blob/6cab9b3368f6a7a551940796beddfb8384ee7551/README.md#prebuilt-binaries
    curl -O https://raw.githubusercontent.com/newren/git-filter-repo/v2.38.0/git-filter-repo
    chmod +x git-filter-repo

    git filter-repo --version
fi

step Cloning upstream if needed
if ! [ -e upstream ]; then
    # Clone mozilla-unified with git-cinnabar.
    if ! [ -e mozilla-unified ]; then
        curl -O https://hg.mozilla.org/mozilla-central/raw-file/default/python/mozboot/bin/bootstrap.py
        python3 bootstrap.py --vcs=git --no-interactive
    fi

    # Convert mozilla-unified to a bare repo, to save space.
    git clone --bare mozilla-unified upstream

    # Restore the original remote url.
    remote_url=$(git -C mozilla-unified remote get-url origin)
    git -C upstream remote set-url origin "$remote_url"

    # Download the git-cinnabar metadata.
    if ! [ -e bundle.git ]; then
        curl -LO https://community-tc.services.mozilla.com/api/index/v1/task/project.git-cinnabar.bundle.mozilla-unified/artifacts/public/bundle.git
    fi

    # Restore the git-cinnabar metadata.
    git -C upstream bundle unbundle ../bundle.git
    mkdir -p upstream/refs/cinnabar
    git bundle list-heads bundle.git | awk '{print $1}' > upstream/refs/cinnabar/metadata
fi

step Updating upstream
git -C upstream fetch

step Filtering upstream
# Cloning and filtering is much faster than git filter-repo --source --target.
# Using git clone here because we don’t care about refs/cinnabar/metadata.
git clone --bare upstream filtered
git -C filtered filter-repo --force --paths-from-file ../../style.paths
