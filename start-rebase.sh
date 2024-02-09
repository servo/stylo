#!/bin/sh
# Usage: start-rebase.sh [extra git-rebase(1) arguments ...]
# Equivalent to git rebase --onto upstream <last old upstream commit>.
set -eu

first_commit=$(git log --pretty=\%H --grep='Servo initial downstream commit')
old_base=$first_commit~
new_base=upstream

git rebase --onto "$new_base" "$old_base" "$@"
