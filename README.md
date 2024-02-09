style
=====

This repo contains Servo’s downstream fork of Stylo.

The branches are as follows:

- [`upstream`](https://github.com/servo/style/tree/master) has upstream mozilla-central filtered to the paths we care about ([style.paths](style.paths)), but is otherwise unmodified
- [`main`](https://github.com/servo/style/tree/ci) has our downstream patches, plus the scripts and workflows for syncing with [mozilla-central](https://searchfox.org/mozilla-central/source/servo), to be rebased onto `upstream`

## Syncing `upstream`

Start by generating a filtered copy of mozilla-central. This will cache the raw mozilla-central in `_cache/upstream`, storing the result in `_filtered`:

```sh
$ ./sync.sh _filtered
```

Now overwrite our `upstream` with those commits and push:

```sh
$ git fetch -f --progress ./_filtered master:upstream
$ git push -fu --progress origin upstream
```

## Rebasing `main` onto `upstream`

In general, the filtering process is deterministic, yielding the same commit hashes each time, so we can rebase normally:

```sh
$ git rebase upstream
```

But if the filtering config changes or Mozilla moves to GitHub, the commit hashes on `upstream` may change. In this case, we need to tell git where the old upstream ends and our own commits start (notice the `~`):

```sh
$ git log --pretty=\%H --grep='Servo initial downstream commit'
e62d7f0090941496e392e1dc91df103a38e3f488

$ git rebase --onto upstream e62d7f0090941496e392e1dc91df103a38e3f488~
Successfully rebased and updated refs/heads/main.
```

`start-rebase.sh` takes care of this automatically, but you should still use `git rebase` for subsequent steps like `--continue` and `--abort`:

```sh
$ ./rebase.sh
$ ./rebase.sh -i            # interactive
$ git rebase --continue     # not ./start-rebase.sh --continue
$ git rebase --abort        # not ./start-rebase.sh --abort
```
