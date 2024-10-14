Stylo
=====

This repo contains Servo’s downstream fork of [Stylo](https://searchfox.org/mozilla-central/source/servo), the CSS engine used by Servo and Firefox (Gecko). Stylo is a full CSS engine that implements both parsing and style resolution.

The branches are as follows:

- [`upstream`](https://github.com/servo/style/tree/upstream) has upstream mozilla-central filtered to the paths we care about ([style.paths](style.paths)), but is otherwise unmodified
- [`main`](https://github.com/servo/style/tree/ci) has our downstream patches, plus the scripts and workflows for syncing with mozilla-central, to be rebased onto `upstream`

## Building Servo against your local Stylo

Assuming your local `servo` and `stylo` directories are siblings, you can build `servo` against `stylo` by adding the following to `servo/Cargo.toml`:

```toml
[patch."https://github.com/servo/stylo.git"]
derive_common = { path = "../stylo/derive_common" }
malloc_size_of = { path = "../stylo/malloc_size_of" }
selectors = { path = "../stylo/selectors" }
servo_arc = { path = "../stylo/servo_arc" }
servo_atoms = { path = "../stylo/atoms" }
size_of_test = { path = "../stylo/size_of_test" }
static_prefs = { path = "../stylo/style_static_prefs" }
style_config = { path = "../stylo/style_config" }
style_derive = { path = "../stylo/style_derive" }
style = { path = "../stylo/style" }
style_traits = { path = "../stylo/style_traits" }
```

## Syncing `upstream` with mozilla-central

Start by generating a filtered copy of mozilla-central. This will cache the raw mozilla-central in `_cache/upstream`, storing the result in `_filtered`:

```sh
$ ./sync.sh _filtered
```

If `_filtered` already exists, you will need to delete it and try again:

```sh
$ rm -Rf _filtered
```

Now overwrite our `upstream` with those commits and push:

```sh
$ git fetch -f --progress ./_filtered master:upstream
$ git push -fu --progress origin upstream
```

## Rebasing `main` onto `upstream`

Start by fetching `upstream` into your local repo:

```sh
$ git fetch -f origin upstream:upstream
```

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
$ ./start-rebase.sh upstream
$ ./start-rebase.sh upstream -i     # interactive
$ git rebase --continue             # not ./start-rebase.sh --continue
$ git rebase --abort                # not ./start-rebase.sh --abort
```

Or if we aren’t ready to rebase onto the tip of upstream:

```sh
$ ./start-rebase.sh upstream~10 -i
```
