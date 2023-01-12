
# git-freq ![ci](https://github.com/fujimura/git-freq/actions/workflows/ci.yaml/badge.svg)

A Git subcommand to show total addition, deletion per file

## Installation

```
$ stack install git-freq
```

## Usage

Just hit `git freq` in the repository. Total addition, deletion per file will be shown as a csv in following format.

`file name, addition, deletion`

See `git freq --help` for more options.

## Example

[Lens](https://github.com/ekmett/lens) at [2587bb01](https://github.com/ekmett/lens/commit/2587bb01a1f63199130b15c9e65bf4557480318d)

```
$ git freq | head
src/Control/Lens.hs,5365,5263
src/Control/Lens/Fold.hs,5885,3471
src/Control/Lens/Internal.hs,4205,4123
src/Control/Lens/Type.hs,3493,2869
src/Control/Lens/TH.hs,3530,2066
src/Control/Lens/Setter.hs,3097,1846
src/Control/Lens/Internal/Zipper.hs,2432,2436
src/Control/Lens/Traversal.hs,2846,1628
src/Control/Exception/Lens.hs,2646,1697
src/Control/Lens/Plated.hs,2395,1715
```

## How to run tests

```
$ cabal test
```

## Contributing

- Fork it
- Create your feature branch (git checkout -b my-new-feature)
- Commit your changes (git commit -am 'Add some feature')
- Push to the branch (git push origin my-new-feature)
- Create new Pull Request
