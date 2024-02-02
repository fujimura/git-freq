# git-freq ![ci](https://github.com/fujimura/git-freq/actions/workflows/ci.yaml/badge.svg)

A Git subcommand to show total addition, deletion, count of changes per file

## Installation

```
$ cabal install git-freq
```

## Usage

Just hit `git freq` in the repository. Total addition, deletion per file will be shown as a csv in following format.

`file name, addition, deletion`

See `git freq --help` for more options.

## Example

[Lens](https://github.com/ekmett/lens) at [2587bb01](https://github.com/ekmett/lens/commit/60f773bdb8538d9023983e020b5cf7d0e1687df9)

```
$ git freq | tail
src/Control/Lens/Internal/Zipper.hs,1995,2003,108
src/Control/Lens/Setter.hs,3295,1929,148
src/Control/Lens/Traversal.hs,3430,1962,295
src/Control/Exception/Lens.hs,3498,2021,93
src/Language/Haskell/TH/Lens.hs,4566,1729,75
src/Control/Lens/Lens.hs,4183,2683,218
src/Control/Lens/TH.hs,4274,3386,236
src/Control/Lens/Internal.hs,4205,4130,195
src/Control/Lens/Fold.hs,6552,3766,281
src/Control/Lens.hs,5395,5297,195
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
