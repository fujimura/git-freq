# git-freq

A Git subcommand to detect frequently changed code in the repository

## Installation

```
$ cabal install git-freq
```

## Usage

Just hit `git freq` in the repository.

## Example

[Ruby on Rails](https://github.com/rails/rails) at [a6c8cde835](https://github.com/rails/rails/commit/a6c8cde83526e4ec5b1212b5e6d1e512ebf7c0ec)

```
$ git freq | head
activerecord/CHANGELOG.md,12867,12139
activerecord/lib/active_record/base.rb,10778,10172
actionpack/test/controller/routing_test.rb,10338,8400
activerecord/lib/active_record/associations.rb,10213,8248
actionpack/CHANGELOG.md,8840,8697
railties/html/javascripts/prototype.js,8379,8280
activerecord/CHANGELOG,7923,7921
guides/source/active_support_core_extensions.md,9588,5710
actionpack/CHANGELOG,6845,6838
railties/guides/source/initialization.textile,6847,6582
```

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

TODO: Write contribution instructions here
