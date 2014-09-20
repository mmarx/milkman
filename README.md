# milkman

Computes all minimal conceptual boolean factorizations of a given
formal context.  Currently supports reading contexts in Burmeister and
conexp format, and exports to Burmeister format.  Support for minimal
pre-conceptual factorizations is currently work-in-progress.

## Build status
[![Build Status](https://travis-ci.org/mmarx/milkman.svg?branch=develop)](https://travis-ci.org/mmarx/milkman)

## Setup
    cabal install --only-dependencies
    cabal configure
    cabal build

It might be a good idea to use hsenv/cabal sandboxes as a build environment.

## Usage
    milkman path-to-context.cxt
