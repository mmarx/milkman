# milkman [![Build Status](https://travis-ci.org/mmarx/milkman.svg?branch=master)](https://travis-ci.org/mmarx/milkman)

Computes all minimal (pre-)conceptual boolean factorizations of a
given formal context.  Currently supports reading contexts in
Burmeister and conexp format, and exports to Burmeister format.

## Setup
    cabal install --only-dependencies
    cabal configure
    cabal build

It might be a good idea to use hsenv/cabal sandboxes as a build environment.

## Usage
    milkman -o output-prefix [--verbose|-v] [--pre-conceptual] path-to-context.cxt
