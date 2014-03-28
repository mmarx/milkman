# milkman

Computes all minimal conceptual boolean factorizations of a given
formal context.  Currently supports reading contexts in Burmeister and
conexp format, and exports to Burmeister format.

## Setup
cabal configure && cabal build

It might be a good idea to use hsenv as a build environment.

## Usage
milkman path-to-context.cxt