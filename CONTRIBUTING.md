Thank you for contributing to *Masna3*! Please read this document:

## Project Setup

We need you to read and acknowledge our [Code of Conduct][CoC] document.

The compiler version used is described in the `cabal.project` file.
`cabal-install` version 3.14 or higher is needed.

The following Haskell command-line tools will have to be installed:

* `postgresql-migration`: To perform schema migrations
* `fourmolu`: To style the code base. Version is 0.18.0.0
* `hlint` v3.10 & `apply-refact`: To enforce certain patterns in the code base ("lint")
* `cabal-gild`: To style the cabal files
* `ghc-tags`: To generate tags for the project

Outside of the masna3 root directory run:
```
cabal install --ghc-options="-j" -j postgresql-migration hlint cabal-gild ghc-tags
```

(Some of the above packages could have incompatible dependencies, so consider installing them separately with `cabal install`)

## Build rules

The project uses a `Makefile` to run usual build/clean/style rules.
Please type `make help` to see these rules.

[CoC]: https://github.com/tchoutri/masna3/blob/main/CODE_OF_CONDUCT.md

## Configuration

*Masna3* uses environment variables for configuration. The base configuration lives in `environment.sh`. Create a file called `environment.local.sh` and source `environment.sh` to overwrite the variables you need.

You can use [`direnv`](https://direnv.net/) to auto-load your local configuration.
