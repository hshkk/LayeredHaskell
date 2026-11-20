# Layered Haskell
[![tests](https://img.shields.io/github/actions/workflow/status/hshkk/LayeredHaskell/ci.yml?branch=main&label=tests)](https://github.com/hshkk/LayeredHaskell/actions/workflows/ci.yml)
[![Documentation](https://img.shields.io/badge/docs-Haddock-blue)](https://hshkk.github.io/LayeredHaskell/)

**Layered Haskell** is a _layered_ functional language in which users can enable and disable various language features at will. 
The modularity of Layered Haskell enables language variants that support a gentle introduction to functional programming for novices while also providing a means for exploring language extensions.

> **Documentation:** [https://hshkk.github.io/LayeredHaskell/](https://hshkk.github.io/LayeredHaskell/)

> **Contribution guidelines:** [CONTRIBUTING.md](./CONTRIBUTING.md)

> **License:** [MIT](./LICENSE)

# Installation

Clone the repository and execute `cd LayeredHaskell`.

It is _not_ recommended to use `cabal install` on either `LayeredHaskell` or `LayeredHaskell-make`, as the program as it exists will likely not function as intended, if at all. One should use `cabal run` as the [Usage](#usage) section describes, or write a (local) command alias. This may be changed in the future.

# Usage

This project uses [Cabal](https://www.haskell.org/cabal/), a package manager for Haskell libraries and programs. The current project has been tested against version 3.14.2.0. If a newer version is already installed, one may try passing the `--allow-newer` flag into the `cabal` executable.

Some useful commands are as follows.

| Command  | What it does |
| -------- | ------- |
| `cabal update`  | Refreshes the local list of known packages from [Hackage](https://hackage.haskell.org/), the central package archive for Haskell. |
| `cabal build` | Compiles libraries and executables within the current project. |
| `cabal run`  | Compiles and executes the project's main executable. |

## Enabling Features

To enable (or disable) features, execute the following.
```
cabal run LayeredHaskell-make -- set <features>
```
Substitute `<features>` for a (nonempty) set of features separated by spaces.
To see what features can be enabled, refer to the [What's available?](#whats-available) section.

Execute the following command to inspect the set of enabled features without changing the existing configuration.
```
cabal run LayeredHaskell-make -- show
```

If you would like to inspect the source files corresponding to the set of enabled features, run `cabal build` after executing `set` and check the contents of `src/LH/Feature/`.

### What's available?

For a list of features, execute the following.
```
cabal run LayeredHaskell-make -- ls
```
For a list of *all* features (that is, including hidden ones), execute the following. Note that hidden features cannot be manually enabled, and an error will be thrown if one tries to manually enable a hidden feature.
```
cabal run LayeredHaskell-make -- ls a
```

For a list of all valid feature combinations, execute the following.
```
cabal run LayeredHaskell-make -- ls c
```

### Additional Information

While these commands should cover most needs during development, for a complete list, execute the `help` command as follows.
```
cabal run LayeredHaskell-make -- help
```

## Execution

The main executable can be invoked as follows. (Alternatively, `LayeredHaskell` can be replaced with `exes`.)
```
cabal run LayeredHaskell -- <filepath>
```
Currently, the file located at `<filepath>` will be parsed, and its abstract syntax tree will be printed (provided that there are no parse errors). This will soon be fed into a typechecker and then an interpreter.

## Disclaimer

The project is in early development; many aspects of the implementation have not yet been finalized. User discretion is advised.

