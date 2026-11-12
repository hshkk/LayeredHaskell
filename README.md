# Layered Haskell
**Layered Haskell** is a _layered_ functional language in which users can enable and disable various language features at will. 
The modularity of Layered Haskell enables language variants that support a gentle introduction to functional programming for novices while also providing a means for exploring language extensions.

> **Documentation:** [https://hshkk.github.io/LayeredHaskell/](https://hshkk.github.io/LayeredHaskell/)

> **Contribution guidelines:** [CONTRIBUTING.md](./CONTRIBUTING.md)

> **License:** [MIT](./LICENSE)

# Installation

Clone the repository and execute `cd LayeredHaskell`.

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
cabal run LayeredHaskell-make -- set ...
```
Substitute `...` for a (nonempty) set of features separated by spaces.

If you would like to inspect the source files corresponding to the set of enabled features, run `cabal build` afterwards and check the contents of `src/LH/Feature/`.

### What's available?

For a list of features, execute the following.
```
cabal run LayeredHaskell-make -- ls
```
For a list of *all* features (that is, including hidden ones), execute the following.
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

