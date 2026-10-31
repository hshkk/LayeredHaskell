# Layered Haskell
**Layered Haskell** is a _layered_ functional language in which users can enable and disable various language features at will. 
The modularity of Layered Haskell enables language variants that support a gentle introduction to functional programming for novices while also providing a means for exploring language extensions.

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

For most, invoking `cabal update` then `cabal run` will suffice: it will automatically update and build the prerequisite libraries and executables.

## Disclaimer

The project is in early development; many aspects of the implementation have not yet been finalized. User discretion is advised.

