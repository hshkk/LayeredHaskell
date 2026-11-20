# Layered Haskell
[![Tests](https://img.shields.io/github/actions/workflow/status/hshkk/LayeredHaskell/ci.yml?branch=main&label=tests)](https://github.com/hshkk/LayeredHaskell/actions/workflows/ci.yml)
[![Documentation](https://img.shields.io/badge/docs-Haddock-blue)](https://hshkk.github.io/LayeredHaskell/)

**Layered Haskell** is a _layered_ functional language in which users can enable and disable various language features at will. 
The modularity of Layered Haskell enables language variants that support a gentle introduction to functional programming for novices while also providing a means for exploring language extensions.

> **Documentation:** [https://hshkk.github.io/LayeredHaskell/](https://hshkk.github.io/LayeredHaskell/)

> **Contribution guidelines:** [CONTRIBUTING.md](./CONTRIBUTING.md)

> **License:** [MIT](./LICENSE)

# Installation

Download the archive corresponding to your operating system from the [latest release](https://github.com/hshkk/LayeredHaskell/releases). It should contain several executable files that you can run directly from the terminal.

|Archive|Operating system|
|---|---|
|`LayeredHaskell-linux.tar.gz`|Linux (Ubuntu)|
|`LayeredHaskell-win.zip`|Windows|
|`LayeredHaskell-osx.tar.gz`|macOS|

If you do not see your operating system, you may have to follow the instructions in the [Building From Source](#building-from-source) section to build the binaries yourself.

## Building From Source

If the executable fails to run, your machine may not be compatible with the operating system environments used to build the binaries on GitHub's servers. If so, you will have to build it yourself.

1. Install [GHC](https://www.haskell.org/ghc/) (the Glasgow Haskell Compiler) and [Cabal](https://www.haskell.org/cabal/) (a package manager for Haskell libraries and programs). The easiest way to do this is **GHCup**, a general-purpose installer for Haskell tools. Follow the instructions at [https://www.haskell.org/ghcup/](https://www.haskell.org/ghcup/), then install and select compatible versions as follows.
```
ghcup install ghc 9.6.7
ghcup set ghc 9.6.7
ghcup install cabal 3.12.1.0
ghcup set cabal 3.12.1.0
```
2. Verify the installation by executing the following, which should print `9.6.7` and `3.12.1.0` respectively.
```
ghc --version
cabal --version
```

3. Execute the following commands. `scripts/Build.hs` is a script for generating the necessary binaries. Note that the first run may take a while, since Cabal has to download and compile various dependencies. 
```
git clone https://github.com/hshkk/LayeredHaskell.git
cd LayeredHaskell
runghc scripts/Build.hs
```

4. After the script finishes its execution, check `dist/LayeredHaskell-<os>`, where `<os>` is the name of your operating system. (This will in most cases match the names indicated at the beginning of the [Installation](#installation) section.) Inside you will find a set of executables generated specifically for your machine.

## Local Development

Execute the following commands.
```
git clone https://github.com/hshkk/LayeredHaskell.git
cd LayeredHaskell
```
Reference the [Usage](#usage) section for information on what commands are available.

Note that it is _not_ recommended to use `cabal install` on either `LayeredHaskell` or `LayeredHaskell-make`, as the generated executable will likely not function as intended, if at all. One should only use `cabal run` as the [Usage](#usage) section describes, or write a (local) command alias.

# Usage

This section applies only to developers.

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

### Additional information

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

