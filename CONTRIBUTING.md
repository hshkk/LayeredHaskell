# Contributing to this repository

Contributors should follow these guidelines when submitting their work to this repository.

## How do I get started?

This project uses [Cabal](https://www.haskell.org/cabal/), a package manager for Haskell libraries and programs. Some useful commands to be familiar with before contributing to this repository are as follows.

| Command  | What it does |
| -------- | ------- |
| `cabal update`  | Refreshes the local list of known packages from [Hackage](https://hackage.haskell.org/), the central package archive for Haskell. |
| `cabal build` | Compiles libraries and executables within the current project. |
| `cabal run`  | Compiles and executes the project's main executable. |
| `cabal repl` | Opens an interactive session loaded with the project's components. |
| `cabal test` | Compiles and executes the project's test suites, which can be found [here](test/). |
| `cabal haddock` | Generates [Haddock](https://hackage.haskell.org/package/haddock) HTML documentation for the project. |

## What should I know before contributing?

There is a [simple Haskell CI workflow](.github/workflows/haskell.yml) that checks whether [all tests](test/) pass before pushing to or making a pull request on `main`.
It would thus be prudent to invoke `cabal test` on your local machine before attempting to make a change to `main`. 
Outside of this, it is your responsibility to ensure documentation (including comments) is up to date and reflects the implementation.

Commit messages should be succinct, informative, and free of grammatical mistakes.
Pull requests and issues are held to the same standard.

### For capstone project team members

**Each member should be an author of at least one merged pull request or accepted deliverable per sprint.** Responsibilities will be delegated at sprint planning via an issue assigned to them. The work will be split such that no issue exceeds 2-3 days. Reviews are due within 2 days, and any unreviewed work should be discussed in meetings.

**Accepted work is defined as a contribution that passes all tests, receives approval from at least one other reviewer, and is merged into `main`.** A non-programmatic contribution (such as documentation) is accepted if it has been reviewed by a teammate and linked to the issue.

Should a team member encounter difficulties in delivering accepted work by mid-sprint, the team should reassign or resize the task.

## Who should I contact if I have questions?

```
Hangil Kim   <kimhang[at]oregonstate.edu>
```
