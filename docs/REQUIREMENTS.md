# Requirements

Our goal is to create a _layered_ implementation of a small Haskell-esque functional language. By layered, we mean a series of mini languages, or _levels_ that grow in features and thus expressiveness. This design choice raises several intriguing questions! What features should be added? What should be omitted? Can we obtain level-specific error messages? How do we eliminate redundancy in the implementation?

Functional languages can be somewhat vexing for novices due to complicated error messages (especially Haskell) incurred by complex features and language-specific idiosyncrasies. This can make the transition from imperative programming quite jarring and frustrating. By introducing language levels, we can provide a systematic way to progressively introduce language features without overwhelming students. Moreover, these levels can meaningfully serve as laboratories for programming language research, as complex features are stripped away.

## Functional Requirements
> **FR1:** A user can enable or disable a desired language feature. The parser, interpreter, and typechecker should be able to accommodate this change without static type errors.

> **FR2:** A user receives clarifying error messages in the context of enabled features. We define clarifying error messages as those _not_ reflecting disabled features, and those pointing to specific places in the source that incurred the error message.

## Non-Functional Requirements
> **NFR1:** A language feature is enabled or disabled via a single, clearly-documented command or input.

> **NFR2:** A language feature is ideally implemented via a single change to the implementation structure. In other words, the addition of one feature should, in general, not incur several changes across multiple files.

## Constraints
> **C1:** Ideally executable without installation of Haskell or GHC on a local machine. Alternatively, executable on the ENGR servers.

> **C2:** Any program in Layered Haskell should also be a valid Haskell program. This restricts the syntax rules of our language, and forces us to implement layout.

> **C3:** The implementation of Layered Haskell should minimize redundancy as much as possible. This prevents, for instance, a monolithic implementation that may lead to many non-trivial changes across many files. Thus, this constraint is related to **NFR2**.
