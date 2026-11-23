# RFC: Organization and Definition of Syntax for Layered Haskell

| Field | Value |
| --- | --- |
| Author | Oscar Abelanet |
| Original date | 2026-10-25 |
| Status | Accepted |
| Decision date | 10/20/2026 |
| Deciders | Layered Haskell project team |
| Superseded by | Not applicable |

## Problem and Context

Layered Haskell is an educational programming-language project intended to introduce functional programming in stages. Instead of presenting students with the entire Haskell language immediately, or elm, our project divides a simplified version of Haskell into layers, to slowly expose more features to students and aims to provide more understandable error messages. A student should be able to write a program using only the features in their current layer, while our compiler can identify features only from that layer.

This RFC tackels the language's concrete syntax: the grammar productions used to recognize source code. It does not decide every language feature or type-checking rule. The existing design discussion named a shared lexical foundation (identifiers, constructors, digits, and literals), a basic layer **B** (including integer and Boolean constructs and simple branching and operators), and later additions for expressions and function definitions, and other features as well. For purposes of this RFC, **E** denotes the expresion features and **F** is the function features. A name such as **FB** means that features **F** and **B** are enabled together. This RFC will not cover all features that we have decided on, but more of a guide on how we are going to approach combining them.

The architectural question is how to represent shared rules and layer-specific changes. For example, a B expression might allow `x + x`, but the complete definition `double x = x + x` should only be allowed by a layer that supports user defined functions, namely **F**. Other layers may also need to replace a rule rather than simply add alternatives, as with potentially different type definitions in B and D. The grammar representation therefore needs both reuse and overwriting capabilities.

The decision is needed as we formalizes the language and connect the grammar to parsing, layer selection, tests, and error messages. Without a common approach, we could independently encode incompatible assumptions about rule precedence and inheritance. Later migration would require changing grammar files, parser integration, tests, and documentation. Doing nothing temporarily is possible while individual features are prototyped, but if we dont decide on concrete rules now, we will have conflicts in the future when trying to expand the language.

## Options

### Option A: One unified grammar with layer restrictions

Maintain one complete grammar containing all supported syntax, then use a layer setting to take out constructs not available to a particular layer. Its principal benefit is a single source of productions, with little duplication and no composition step. Its cost is that constructs that may be syntactically valid in a more complete layer must be distinguished after or during parsing to only allow grammer from the current layer. This approach puts all maintenance and code into one file but makes it more difficult to debug and add features in the future, causeing issues when our language becomes large.

### Option B: Incremental grammar extensions through superimposition

Give each feature its own grammar file containing new rules and possible changes to inherited rules. To build a selected layer, combine its dependency grammars in a defined order. A higher layer can add a production or overwrite an inherited one. This is the team's **accepted approach**.

Superimposition avoids repeating unchanged lexical and syntactic rules, and makes it much easier to debug and add individual features. Its cost is how it implements it, a dependency order, and careful handling of conflicting definitions. The team must distinguish an *extension*, which retains existing alternatives, from an *override*, which replaces a rule. A silent overwrite could accidentally remove previously accepted programs. This option is appropriate when layers share substantial syntax but occasionally need distinct definitions. The proposed implementation and examples below specify how to make this choice testable.

### Option C: Postpone a shared grammar architecture

Continue defining features and parser behavior locally, without settling how grammar files relate. However, continuing development like this would make it much more difficult to maintaion and add code between developers. A later merge could expose disagreements that are difficult to resolve once tests and examples depend on them. Thus doing nothing is a real short term option, but a poor choice for the future of this project.

## Analysis

The main criteria that we are checking for are if the layer successfully works, maintainability, ease of understanding the code to contributors, and quality error messages. The main issue is where the compelxity is. ie one large conditional grammar, superimposition, multiple repeated grammars, or future integration work.

The Haskell 2010 Language Report provides a useful reference for how Haskell defines its grammar with it's lexical syntax, expressions, declarations, and parsing conventions (Marlow, 2010, chapters 2 and 10). While it helps guide which syntax we want to simplify, it does not necessarily support Option B over the other approaches. The report also describes features, such as layout rules, that our team will need to decide whether to include or leave out.

A more relevant example is ANTLR, which allows grammars to import rules from other grammars and overwrite existing rules when necessary (ANTLR Project, n.d., “Grammar Structure”). This is similar to our proposed superimposition approach, although our project does not necessarily use ANTLR or follow its exact implementation. We will still need to define how our own grammar extensions and overwrites work and verify that they behave as expected through testing.

For a concrete example, consider the following. This is not the team's confirmed grammar notation, but just an example. The `extend` and `override` operations are conceptual and would need to be translated into an agreed upon format:

```text
Shared lexical rules:
  identifier ::= letter {letter | digit}
  integer    ::= digit {digit}

B grammar:
  atom       ::= integer | identifier | "(" expr ")"
  expr       ::= atom | expr "+" atom
  program    ::= expr

F-over-B overlay:
  extend expr with: expr atom              # function application
  override program ::= declaration | expr
  declaration ::= identifier identifier "=" expr
```

After combining F with B through superimposition, the resulting program rule would allow both declarations and expressions, while the expr rule would also include function applications and the existing B expressions. However, the B grammar on its own would still use program ::= expr and would not allow function declarations. This shows how superimposition can both add new alternatives and replace existing rules. The team will also need to define the order in which grammars are combined, such as starting with shared definitions, then B, and finally F. Invalid or circular dependencies should produce an error, but this shouldn't happen too often as the proposed current feature layout is fairly linear.

Option A could also recognize a function definition like double x = x + x, but it would need additional checks to reject it in B. Option B requires more work when combining grammar files, but makes it easier to see which features belong to each layer. One concern is that overwriting a rule could accidentally remove syntax that was previously allowed. To prevent this, the team should test both the combined grammar and the original lower layer grammar whenever changes are made. A program in a lower layer should always parse in a more complete layer.

Another important consideration is the quality of error messages. Just because a parser rejects a program does not mean it explains the problem in a way students can understand, as can be seen in Haskell's error messages. For example, the compiler should be able to distinguish between an incomplete expression like 1 + and a syntactically incorrect line. However, superimposition could make this easier by clearly identifying which layer introduces each feature, and thus features can incorperate their own error messages within themselves.

We have not yet collected benchmarks or test results comparing the performance and maintainability of these approaches. We instead began with the superimposition approach immediatly, as we thought it would be the cleanest and easiest to maintain. Because of this, our recommendation is based on the project's need to share grammar definitions while allowing individual layers to modify them, rather than on measured performance improvements.

## Recommendation and Verification Plan

I recommend continuing with the team's accepted Option B, using superimposition to combine individual grammar files for each layer. The system should follow a clearly defined dependency order, combining the necessary grammar files and applying any extensions or overwrites to create the final grammar for the selected layer. Each change should specify which rule it affects and whether it adds to or replaces that rule. If there are missing rules, conflicting overwrites, or circular dependencies, the system should report an error rather than continuing with an incorrect grammar.

The following are example tests to verify that this approach works, rather than results we have already collected. First, we should test several example programs to confirm that each layer accepts and rejects the correct syntax. For example, B should accept 1 + 2 but reject double x = x + x, since function declarations are not available in that feature. A version combining F and B should accept both examples and correctly identify the function name, parameter, and expression. Both layers should reject an incomplete expression such as 1 + and identify where the error occurs, as well as give an instructive error message that can help the student identify what is wrong and where. 

Second, we should test that superimposition does not unintentionally change lower layers. We can do this by saving the generated B grammar, combining F with B, and then generating B again. The two versions of B should be identical, and the same programs should still be accepted or rejected. We should also confirm that the combined F grammar includes the new function rules while keeping the existing B expressions. This would help identify situations where an overwrite accidentally removes or changes an existing rule.

Third, we should test how the system handles conflicting grammar definitions. For example, if two layers attempt to overwrite the same rule without a defined order, the system should report which rule and files caused the conflict. Similarly, referencing a rule that does not exist or creating circular dependencies should produce an error before parsing begins. We should also compare generated grammars after changes to make sure no rules have been unintentionally removed.

Fourth, we should check for parsing ambiguity and error-message quality. Depending on the parser we use, we can either check its reported grammar conflicts or test programs that could potentially be interpreted in multiple ways. Our goal is to have no unexplained parsing conflicts and to consistently produce the expected results for our test programs. For invalid syntax, error messages should identify the problem, its location, potentially any extra data that would be helpful in the message, and the relevant language layer when possible. For example, entering a function declaration in B should ideally produce a message explaining that functions are not supported in that layer, rather than only reporting a general syntax error. We are still undecide if we want to mention higher level features in lower layers in error messages, as we don't want to confuse students that have not yet been introduced to the higher level features/layers.

Finally, we should maintain a set of tests for every supported layer and run them whenever grammar definitions or superimposition rules change. Any changes involving overwrites should also be reviewed to confirm that existing features still work correctly. We can consider the approach successfully verified when all expected programs are accepted or rejected correctly, lower-layer grammars remain unchanged, conflicts produce appropriate errors, and error messages meet our requirements. Until these tests are completed, they represent our planned verification process rather than confirmed results.

## Constraints

**Language compatibility**. Our project is designed to support a simplified version of Haskell rather than every feature included in Haskell 2010. The grammar should follow Haskell's syntax for the features we include, while any differences or excluded features should be documented. Since one of our goals is for programs written in our language to also compile with a Haskell compiler, we should test example programs using the actual Haskell compiler rather than relying only on our parser. We should also record which compiler version and settings were used during testing.

**Licensing and intellectual property**. The Haskell 2010 Language Report allows its content to be copied under certain conditions, such as keeping its copyright notice and identifying any modifications (Marlow, 2010, copyright notice). To avoid licensing issues, we should write our own simplified grammar rules and cite the report when using it as a reference. If we decide to use an external parser generator, library, or existing grammar, we will need to review its license before including it in our project. ANTLR is currently being used as a reference for grammar organization, not necessarily as a tool we plan to use.

**Data, security, and confidentiality**. Our grammar design does not require personal student information or external services. Grammar files and test programs should be stored in our repository without including passwords, private student submissions, or other sensitive information. Layered Haskell will hopefully be on the OSU engineering servers for student that would prefer to work on there, instead of downloading it directly. Since our compiler will process code written by students, we need to treat that input as untrusted and avoid executing student programs unnecessarily during parser testing. Generated grammars should also be deterministic and thus reproducible.

**Maintainability**. To keep the project manageable as we add more features, we need consistent naming conventions, clearly documented dependencies between layers, and a way to inspect the final grammar produced through superimposition. Adding or overwriting rules in a higher layer should never directly modify the original grammar files of lower layers. Although these requirements add some complexity to implementing superimposition, they will make it easier to debug changes, review new features, and maintain the language over time.

## Delegation and Validation

No AI was used in the decisions and drafting of this final draft. We have also relegated all use of AI
in this project, and thus AI has no place for checks and validations on this project.

## Decision

**Accepted: Option B, incremental grammar extensions through superimposition.** Each layer will have its own grammar file, with higher layers building on the rules defined in lower layers. Through superimposition, these higher layers can either add new rules or overwrite existing ones when needed. For now we are convinced that this superimposition approach works, but we still have to do more testing with it to make sure it fully works.

## References

1. Marlow, Simon, editor. *Haskell 2010 Language Report*. 2010. Chapters 2, 3, 4, and 10; copyright notice. https://www.haskell.org/onlinereport/haskell2010/
2. ANTLR Project. “Grammar Structure.” ANTLR 4 Documentation, antlr/antlr4, doc/grammars.md, section “Grammar Structure.” Accessed November 22, 2026. https://github.com/antlr/antlr4/blob/dev/doc/grammars.md