# Design Document: Grammar-Based Test Generator

## 1. Introduction
This document outlines the design for a tool capable of generating valid source code that conforms to a language specification defined by a **Happy** grammar (`.y`) and an **Alex** lexer (`.x`). The primary use case is grammar-based fuzzing: generating random, syntactically valid test cases to verify parsers and compilers.

## 2. System Architecture
The system operates as a pipeline with three distinct stages:

1.  **Specification Parsing**: Ingesting the legacy `.y` and `.x` files and converting them into a workable Abstract Syntax Tree (AST).
2.  **Semantic Linking**: Bridging the gap between the grammar (which consumes tokens) and the lexer (which produces them) by analyzing the glue code.
3.  **Generation**: Synthesizing a stream of tokens based on the grammar and concreting them into strings based on the lexer's regular expressions.

## 3. Component Design

### 3.1 Happy Grammar Parser
The grammar parser is responsible for reading `.y` files. Unlike standard Happy parsers, this component must preserve the structure needed for generation, specifically:

*   **Parameterized Rules**: Happy supports rules like `List(p)`. The AST must explicitly represent these parameters so the generator can instantiate them (e.g., `List(Identifier)` vs `List(Integer)`).
*   **Token Declarations**: The `%token` directive contains the Haskell code pattern used to match the token (e.g., `%token ID { TokenVar $$ }`). This code pattern is the key identifier for linking.

**Proposed AST Structure:**
```haskell
data Grammar = Grammar {
    tokens :: Map String TokenDef, -- Maps matching pattern to Token Info
    rules  :: Map String [RuleDef]
}

data RuleDef = RuleDef {
    args   :: [String],            -- Parameters for the rule (e.g., "p")
    prods  :: [[Symbol]]           -- Alternative productions
}

data Symbol 
    = Terminal String              -- Refers to a Token
    | NonTerminal String [Symbol]  -- Refers to a Rule, with arguments
```

### 3.2 Alex Lexer Parser
The lexer parser reads `.x` files to understand how tokens are formed. It must handle:

*   **Macro Definitions**: Named regexes (e.g., `$digit = [0-9]`).
*   **Start Codes**: Contexts like `<0>` or `<comment>`.
*   **Rules**: The mapping between a Regex and a Haskell action code.

**Proposed AST Structure:**
```haskell
data Lexer = Lexer {
    macros :: Map String Regex,
    rules  :: [LexerRule]
}

data LexerRule = LexerRule {
    regex :: Regex,
    code  :: String -- The Haskell action code
}

data Regex
    = RChar Char
    | RSet [Char] Bool -- Bool for inversion ([^a])
    | RSeq [Regex]
    | RAlt [Regex]
    | RStar Regex
    ...
```

### 3.3 The Linker (Semantic Mapping)
The linker resolves the dependency between the Grammar and the Lexer. Since Happy and Alex are decoupled tools connected only by arbitrary Haskell code, the linker uses a heuristic approach:

1.  **Extract Pattern**: From the Happy `%token` definition, extract the token constructor (e.g., `TokenVar`).
2.  **Match Action**: Scan the Alex rules. If a rule's action code contains the same constructor, link that Regex to the Happy terminal.
3.  **Constraint Identification**: Identify "keywords" (literals like "if", "while") defined in the lexer. These must be excluded from generic identifiers to prevent collisions.

### 3.4 The Generator
The generation engine is the core logic, driven by `QuickCheck` for randomness.

#### Phase 1: Grammar Expansion (Boltzmann Sampling)
The generator starts at the root non-terminal. To guarantee termination and uniform generation of structures within a target size range, the system implements **Boltzmann Samplers**.

*   **Combinatorial Specification**: The Happy grammar is treated as a system of combinatorial species equations (e.g., `List(A) = 1 + A * List(A)`).
*   **Oracle Calculation**: The system computes the "singularities" (radius of convergence) for the generating functions associated with the grammar variables. These values are used to weight the random choices (probabilities of selecting specific productions) such that the expected size of the generated tree matches the user's request.
*   **Rejection Sampling**: To strictly enforce the target size (or size window), the generator may use rejection sampling, discarding trees that are too small or too large, which is efficient when tuned correctly via the Boltzmann parameter.
*   **Instantiation**: Parameterized rules `List(p)` are handled by substituting the generating function of the argument `p` into the equations for `List`.

#### Phase 2: String Synthesis (Regex Inversion)
When a Terminal is reached, the generator consults the Linker to find the matching Regex. It then "inverts" the regex to produce a string:
*   `[a-z]`: Uniformly select a character between 'a' and 'z'.
*   `R*` (Kleene Star): Generate a list of `R` matches, with a geometric distribution for length to ensure termination.
*   `R1 | R2`: Randomly select one branch.

#### Phase 3: Safety Constraints
A critical issue in random generation is accidental keywords. If the regex `[a-z]+` generates the string `"if"`, and `"if"` is a reserved keyword in the language, the generated code will fail to parse (it will be tokenized as `KW_IF`, not `ID`).
*   **Solution**: The generator maintains a set of all literal keywords found in the lexer. Any string generated by a generic regex (like Identifier) is checked against this set. If it matches a keyword, it is discarded and regenerated.

## 4. Implementation Details

*   **Language**: Haskell.
*   **Libraries**:
    *   `QuickCheck`: For `Gen` monad and random distribution utilities.
    *   `ReadP`: For robust parsing of the input `.y` and `.x` files.
*   **Output**: The tool outputs a flat string. Future versions may include a pretty-printer to insert newlines and indentation based on the token stream structure, which is essential for layout-sensitive languages (like Haskell or Python).
