# Agent Guide for amt-word64

This document provides essential context and guidelines for agents working on the `amt-word64` library.

## Project Overview

`amt-word64` is a Haskell library providing a map implementation for `Word64` keys based on **Array-Mapped Tries (AMT)**. It aims for high performance by using 64-way branching (6 bits per level) and `SmallArray` from the `primitive` package.

## Core Architecture

### Data Structures

- **`Word64Map a`**:
  - `Branch !Bitmap !(SmallArray (Word64Map a))`: An internal node.
  - `Leaf !Word64 a`: A terminal node containing a key and value.
- **`Bitmap`**: A `newtype` wrapper around `Word64`, where each set bit represents a child's presence in the `SmallArray`.
- **`Shift`**: An `Int` representing the bit position (multiples of 6) currently being inspected.

### Invariants

1. **Canonical empty**: The empty map is represented by a 'Branch' with an empty bitmap. Use the `null` function to check for emptiness.
2. **No redundant branches**: A 'Branch' must never have exactly one sub-node if that node is a 'Leaf'. Such branches must be collapsed. Branches with a single 'Branch' sub-node are allowed.
3. **Bitmap consistency**: `popCount bm` must always equal `sizeofSmallArray ary`.
4. **Prefix consistency**: All keys in a subtree must share the same prefix for the bits more significant than the current shift.

## Development and Operations

### Building and Testing

Use standard `cabal` commands for development:

- **Build**: `cabal build` (use `--enable-tests` to include test targets).
- **Test**: `cabal test` or `cabal run amt-word64-test`.
- **Clean**: `cabal clean`.

### Development Workflow

1. **Feature Branches**: Never commit directly to `master`. Always work in a feature branch and prepare a PR. All changes shall be committed with an explanation and mention of the current model. Do not ask for permission before creating commits on feature branches.
2. **Git Operations**: You have standing permission to create commits on feature branches, switch branches, and use non-destructive git commands (e.g., `status`, `log`, `diff`, `add`, `stash`, `switch`). Do not use destructive commands like `reset --hard` unless explicitly asked.
3. **Formatting**: All Haskell code must be formatted using `fourmolu`. Run this after each change and before committing:
   ```bash
   fourmolu --mode inplace src test
   ```
4. **CI Compliance**: Ensure your changes pass the CI check, which fails on warnings for the latest GHC.
5. **File Operations**: Agents have standing permission to read, create, or modify any files within this repository as needed to fulfill their tasks. There is no need to ask for explicit permission for these operations. This permission is explicitly confirmed and should be treated as durable for this repo.

## Key Development Patterns

### Collapsing Logic

When modifying the tree (e.g., in `delete`, `filter`, `union`), use the `collapse` function to maintain invariants:

```haskell
collapse :: Bitmap -> SmallArray (Word64Map a) -> Word64Map a
collapse bm ary = case sizeofSmallArray ary of
  0 -> empty
  1 -> case indexSmallArray ary 0 of
    l@Leaf{} -> l
    _ -> Branch bm ary
  _ -> Branch bm ary
```

**CRITICAL**: Do NOT use `size` (which performs a full tree traversal) for collapsing logic. Use `sizeofSmallArray` and pattern matching on `Leaf`.

### Bit Manipulation

The trie uses 6 bits per level. The `index` function is central to navigating the tree:

```haskell
index :: Shift -> Word64 -> Bitmap -> Index
```

## Testing and Verification

### Test Suite

- Located in `test/Main.hs`.
- Uses `QuickCheck` to compare `Word64Map` against `Data.Map.Strict`.
- **Helper**: Use `fromKList :: [(K, a)] -> Word64Map a` to construct maps from lists of `(K, a)` pairs in tests. `K` is a newtype for `Word64` used to ensure proper test distribution.

### Invariant Checking

Always run `valid` on any generated or modified map in tests to ensure all invariants are preserved.

### CI Environment

- CI is configured to fail on compiler warnings for the latest GHC version (currently 9.14) using `-Werror`.
- Ensure all top-level bindings have type signatures and there are no unused imports or variables.

## Common Pitfalls

- **`Leaf` handling in `differenceWith`**: Ensure the function `f` is applied when keys match, even if the second map is a `Leaf`.
- **Recursion**: Always ensure `Shift` is incremented by 6 when descending into subtrees to avoid infinite loops.

## Simplifier Output (Core) Analysis

To optimize performance-critical functions like `insert`, it is helpful to examine GHC's simplifier output (Core).

### Generating Core Dumps

1.  **Configure Cabal**: Use the `cabal.project.local.dump-code` file to enable dumps.
    ```bash
    cp cabal.project.local.dump-code cabal.project.local
    ```
2.  **Build**: Run `cabal build` to generate the dumps.
    ```bash
    cabal build
    ```
3.  **Locate Dumps**: Core dumps (ending in `.dump-simpl`) are placed in the build directory.
    ```bash
    find dist-newstyle -name "*.dump-simpl"
    ```

### PR Review Retrieval

When a task depends on PR review comments, the fastest path is:

1.  **Ensure `gh` auth**: The CLI must be authenticated (`gh auth login` or `GH_TOKEN`).
2.  **Fetch review comments**: `gh pr view` does not expose line-level review comments.
    ```bash
    gh api repos/<owner>/<repo>/pulls/<pr-number>/comments
    ```
3.  **Fetch review summaries**:
    ```bash
    gh pr view <pr-number> --comments --json reviews,comments,files
    ```

Note: `cabal.project.local` is expected to be untracked when enabling Core dumps.

### Useful Commands for Analysis

- **Search for worker functions**: GHC often creates workers (e.g., `$winsert`) with unboxed arguments.
  ```bash
  grep -n "^\$winsert" path/to/Map.dump-simpl
  ```
- **Inspect strictness and unboxing**: Look at the `Str=` and `Arity=` signatures in the dump.
- **Read specific sections**: Use `sed` to extract lines around a match.
  ```bash
  sed -n '5000,5100p' path/to/Map.dump-simpl
  ```

### Performance Tips
- **Boxing**: Check if arguments like `Shift` (Int) are being re-boxed in recursive calls (e.g., `(I# (+# i# 6#))`). Use bang patterns `!s` to encourage unboxing.
- **Inlining**: Ensure small helper functions are marked `INLINE` or `INLINABLE`.
