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

1. **Canonical empty**: The empty map is always `Branch (BM 0) mempty`. Use the `null` function to check for emptiness.
2. **No redundant branches**: A `Branch` should never have exactly one child if that child is a `Leaf`. Such branches must be collapsed. Internal branches with one child are allowed if they eventually lead to multiple leaves or a different prefix structure.
3. **Bitmap consistency**: `popCount bm` must always equal `sizeofSmallArray ary`.
4. **Prefix consistency**: All keys in a subtree must share the same prefix bits above the current `Shift`.

## Development and Operations

### Building and Testing

Use standard `cabal` commands for development:

- **Build**: `cabal build` (use `--enable-tests` to include test targets).
- **Test**: `cabal test` or `cabal run amt-word64-test`.
- **Clean**: `cabal clean`.

### Development Workflow

1. **Feature Branches**: Never commit directly to `master`. Always work in a feature branch and prepare a PR.
2. **Formatting**: All Haskell code must be formatted using `fourmolu`. Run the following before committing:
   ```bash
   fourmolu --mode inplace src test
   ```
3. **CI Compliance**: Ensure your changes pass the CI check, which fails on warnings for the latest GHC.
4. **File Operations**: Agents have standing permission to read, create, or modify any files within this repository as needed to fulfill their tasks. There is no need to ask for explicit permission for these operations.

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
