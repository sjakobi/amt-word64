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
- **`Index`**: `Index !Bitmap !Int !SlotState`, where fields represent the
  singleton slot bit, compact child-array index, and occupancy state.
- **`SlotState`**: `SlotEmpty | SlotOccupied`.
- **`Shift`**: An `Int` representing the bit position (multiples of 6) currently being inspected.

### Subkey and Slot Terminology

- A **subkey** is the 6-bit slice selected at one trie level:
  `((unsafeShiftR k (shiftToInt s)) .&. subkeyMask)`.
- A **slot** is the branch position addressed by that subkey.
- Subkey values and slot numbers are the same (`0..63`); docs may use
  `subkey/slot` when discussing both viewpoints together.

### Invariants

1. **Canonical empty**: The empty map is represented by a `Branch` with an empty bitmap.
2. **No redundant branches**: If a `Branch` has exactly one sub-node, that
   node must also be a `Branch`.
3. **Bitmap consistency**: `popCount bm` must always equal
   `sizeofSmallArray ary`.
4. **Prefix consistency**: For any node at `Shift` `s`, all keys in its
   subtree must share the same prefix for the bits more significant than `s`.
5. **Empty only at root**: The canonical empty node may only appear at the
   root. Internal nodes are never empty.

## Development and Operations

### Building and Testing

Use standard `cabal` commands for development:

Always run cabal with `CABAL_DIR=/tmp/cabal` to avoid writes in `$HOME`.
Example: `CABAL_DIR=/tmp/cabal cabal build`

- **Build**: `CABAL_DIR=/tmp/cabal cabal build` (use `--enable-tests` to include test targets).
- **Test**: `CABAL_DIR=/tmp/cabal cabal run tests -- --hide-successes`.
- **Clean**: `CABAL_DIR=/tmp/cabal cabal clean`.

### Development Workflow

1. **Feature Branches**: Never commit directly to `master`. Always work in a feature branch and prepare a PR. All changes shall be committed with an explanation and mention of the current model. For performance work, include evidence in the commit message (e.g., before/after snippets of generated code such as Core). Do not ask for permission before creating commits on feature branches.
   Ensure PRs are based on the latest `master` (fast-forward or rebase onto `origin/master` before creating).
   You do not need to ask again before creating a feature branch and committing to it.
2. **Git Operations**: You have standing permission to create commits on feature branches, switch branches, and use non-destructive git commands (e.g., `status`, `log`, `diff`, `add`, `stash`, `switch`). Do not use destructive commands like `reset --hard` unless explicitly asked.
3. **Formatting**: All Haskell code must be formatted using `fourmolu`. Run this after each change and before committing:
   ```bash
   fourmolu --mode inplace src test
   ```
4. **Testing**: Always run tests for code changes. Skip tests only if the changes are documentation-only, and say so explicitly.
5. **Committing**: Always commit your changes to a feature branch. Do not leave work uncommitted.
6. **CI Compliance**: Ensure your changes pass the CI check, which fails on warnings for the latest GHC.
7. **File Operations**: Agents have standing permission to read, create, or modify any files within this repository as needed to fulfill their tasks. There is no need to ask for explicit permission for these operations. This permission is explicitly confirmed and should be treated as durable for this repo.
8. **Command Permissions**: Do not ask before creating commits on feature branches. You have standing permission to run non-destructive git commands (`status`, `log`, `diff`, `add`, `stash`, `switch`, `fetch`, `push`, `rebase`, `worktree add/remove`). You may also run standard build/test commands (`cabal build`, `cabal run tests -- --hide-successes`, `cabal run`, `cabal clean`) and formatting (`fourmolu --mode inplace src test`) without asking. For network calls, `gh` is permitted for PR creation and review fetching, provided it does not perform destructive operations. Ask before running destructive commands like `reset --hard`, `checkout --`, or rewriting remote history unless explicitly requested.
9. **Commit Message Format**: Keep lines under 72 characters. Use a short subject line, then include an explanatory paragraph when the change is non-trivial or benefits from context; use judgement for small changes. In the footer, include a single `Model:` line with the explicit model string (e.g. `GPT-4`). When writing commits from the CLI, use multiple `-m` flags or a heredoc so newlines are real and not escaped `\\n`. Example:
   ```bash
   git commit -m "Subject line" \
     -m $'Explanation with a paragraph that spans\\nmultiple lines without literal \\\\n escapes.' \
     -m "Model: GPT-5"
   ```
   ```
   Subject line

   Explanation wrapped to 72 columns.

   Model: GPT-5
   ```
10. **Commit Author**: Use `Codex <codex@example.com>` for the commit
   author identity unless instructed otherwise.
11. **Core Dumps**: `cabal.project.local` is typically created from `cabal.project.local.dump-code` to enable Core/Code-Prep dumps. It is expected to remain untracked; do not add it to git unless explicitly requested.
12. **PR Reviews**: Never mark review comments/threads as resolved; Simon will do this once satisfied.

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

The trie uses 6 bits per level. `index` is central to navigation and computes
both slot bit and compact child-array position:

```haskell
index :: Shift -> Word64 -> Bitmap -> Index
```

When the array position is only needed for occupied slots, use:

```haskell
indexIfSlotOccupied :: Shift -> Word64 -> Bitmap -> Maybe Int
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
2.  **Build**: Run `CABAL_DIR=/tmp/cabal cabal build` to generate the dumps.
    ```bash
    CABAL_DIR=/tmp/cabal cabal build
    ```
3.  **Locate Dumps**: Core dumps (ending in `.dump-simpl`) are placed in the build directory.
    ```bash
    find dist-newstyle -name "*.dump-simpl"
    ```
    Core-prep dumps live alongside them as `*.dump-prep` (e.g.
    `dist-newstyle/build/.../src/Amt/Word64/Map/Internal.dump-prep`).

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

Note: When posting comments with `gh pr comment -b`, wrap the body in single
quotes or use a quoted heredoc (`<<'EOF'`) to avoid shell interpretation of
backticks.

Note: `cabal.project.local` is expected to be untracked when enabling Core dumps.

### Useful Commands for Analysis

- **Search for worker functions**: GHC often creates workers (e.g., `$winsert`) with unboxed arguments.
  ```bash
  grep -n "^\$winsert" path/to/Internal.dump-simpl
  ```
- **Inspect strictness and unboxing**: Look at the `Str=` and `Arity=` signatures in the dump.
- **Read specific sections**: Use `sed` to extract lines around a match.
  ```bash
  sed -n '5000,5100p' path/to/Internal.dump-simpl
  ```

### Performance Tips
- **Boxing**: Check if arguments like `Shift` (Int) are being re-boxed in recursive calls (e.g., `(I# (+# i# 6#))`). Use bang patterns `!s` to encourage unboxing.
- **Inlining**: Ensure small helper functions are marked `INLINE` or `INLINABLE`.

## Recent Learnings / Helpers

- GitHub review threads: `reviewThreads` GraphQL nodes do not expose `createdAt`;
  use the first comment’s `createdAt` when ordering threads.
- Line-level replies can be posted with GraphQL
  `addPullRequestReviewThreadReply` (thread id + body), which avoids
  needing `commit_id/path/position` for REST comments.
- If GitHub returns `429 Too Many Requests`, it is often a secondary rate
  limit; reduce query frequency or batch multiple fields into one GraphQL
  call and check `gh api rate_limit`.
- QuickCheck performance: when properties over nested lists are slow,
  wrap inputs in a size-scaled newtype (e.g. `ShortList`) or use
  `scale`/`resize` to cap sizes.

- Use `lowBit` (`w .&. negate w`) and `clearLowBit` (`w .&. (w - 1)`) for bitmap scans; both are documented to return 0 on input 0. Every bit-walk (union/diff/intersect/filter/partition/etc.) should loop by clearing the low bit instead of recomputing `popCount`.
- Branch merging helper is `unionBranches`; analogous helpers are `differenceBranches` and `intersectBranch`. Prefer single-pass bitmap walks with mutable arrays and shrink before freeze.
- `filterWithKey` now has a direct ST-based builder to avoid `Maybe` allocation; follow that pattern instead of piping through `mapMaybeWithKey` when you need a no-allocation filter.
- Keep `Word64` key arguments strict (`!k`, `!k1`, `!k2`, etc.) across the module.
- When renaming local recursion helpers for Core readability, prefer short, unique names (e.g., `diff`, `inter`, `mapMb`, `partArr`) to keep dumps compact and search-friendly.
- Module layout: `Amt.Word64.Map.Internal` contains the implementation and exports internal types/constructors; `Amt.Word64.Map.Lazy` re-exports the public API; `Amt.Word64.Map` re-exports `Amt.Word64.Map.Lazy`. Tests import `Amt.Word64.Map.Lazy`, and can import `Amt.Word64.Map.Internal` qualified when needed.
- Before building a new PR on top of `master`, check what was just merged (e.g., `git log origin/master..branch`) to avoid cherry-picking already-merged commits and unnecessary conflicts.
- To find line-level review comments quickly, use `gh api repos/<owner>/<repo>/pulls/<pr>/comments`; review bodies can be empty, so rely on the comments API for actionable items.
- For PR replies, `gh pr comment <number> --body "<text>"` is the quick path; line-level replies require `commit_id`, `path`, and `position` and are harder to post ad hoc.
