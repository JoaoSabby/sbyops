# Sabby / sbyops Vision and Immediate Implementation Plan

## 1. Mission

Sabby is an R ecosystem for tabular data processing with aggressive performance,
low memory usage, persistent auditability in future stages, and an elegant,
intuitive pipeline grammar.

The first implementation step is intentionally smaller than the full vision:
`sbyops` will contain only the high-performance column-removal functions needed
now:

- `sby_select_modal_frequency()`
- `sby_select_correlation()`

This first version will not include reports, dashboards, persistent audit, or a
separate `sbyfeature` package. The goal is to prove that the core user
experience can be fast, automatic, memory-aware, and pleasant to use.

## 2. Non-negotiable principles

1. Use R as the public grammar and orchestration layer.
2. Do not use Python anywhere in the ecosystem.
3. Use international English for public function names, documentation, messages,
   and package text.
4. Keep public functions simple and intuitive.
5. Do not expose routine engine, memory, or thread choices in the common API.
6. Make performance decisions automatically by default.
7. Avoid unnecessary copies and dense materialization.
8. Do not densify sparse data accidentally.
9. Move heavy loops to native code.
10. Prefer immediate execution for the first version.
11. Keep this first implementation focused on the two public selection functions.
    Global configuration helpers are intentionally deferred.

## 3. Naming conventions

The prefix is always `sby`, because `sby` represents Sabby.

The typo `spy` should never be used.

Public functions use:

```r
sby_<verb>_<operation>()
```

Internal helpers use:

```r
sby_int_<name>()
```

Recommended verbs:

| Verb | Meaning | Example |
| --- | --- | --- |
| `select` | remove/select columns | `sby_select_modal_frequency()` |
| `mutate` | create or transform columns | `sby_mutate_zscore()` |
| `filter` | remove rows | `sby_filter_outlier()` |
| `stats` | compute and store statistics | `sby_stats_descriptive()` |
| `audit` | inspect or render audit information | `sby_audit_information()` |

For the current implementation, only `select` functions are in scope.

## 4. Immediate user experience

The current target user experience is:

```r
preprocessed_database <- original_data_base |>
  sby_select_modal_frequency(threshold = 0.95) |>
  sby_select_correlation(threshold = 0.95)
```

With explicit column selection:

```r
preprocessed_database <- original_data_base |>
  sby_select_modal_frequency(where(is.numeric), threshold = 0.95) |>
  sby_select_correlation(where(is.numeric), threshold = 0.95)
```

Rules:

- If no columns are provided, the function selects the applicable columns
  automatically.
- Columns not selected for evaluation are kept in the data.
- The return value is the data with columns removed.
- The common API should not require `engine`, `memory_budget`, or `n_threads`.

## 5. Configuration scope

This first implementation will not include public configuration helpers.

The only public functions in this phase are:

```r
sby_select_modal_frequency()
sby_select_correlation()
```

Advanced configuration helpers such as `sby_set_config()`, `sby_get_config()`,
and `sby_reset_config()` are deferred until after the two selection functions are
stable. Internal heuristics may still be used to protect memory and choose safe
execution paths, but those details are not part of the public API yet.

## 6. Error handling policy

The first version should not stop unnecessarily for recoverable data problems.

### Recoverable column-level issues

Examples:

- non-numeric column evaluated by correlation;
- all-missing column;
- zero-variance column;
- column with non-finite values;
- unsupported column type for modal-frequency calculation.

Default behavior with `error_policy = "continue"`:

- continue processing;
- skip or keep the problematic column according to the operation;
- emit a compact message only if needed;
- do not crash the full pipeline.

With `error_policy = "warn"`:

- continue processing;
- emit warnings.

With `error_policy = "stop"`:

- fail fast on recoverable problems.

### Fatal issues

These should always stop:

- invalid input object;
- native engine returning invalid results;
- memory allocation failure;
- corrupted internal metadata;
- impossible configuration state.

## 7. Immediate implementation: `sby_select_modal_frequency()`

### Public signature

```r
sby_select_modal_frequency <- function(.data, ..., threshold)
```

No public `engine`, `memory_budget`, or `n_threads` arguments.

### Behavior

The function removes columns where the modal value frequency is greater than or
equal to `threshold`.

For each evaluated column:

```text
modal_ratio = modal_count / number_of_rows
remove if modal_ratio >= threshold
```

If columns are passed through `...`, only those columns are evaluated. Columns
outside the selection are kept.

If no columns are passed, all supported columns are evaluated.

### Dense implementation

The initial dense implementation should use the existing R + Fortran strategy:

1. R validates and selects columns.
2. R encodes supported columns into compact integer codes.
3. Fortran receives integer codes and computes modal count and ratio.
4. R removes columns whose ratio is above the threshold.
5. The resulting data is returned.

The native loop should be parallelized by column with OpenMP.

### Sparse direction

Sparse support can come after the dense version. For sparse matrices, zero is the
likely modal value and should be counted through structural zeros instead of
materializing the full dense matrix.

For a sparse column:

```text
zero_count = number_of_rows - explicit_non_zero_count
```

The sparse engine should compare `zero_count` against frequencies of explicit
values.

This is not required in the first implementation unless sparse input is already
part of the immediate target dataset.

## 8. Immediate implementation: `sby_select_correlation()`

### Public signature

```r
sby_select_correlation <- function(.data, ..., threshold)
```

The first version should use Pearson correlation only. A future version can add
`method`, but the first API can stay minimal.

No public `engine`, `memory_budget`, or `n_threads` arguments.

### Behavior

The function removes highly correlated columns.

For evaluated numeric columns:

```text
remove a column when abs(correlation) >= threshold
```

When a highly correlated pair is found, remove the column with the higher mean
absolute correlation against the still-active candidate columns. This mirrors the
current proposed strategy and is easy to explain.

Columns not evaluated are kept.

### Automatic missing-value behavior

The first implementation uses automatic pairwise finite handling. Meaning:

- compute each pair using rows finite in both columns;
- do not stop the full pipeline because a selected column has NA;
- skip zero-variance or unusable pairs internally.

### Dense memory-safe implementation

The function should automatically choose one of two internal strategies:

1. **dense matrix strategy** for moderate numbers of columns;
2. **dense streaming/block strategy** for large numbers of columns.

The user should not choose the strategy.

Internal logic:

```text
estimate full correlation matrix memory = 8 * p * p bytes
if safe under internal memory policy:
    use dense matrix Fortran engine
else:
    use dense streaming/block Fortran engine
```

The memory estimate and chosen strategy are internal details.

### Dense matrix engine

For moderate `p`:

1. Convert selected numeric columns to a numeric matrix.
2. Call a Fortran/OpenMP engine.
3. Compute absolute Pearson correlations.
4. Return the correlation matrix or enough information for R to decide removals.
5. Remove columns.

### Dense streaming/block engine

For large `p`:

1. Do not allocate a full `p x p` matrix.
2. Process column pairs in blocks.
3. Keep only candidate pairs above threshold or directly update removal
   decisions.
4. Return only removed columns and minimal internal information.

This is the key engine for low memory usage.

### Sparse direction

Future sparse engine:

1. Detect sparse input.
2. Use sparse column pointers and row indices.
3. Precompute per-column sums and sum of squares.
4. Compute pairwise `sum_xy` only on intersections of non-zero row indices.
5. Avoid creating a dense correlation matrix.
6. Prefer streaming threshold logic.

## 9. Automatic parallelism

The user should not be forced to pass thread counts to each function.

Initial behavior:

- `threads = "auto"` in config;
- internal helper chooses a safe thread count;
- prefer physical cores over logical hyperthreads for memory-bound operations;
- avoid oversubscription with BLAS/MKL/OpenMP libraries.

Future helper functions may add global thread configuration and benchmarking. The first implementation relies on OpenMP runtime defaults and internal safeguards.

## 10. What is intentionally out of scope for the first version

The following are part of the Sabby vision, but not part of the immediate
implementation:

- reports;
- dashboards;
- persistent audit;
- DuckDB-backed audit store;
- Arrow-backed artifact store;
- lazy execution;
- `collect()`;
- `sbyfeature` package split;
- sparse engine unless immediately required;
- KNN imputation;
- z-score mutation;
- dummy encoding;
- holiday features;
- outlier mutation/filtering.

The first version should be excellent at removing columns by modal frequency and
correlation.

## 11. GitHub setup for the ecosystem

### Current step

Use the existing `sbyops` repository for the first implementation.

For now, `sbyops` contains:

- global configuration;
- internal helpers;
- native engines;
- `sby_select_modal_frequency()`;
- `sby_select_correlation()`.

### Recommended future repositories

When the first two functions are stable, create:

```text
sbyfeature
sabby
```

Later:

```text
sbymissing
sbyoutlier
sbystats
sbyaudit
sbyinstance
```

### Suggested GitHub organization

Create a GitHub organization if possible:

```text
sabby-r
```

or another available Sabby-related organization name.

### Initial repositories

1. `sbyops`
2. `sbyfeature`
3. `sabby`

### Recommended GitHub issues for `sbyops`

- Create automatic thread selection helper.
- Create modal-frequency dense Fortran engine.
- Create correlation dense matrix Fortran engine.
- Create correlation dense streaming Fortran engine.
- Add memory-safe internal strategy selection.
- Add robust recoverable error handling.

### Recommended GitHub issues for future `sbyfeature`

- Move `sby_select_modal_frequency()` from `sbyops` to `sbyfeature`.
- Move `sby_select_correlation()` from `sbyops` to `sbyfeature`.
- Keep low-level native utilities in `sbyops`.
- Add `sbyops` as dependency.
- Add package-level documentation.
- Add feature-selection benchmarks.

## 12. Working with Codex across packages

### Phase 1: single repository

Work only in `sbyops`.

Ask Codex to implement one tightly scoped change at a time:

1. config helpers;
2. modal-frequency selection;
3. correlation selection;
4. automatic thread selection;
5. memory-safe correlation strategy.

### Phase 2: split into `sbyfeature`

When ready, create the `sbyfeature` repository on GitHub and clone it locally.

Then run Codex separately in each repository:

1. one task in `sbyops` to expose stable low-level helpers;
2. one task in `sbyfeature` to implement public feature-selection functions;
3. one task in `sbyops` to remove public feature-selection functions after the
   migration is complete.

Coordinated multi-repository changes should be done as a planned sequence of
small PRs, not one huge change.

## 13. Future prompt: move feature functions from `sbyops` to `sbyfeature`

Use this prompt after the first `sbyops` implementation is stable and the
`sbyfeature` repository exists.

```text
We are splitting the Sabby ecosystem into `sbyops` and `sbyfeature`.

Context:
- `sbyops` is the core package. It should own configuration, metadata, native
  engine registration, low-level helpers, thread selection, memory-safe strategy
  helpers, and shared native kernels.
- `sbyfeature` is the feature-selection package. It should own public
  feature-selection verbs such as `sby_select_modal_frequency()` and
  `sby_select_correlation()`.
- Do not use Python anywhere.
- Public names and documentation must be in international English.
- Public functions should stay simple. Do not expose `engine`, `memory_budget`,
  or per-call thread controls in the initial user-facing API.
- Internal helpers use the `sby_int_` prefix.

Task:
1. In `sbyops`, identify the public feature-selection functions and the
   lower-level helpers they depend on.
2. Keep low-level config, native engine, thread, memory, and validation helpers
   in `sbyops` when they are generally useful.
3. Move public feature-selection functions to the new `sbyfeature` package:
   - `sby_select_modal_frequency()`
   - `sby_select_correlation()`
4. Make `sbyfeature` depend on/import `sbyops`.
5. Update `sbyfeature` DESCRIPTION, NAMESPACE, documentation, tests, and examples.
6. Remove public feature-selection exports from `sbyops` after migration, unless
   a temporary development re-export is explicitly necessary.
7. Add tests proving that:
   - `sby_select_modal_frequency()` removes modal-frequency columns;
   - `sby_select_correlation()` removes highly correlated columns;
   - selected columns only are evaluated;
   - non-selected columns are kept;
   - recoverable column problems do not stop normal pipelines unnecessarily;
   - no user-facing engine or memory-budget arguments exist.
8. Update roadmap documentation in both repositories.
9. Run package checks for both repositories where available.
10. Commit changes and create PRs for each repository.
```

## 14. First implementation acceptance criteria

The first implementation in `sbyops` is successful when this works:

```r
preprocessed_database <- original_data_base |>
  sby_select_modal_frequency(threshold = 0.95) |>
  sby_select_correlation(threshold = 0.95)
```

And:

1. no user-facing `engine` argument exists;
2. no user-facing `memory_budget` argument exists;
3. no user-facing per-call `n_threads` argument is required;
4. both functions remove columns and return data;
5. columns not selected for evaluation are kept;
6. correlation handles missing values automatically with pairwise finite observations;
7. recoverable issues do not stop normal pipelines unnecessarily;
8. heavy work is done in native code where practical;
9. correlation avoids unsafe full-matrix allocation when necessary;
10. tests cover the main removal behavior.
