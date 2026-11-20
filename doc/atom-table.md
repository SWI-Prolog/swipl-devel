# Compact atoms with flag bits

Status: proposal, not implemented.

## Goals

1. Make `struct atom` smaller.  Atoms take a large part of the memory of
   many applications.
2. Make room for per-atom flags, so that the reader and writer can
   decide cheaply:
   - that an atom is not an operator (in any module), and
   - that an atom can be written without quotes.

PR #1588 achieves part of (2) for operators with a fixed 4096-bit filter
indexed by `indexAtom(name) % 4096` (see "Interim: the operator filter").
This document describes a per-atom alternative that comes with a memory
saving.

## Current layout

`src/pl-incl.h`:

```c
struct atom
{ Atom		next;		/* next in chain */
  atom_t	atom;		/* as appearing on the global stack */
  unsigned int  hash_value;	/* hash-key value */
  unsigned int	references;	/* reference-count */
  union
  { struct PL_blob_t * type;	/* blob-extension */
    uintptr_t	next_invalid;	/* next invalidated atom */
  };
  size_t	length;		/* length of the atom */
  char *	name;		/* name associated with atom */
};
```

| Field          | 64-bit | 32-bit (WASM) |
|----------------|-------:|--------------:|
| `next`         |      8 |             4 |
| `atom`         |      8 |             4 |
| `hash_value`   |      4 |             4 |
| `references`   |      4 |             4 |
| `type`         |      8 |             4 |
| `length`       |      8 |             4 |
| `name`         |      8 |             4 |
| **total**      | **48** |        **28** |

The text is allocated separately (`PL_malloc()`), so a short atom costs
about 48 + 24..32 bytes on 64-bit.  Saving 8 bytes is roughly 10% of the
memory used by atoms.

Atoms live in `GD->atoms.array`: block *k* holds the atoms with index
in [2^k, 2^(k+1)), and `fetchAtomArray(i)` is
`&blocks[MSB(i)][i]` (the block pointers are offset so that they can be
indexed with the full index).  Index 0 is never used
(`GD->atoms.highest` starts at 1).

The hash table (`GD->atoms.table`) is an array of `Atom` bucket heads.
Only atoms whose type has `PL_BLOB_UNIQUE` are in the table; other blobs
leave `next` unused.  Lookups are lock-free: a thread publishes the table
and the bucket it walks as hazard pointers (`acquire_atom_table()`,
`acquire_atom_bucket()`), new atoms are pushed onto a bucket with a CAS
on the head (`lookupBlobEx()`), and AGC unlinks atoms (`unlinkAtom()`)
while holding `L_REHASH_ATOMS`, skipping buckets that are in use
(`atomBucketInUse()`).

## Where the bits cannot come from

- **`hash_value`.**  `term_hash/2` feeds `hash_value` directly into its
  hash (`termHashValue()` in `pl-prims.c`; also `pl-termhash.c`), and
  the manual guarantees that these hashes are constant over invocations,
  versions and platforms.  Taking bits changes every atom's hash.
- **`references`.**  The top 6 bits hold the atom state
  (`ATOM_STATE_MASK`); 26 bits remain for the count.  Taking more lowers
  the reference count limit of heavily used atoms.
- **`length`.**  Taking the top bits works on 64-bit (lengths never
  approach 2^48), but on 32-bit it leaves too little for long atoms.
  This is the fallback if the change below is not made.

## Why `atom->atom` cannot simply be dropped

`atom->atom` is always `MK_ATOM(index)`, so it is redundant given the
index.  But the index cannot be computed from an `Atom` pointer: the
blocks are separately allocated, so mapping a pointer back to its index
requires a search over the blocks.  The hot path needs exactly that:
`lookupBlobEx()` walks a hash chain of pointers and returns `a->atom`.

## Proposal: link the hash chains by index

Replace the `next` pointer by a 32-bit atom index and use the freed half
word for flags:

```c
struct atom
{ uint32_t	next;		/* index of next in chain; 0: end */
  uint32_t	flags;		/* ATOM_F_* */
  unsigned int  hash_value;	/* hash-key value */
  unsigned int	references;	/* reference-count */
  union
  { struct PL_blob_t * type;	/* blob-extension */
    uintptr_t	next_invalid;	/* next invalidated atom */
  };
  size_t	length;		/* length of the atom */
  char *	name;		/* name associated with atom */
};
```

This is 40 bytes on 64-bit (from 48), and 28 bytes on 32-bit (unchanged
size, but with 32 flag bits).  The bucket table becomes an array of
`uint32_t`, halving its size.

### Consequences

- **Lookup.**  Chain walks follow `fetchAtomArray(a->next)` instead of
  `a->next`.  The index of each atom on the chain is known, so the result
  is `MK_ATOM(index)`.  Each step costs an `MSB()` and one extra load;
  chains are short, but this must be measured (see below).
- **Insertion.**  The CAS on the bucket head becomes a 32-bit CAS
  (`COMPARE_AND_SWAP_UINT32`).  ABA behaviour is unchanged: the head
  check `head == table[v]` compares indices instead of pointers, and an
  index is only reused after AGC destroyed the atom, which already
  requires that no thread is walking that bucket.
- **Hazard pointers.**  `access.atom_bucket` publishes a `uint32_t *`
  instead of an `Atom *`.  `atomBucketInUse()` compares the same way.
- **Unlinking and rehashing.**  `unlinkAtom()`, `rehashAtoms()`,
  `resetListAtoms()`, `registerBuiltinAtoms()` and the debug walkers
  (`findAtomSelf()`, `pl_atom_hashstat()`) switch to indices.  Their
  logic does not change.
- **Limit.**  At most 2^32 - 1 live atoms (including blobs).  That
  requires at least 160 GB of atom structures, and AGC reuses indices.
  `reserveAtom()` and the local-chunk allocator must raise a resource
  error rather than hand out an index >= 2^32.
- **No more `a->atom`.**  About 40 uses, all in `pl-atom.c` except
  `PL_atom_from_index()` (`pl-fli.c`, becomes `MK_ATOM(i)`) and the
  blob save call in `pl-qlf.c` (which has the `atom_t` at hand).  They
  fall into three groups:
  - loops over the atom array (`collectAtoms()`, `lockAtoms()`,
    blob scans, `current_blob/2`): the loop index is known;
  - lookups: the index is known from the chain walk;
  - helpers that take an `Atom` and need the handle
    (`bump_local_references()`, `bump_scanned_references()`,
    `preDestroyAtom()` calling `release()` or the AGC hook,
    `dbgAtomName()`, the alternatives struct in completion): pass the
    `atom_t` (or the index) instead of, or next to, the `Atom`.
  - `atom->atom != ATOM_garbage_collected` becomes an index or pointer
    comparison.
- **Packages** do not see `struct atom`; the public API is unaffected.

## The flags

```c
#define ATOM_F_OPERATOR	0x1	/* is or was an operator in some module */
#define ATOM_F_QCHECKED	0x2	/* ATOM_F_PLAIN is valid */
#define ATOM_F_PLAIN	0x4	/* never needs quotes */
```

Flags are set with an atomic OR.  They must be zeroed whenever an atom
slot is (re)used: in `reserveAtom()`, `reserveLocalAtom()` and in
`registerBuiltinAtoms()`.

### Operator flag

- Set in `defOperator()` when the name is added to a module's operator
  table, under `L_OP` as today.  Never cleared while the system runs: a
  set flag means "may be an operator", and the full lookup decides.
  This is the meaning of the PR #1588 filter, but exact per atom, so
  there are no false positives from collisions.
- `currentOperator()` and `priorityOperator()` return early if the flag
  is clear.  `initOperators()` already declares all system operators
  through `defOperator()`, so they get the flag.
- No lazy initialisation is needed.
- `PL_cleanup()` destroys the atom table, so the flags disappear with it.

### Quote flags

Whether an atom needs quotes in general depends on context
(`atomType()` and `unquoted_text()` in `pl-write.c`):

- the module's `var_prefix` (only makes *more* atoms unquoted),
- the `dot_in_atom` flag and `PL_WRT_NODOTINATOM`,
- the stream encoding (`Scanrepresent()`) and `PL_WRT_QUOTE_NON_ASCII`,
- options for symbol and solo characters
  (`PL_WRT_BACKQUOTE_IS_SYMBOL`, `PL_WRT_PATTERN_SYNTAX_SOLO`).

The one class that is independent of all of these is:

> a non-empty text atom whose first character is an ASCII lowercase
> letter, followed only by ASCII letters, digits and `_`.

Every encoding represents ASCII, `var_prefix` only adds unquoted atoms,
and there is no `.`.  This is also by far the most common class.  So:

- On the first check, the writer computes whether the atom is in this
  class and sets `ATOM_F_QCHECKED`, plus `ATOM_F_PLAIN` if it is.  A
  race between two writers sets the same bits.
- If `ATOM_F_PLAIN` is set, `atomType()` returns `AT_LOWER` without
  scanning.  Otherwise it runs the existing code.  Symbol-char atoms
  (`+`, `=..`) are not cached: they depend on options and on `/*`.
- `resetListAtoms()` changes the name of `'[|]'`: clear its quote flags.
- `unquoted_atom()` (used outside the writer) can use the same test.

## Expected gains

Profile of `writeq/2` on 90,000 facts with short atoms (callgrind,
instruction counts, master of 2026-11-20):

| Part of `writeq/2`                             | Share |
|------------------------------------------------|------:|
| `Sputcode()` and other stream output           |  ~29% |
| `atomType()`, the quote check                  |  7.6% |
| `currentOperator()` / `priorityOperator()`     | ~2-3% |

The quote flags save at most the `atomType()` share, more for longer
atoms.  The operator flag helps the reader most: it asks
`currentOperator()` up to three times per name token.  PR #1588 reports
`read_clause/3` 0.89 -> 0.79 sec on 200,000 facts and 4% for `writeq/1`.

Memory: 8 bytes per atom on 64-bit, plus half of the bucket table.

## Interim: the operator filter (PR #1588)

Merged (PR #1588).  It keeps a static 4096-bit set `op_names` in
`pl-op.c`, cleared by `cleanupOperators()` from `PL_cleanup()`.

- It is never wrong, only slower: each operator name sends 1/4096 of
  all atoms through the full lookup.  About 2.5% with the system
  operators, about 22% with 1,000 user operators.
- When this proposal is implemented, the filter is replaced by
  `ATOM_F_OPERATOR`.

## Plan

1. Index chains, no flags yet.  Measure:
   - atom lookup throughput (e.g., `atom_codes/2` on new and existing
     atoms, `read_term/2` on a large file), single and multi-threaded;
   - memory per atom (`statistics(atom_space, _)`, RSS with 10M atoms).
   Accept only if lookup is not measurably slower.
2. `ATOM_F_OPERATOR`, replacing the PR #1588 filter.
3. Quote flags.  Measure `writeq/2` and `print_message/2` heavy code.

## Testing

- The existing suites, in particular `tests/GC/` (AGC, local blobs)
  and `tests/thread/`, under `build.asan` and in a
  single-threaded build.
- A stress test that creates and collects atoms from several threads
  while others look them up, to exercise the CAS, rehashing and
  `atomBucketInUse()` with index chains.
- Writer tests for the quote cache: the same atom written with
  `var_prefix` on and off, with `dot_in_atom`, to an ASCII stream, and
  after `resetListAtoms()`.
- 32-bit build (WASM) for the 28-byte layout.
