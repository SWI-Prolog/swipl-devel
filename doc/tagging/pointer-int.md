# Pointer integers

Status: **done** (4a0c45d40).

A _pointer integer_ holds a C pointer that `PL_put_pointer()` or
`PL_unify_pointer()` hands to Prolog.  It is an integer for Prolog, but
never identical to a plain integer, so Prolog code cannot create one.
This was the `T_PTRINT` type of [top-tags.md](top-tags.md); with the
[kind byte](kind-byte.md) it is a variant kind.

## Representation

`TAGEX_PTRINT` is `TAG_INTEGER|STG_INLINE|VARIANT_BIT`: a small integer
with the variant bit (`consPtrInt()`).  `isTaggedInt()` ignores the
variant bit, so all code that reads a small integer (arithmetic,
comparison by value, write/1, term_hash/2, `PL_get_int64()`) handles a
pointer integer without change, and the test costs the same (a mask of
5 instead of 6 bits).  `isPtrInt()` tests the exact kind.

## Semantics

  - integer/1, number/1 and atomic/1 succeed; arithmetic sees the address
    and produces plain integers.
  - Unification with a plain integer of the same value fails, as the
    cells differ.  compare/3 orders a pointer integer directly after
    the plain integer with the same value; the extra test only runs if
    the values are equal.
  - Clause indexing uses the cell as key, so it agrees.
  - Clauses keep the type (`H_PTRINT`, `B_PTRINT`, operand the address),
    as do compiled tries (`T_PTRINT`, `T_TRY_PTRINT`), records
    (`PL_TYPE_PTRINT`), findall/3, copy_term/2, global variables, tables
    and GC.
  - Reading, external records (fast_term_serialized/2,
    `PL_record_external()`) and `.qlf` files produce plain integers.
  - `PL_get_pointer()` only accepts pointer integers, which closes the
    path to crash the system by passing a computed integer to a
    predicate that expects a pointer.  A pointer must fit in a small integer;
    otherwise `PL_put_pointer()` and `PL_unify_pointer()` raise a
    representation error.  On mainstream systems user-space pointers
    are below 2^48 (larger address spaces are opt-in through `mmap()`
    hints).  The exception is 64-bit Android, which tags heap pointers
    in the top byte (0xB4, or per allocation with MTE).  There
    `O_BIG_ADDRESS` is defined (pl-data.h; it can also be set with
    `-DO_BIG_ADDRESS=1`): `PL_put_pointer()` makes a plain big integer
    of such a pointer, `PL_get_pointer()` and is_address/1 accept
    integers that are not small, and tabling delay lists hold the
    answer node as a big integer.  Tests that tell a pointer from a
    plain integer are skipped when pointers are big
    (`condition(small_address)`).  Verified on Linux with a temporary
    patch that sets the top byte to 0xB4 in `pointerToInt()` and
    strips it in `intToPointer()`, built with `-DO_BIG_ADDRESS=1`:
    224/224 tests.  Foreign code that creates a pointer
    with `PL_unify_int64()` and reads it back with `PL_get_pointer()`
    breaks; the packages do not do that.

Tests: `tests/core_lang/test_ptrint.pl` (26 tests; is_address/1 and
`must_be(address, X)` included).  Each of these
mutations fails some test: no variant bit, compiling as a plain
integer, recording as a plain integer, compiling tries as a plain
integer, and comparing equal values as equal.

## Cost

Measured 2026-11-02 against `kind-byte` (7c7a5ef6e), pinned, both
builds fresh, core plus the packages ErgoAI needs.  None of the
benchmarks creates pointer integers.

  - Tabling benchmarks: 99.9% and 99.1% in total.  The small ones are
    1-6% slower in both runs (recognize 105%, pingpong 103-106%),
    but they execute fewer instructions (20krecognize -3.7%,
    10kpingpong -5.4%) and the per-function cycle changes go both ways
    (`S_STATIC` +49% with the same code and +8% instructions,
    `put_fastheap()` -23%): code placement, as the new VM instructions
    shift the code.
  - `bench/` 99.4% (geometric mean, 3 rounds); moded_path (tabling)
    106-110%, no other program differs by more than 4% in all rounds.
  - ErgoAI cyc150: A 28.8-28.9 s instead of 29.3-29.4 s, B 11.4 s
    instead of 11.4-11.5 s.

The strict `PL_get_pointer()` and the pointer answer nodes in delay
lists against the prototype: tabling benchmarks 98.8% and
99.6% in total, no benchmark beyond ±3% in both runs except the
access benchmarks accOneAtom, accOneComp and accVariants (94–96%).

## Users

Foreign code using `PL_put_pointer()`/`PL_unify_pointer()` (jpl, odbc,
semweb, sgml, cpp, ...) gets pointer integers without change; the
package tests pass with the strict `PL_get_pointer()`.  Inside the
core, the producers that handed pointers to Prolog as plain integers
are converted:

  - Tabling delay lists hold ground answers as `+(ATrie, Answer)`
    with the answer node as integer (`delay_to_data()`).  These reach
    trie_term/2, `'$tbl_force_truth_value'/3` and others, which use
    `PL_get_pointer_ex()`.  The tabling tests fail if the node is a
    plain integer (`abstract_completion`, `answer_discard`).
  - The cookie of `win_add_dll_directory/2`.

Pointers that only appear in debug output (`pointerToInt()` in
`Sdprintf()`) are unchanged.  Worklists and components are not
pointers but integer handles (see the tabling handles in
[kind-byte.md](kind-byte.md)); `<trie>` handles are blobs.
