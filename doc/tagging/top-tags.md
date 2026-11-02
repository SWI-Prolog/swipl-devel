# Moving the type tags to the top of the cell

Status: **rejected** (2026-11-02).  Step 3 was implemented as the
compile-time option `O_TOP_TAGS` and passed all tests, but it is slower
on both x86-64 (0.5-3%) and ARM64 (~5%), so it was removed again; the code is not in the history.  The
low-bit layout was cleaned up instead: see [kind-byte.md](kind-byte.md).
What the top-tag work found (layout dependencies, hash clustering,
packages that shift `atom_t`) is fixed there.

ARM64 (Apple M1, Apple clang, kind-byte before squashing (code as 7c7a5ef6e, with `O_TOP_TAGS` still available) against
origin/master 08abe65e9, fresh builds, 3 interleaved rounds of
`bench/ --speedup=4`): kind byte 0.995-1.003 of master, top tags
1.043-1.055 (geometric mean); top tags >4% slower in all rounds for 15
of 36 programs (det 1.29, qsort 1.20, eval 1.20, meta_qsort 1.19), only
tak faster (0.94).  nreverse: top tags 1.060 time, 1.087 instructions,
1.057 cycles of the kind byte.  Top Byte Ignore does not make up for
the extra instructions for top-byte constants and tag tests.  semweb
crashed with top tags because the M1 still had the semweb commit that
shifts `atom_t` (fixed in semweb ea4ee3c).

The rest of this note is the design as it was.

This note works out what it would take to replace the current low-bit
tagging (`src/pl-data.h`) with a layout that keeps the type in the top
byte of the 64-bit cell.  The goals are:

  - Make the type system explicit: one tag per kind of cell instead
    of tag + storage bits + header bits.
  - Free tags for new types, notably a _pointer integer_: an integer
    that can only be created from C, so Prolog code cannot hand an
    invalid pointer to the system.  Candidates are the answer pointers
    in tabling, the trie pointers in private tries and the many
    foreign libraries that use `PL_unify_pointer()`.
  - Allow using a pointer cell as an address without decoding it on
    hardware that ignores the top byte.


## The current layout

All cells are 64 bits, also on 32-bit hardware.  The low 7 bits are
reserved (`LMASK_BITS`):

```
 63                                           7 6 5 4 3 2 1 0
+----------------------------------------------+-+-+---+-----+
|                 value                        |F|M|stg| tag |
+----------------------------------------------+-+-+---+-----+
```

  - `tag` (3 bits): `VAR`, `ATTVAR`, `FLOAT`, `INTEGER`, `STRING`,
    `ATOM`, `COMPOUND`, `REFERENCE`.
  - `stg` (2 bits): `STATIC`/`INLINE`, `GLOBAL`, `LOCAL`, `RESERVED`.
  - `M`, `F`: the GC mark and first bits.
  - Pointers are stored as `p << 7` (`consPtr()`), and `valPtr()`
    shifts them back.

The storage bits date from the time when stack pointers were offsets
relative to the stack base, so the base had to be known.  Pointers are
now absolute and the storage bits have been reused for typing:

| tag\|stg               | meaning                                         |
|------------------------|-------------------------------------------------|
| `VAR\|STATIC`, 0       | unbound variable                                |
| `VAR\|STATIC`, ≠0      | GC: varref while sweeping (`consVar()` in `pl-gc.c`) |
| `VAR\|LOCAL`           | tries: `RESERVED_TRIE_VAL()`, key pops           |
| `VAR\|RESERVED`        | compiler/reader var info (`setVarInfo()`, `consVarInfo()`) |
| `ATTVAR\|GLOBAL`       | attributed variable                             |
| `ATTVAR\|STATIC/RESERVED` | tries: begin/end attvar keys                |
| `FLOAT\|GLOBAL`        | pointer to indirect float                       |
| `INTEGER\|INLINE`      | small integer (57 bits)                         |
| `INTEGER\|GLOBAL`      | pointer to indirect big integer *or* rational; `MP_RAT_MASK` in the size word decides |
| `STRING\|GLOBAL`       | pointer to indirect string                      |
| `xxx\|LOCAL`           | header (guard) of an indirect on the global stack; any cell with bit `STG_LOCAL` there is read as a header |
| `ATOM\|STATIC`         | atom                                            |
| `ATOM\|GLOBAL`         | functor (`functor_t`)                           |
| `COMPOUND\|GLOBAL`     | compound term                                   |
| `COMPOUND\|STATIC`     | `variant/2`: temporary marks (`TAG_COMPOUND_x`) |
| `REFERENCE\|GLOBAL`    | reference to a global-stack cell                |
| `REFERENCE\|LOCAL`     | GC/shift: reference to a local-stack cell (`makeRefLok()`) |
| `xxx\|GLOBAL` (index)  | clause/trie handle for an indirect (`pl-indirect.c:321`) |

Outside the stacks, private tries add `TRIE_PTR_TAG` (`0x7`) in node values
(`pl-trie.h:201`).

Problems:

  - Telling a small integer, a big integer and a rational apart takes
    the tag, the storage bits and, for rationals, a bit in the size
    word of the indirect.  `isInteger()` must read the header.
  - The free combinations that can live on the global stack are only
    `FLOAT|STATIC`, `STRING|STATIC` and `REFERENCE|STATIC`.  `LOCAL` or
    `RESERVED` cells are read as headers, and the GC follows `GLOBAL`
    cells as pointers.  Each free slot also clashes with a test that
    looks only at `tag()` (`isFloat()`, `isString()`, `deRef()`), so
    every such test would need an audit before the slot can be used.
  - `REFERENCE|LOCAL` versus `REFERENCE|GLOBAL` can be decided by comparing
    addresses: the local stack is always above the global stack, and
    `makeRefLG()` already does so.


## What the hardware allows

Cells holding a pointer could keep their tag in the top byte and be
used as an address directly where the hardware ignores those bits:

| platform        | top bits in a load/store address                  |
|-----------------|----------------------------------------------------|
| ARM64 Linux     | ignored (Top Byte Ignore, 8 bits).  System calls only accept tagged addresses after `prctl(PR_TAGGED_ADDR_ENABLE)`, so strip pointers handed to the OS. |
| ARM64 macOS     | ignored (TBI on for user-space data).  *To verify.* |
| x86-64          | must be canonical (copies of bit 47 or 56), else `#GP`.  Intel LAM / AMD UAI exist; Linux support is limited and Windows has none. |
| RISC-V          | only with the pointer-masking extension, enabled per process.  Rare. |
| 32-bit, wasm32  | the address is the low 32 bits; truncation is free. |
| wasm64          | must mask: the bounds check sees the top bits.      |

Stripping on x86-64 costs one `and` with a mask held in a register,
which costs about the same as the current `shr 7`.  So **top tags cost
nothing on any platform and remove one instruction from every
pointer decode on ARM64 and 32-bit**.  On `deRef()` that is one cycle
on a dependency chain whose load-to-use latency is about 4 cycles.

Only the top byte is used for the tag, so pointers must fit in 56 bits.
That is true for all current user-space configurations, including x86
LA57 (57-bit virtual addresses, user half below 2^56) and ARM64 52-bit
virtual addresses.


## Proposed layout

```
 63      56 55                                       3 2 1 0
+---------+------------------------------------------+-+-+-+
|   tag   |              pointer / index             |0|F|M|   tagged cell
+---------+------------------------------------------+-+-+-+
|  sign + value (see below)                          |1|F|M|   small int
+----------------------------------------------------+-+-+-+
```

  - Bits 0–1: GC mark and first bits, as now.  We need them.
  - Bit 2 set: small integer.  Pointers are 8-byte aligned, so bit 2 is
    0 in every pointer cell.  `isTaggedInt(w)` is `w&4` and
    `valInt(w)` is `(sword)w>>3`.
  - Otherwise the top byte is the type.  A variable stays `(word)0`.
    Pointer cells hold the absolute address in bits 0–55.  Atoms and
    functors hold their index in bits 3–55.

### Small integers: two options

  (A) **61-bit integers.**  Any value in bits 3–63.  The top byte of a
      small integer is then arbitrary, so every type test must first
      check bit 2: `isRef(w)` becomes `!(w&4) && w>>56 == T_REF`.  On
      x86 that is a 64-bit mask (`0xff00000000000004`) in a register,
      or two tests.

  (B) **53-bit integers.**  Restrict small integers so that their top
      byte is `0x00` or `0xff` (bits 55–63 all equal).  No pointer type
      uses those two tags, so a type test is `w>>56 == T` (shift +
      compare with an 8-bit immediate).  Integers between 2^52 and the
      current 2^56 move to indirects.  Overflow is checked the same way
      `inTaggedNumRange()` works now.

`deRef()` and the unification/comparison dispatch test types on
arbitrary cells all the time, so (B) is the likely winner.  The cost
is the smaller range of small integers.  Time stamps in microseconds
(~2^51) still fit, while nanoseconds (~2^61) are indirect today
anyway.  Decide by measuring.

### Tags

With up to 254 tags each kind of cell gets its own tag.  The numbering
follows the standard order of terms, so differing types still compare
by tag:

| tag            | contents                                  |
|----------------|-------------------------------------------|
| `0x00`/`0xff`  | var (`0`), small int (bit 2), reserved     |
| `T_ATTVAR`     | pointer to attvar cell                    |
| `T_FLOAT`      | pointer to indirect double                |
| `T_MPZ`        | pointer to indirect big integer           |
| `T_MPQ`        | pointer to indirect rational              |
| `T_PTRINT`     | pointer integer (see below)               |
| `T_STRING`     | pointer to indirect string                |
| `T_ATOM`       | atom index                                |
| `T_COMPOUND`   | pointer to functor cell                   |
| `T_FUNCTOR`    | functor index (functor cell, `functor_t`) |
| `T_REF`        | pointer to cell (global, local during GC) |
| `T_HDR_*`      | headers of indirects, one per type        |
| `T_INTERNAL_*` | trie keys, compiler var info, GC varrefs, variant/copy-term marks |

Type classes such as `isInteger()`, `isNumber()` and `isAtomic()` become
a single bitmap test, `(CLASS_BM >> (w>>56)) & 1`, over a 256-bit set,
or a compare when the tags of a class are contiguous.  A rational no
longer needs its header read, so `MP_RAT_MASK` disappears.

Headers get a top nibble of their own (e.g. `0x4?`).  Small integers
under (B) have a top nibble of `0x0` or `0xf`, so the GC's downward scan
for headers becomes `(w&3) || (w>>60) == 4` instead of today's
single `w & (MARK|FIRST|STG_LOCAL)`.

Private markers (trie keys, compiler and reader var info, variant and
copy-term marks) each get an explicit `T_INTERNAL_*` tag instead of
borrowing storage bits.  That makes them greppable.


## The pointer integer

`T_PTRINT` holds a C pointer (address in bits 0–55).  It is an integer
to Prolog:

  - `integer/1` and `number/1` succeed, and arithmetic sees the
    address value.  Arithmetic results are always plain integers, so
    `X is P+0` cannot create a pointer.
  - `=/2` and `==/2` fail against a plain integer with the same value:
    the cells differ and the tags differ.  Indexing uses the cell as
    the key, so it agrees.
  - `compare/3` orders a pointer directly after the plain integer with
    the same value, never equal.  `=:=/2` compares values and succeeds.
  - `write/1` prints the number.  Reading it back yields a plain
    integer, which is safe.
  - Records keep the type.  `fast_write/2` and `.qlf` files either
    reject pointers or write them as plain integers.

A pointer integer cannot be forged, but it can still dangle: the GC
does not know what it points to.  It is safe only where the lifetime
of the target is managed elsewhere, as in tabling today.

Users:

  - Tabling: `delay_to_data()`/`word_to_answer()` (`pl-tabling.c`) and the
    other `pointerToInt()` sites: answer trie nodes, monotonic
    dependencies.  Worklists and components no longer need it: they
    use integer handles into a per-thread slot array (see
    [local blobs](local-blobs.md)), which also detect stale handles.
  - Foreign code using `PL_unify_pointer()`/`PL_get_pointer()`.
  - Private tries: `TRIE_PTR_TAG` node values become ordinary cells.  A
    trie can then be passed to Prolog without first creating its atom
    handle.

Alternatives considered for the tabling and trie pointers (2026-11-02):

  - `<trie,N>` handles for trie nodes, with a node array per trie.
    Private tries allocate their first nodes individually and then from
    chunks of growing size that are swept, and concurrent tries (all
    user tries and shared tables) allocate each node with several
    writers at once, so each insert would pay for an atomic index.
    Tabling manages the lifetime of the nodes it hands out, so a lookup
    per decode buys nothing there.  `trie_term/2` has no trie argument,
    so the handle of `trie_insert/4` would have to become a compound.
  - A global handle table for the nodes `trie_insert/4` returns only.
    Safe and cheap, but left for now.

Open decision: should `PL_get_pointer()` accept only pointer integers?
That closes a crash path from Prolog, but breaks foreign code that
rebuilds a pointer from a plain integer.  jpl, odbc, semweb and sgml
use `PL_unify_pointer()`/`PL_get_pointer()`.  The alternative is to use
the new type only inside the core.


## Effect on hot paths

  - `deRef()`: `w>>56 == T_REF` then load.  On ARM64 and 32-bit the cell
    is the address; on x86-64 add an `and`.
  - Unification: unchanged in structure.  `w1 == w2` first; different
    tags fail.  The `storage(w) == STG_INLINE` tests become `w&4`.
  - `compare_primitives()`: dispatch on small int first, then on tag.
    The inline integer fast path can compare the cells as signed words
    instead of two `valInt()` calls.
  - Arithmetic: `valInt()` stays one shift.  `consInt()` is `v<<3|4`.
  - Indexing (`indexOfWord()`): unchanged in structure.
  - `term_to_fastheap()` relocation: with the address in the low bits,
    relocating a pointer is `w + delta`, with no shift.
  - GC: the relocation chains save and restore the tag of the cell at
    the end of a chain.  That now saves the top byte instead of the
    storage bits.


## 32-bit systems

`atom_t`, `functor_t` and `code` are `uintptr_t`, so 32 bits there, while
cells are 64 bits.  An atom handle with its tag in bit 56 does not fit
in `atom_t`.  The conversions `word2atom()`, `atom2word()`, `code2word()`
etc. (`pl-incl.h:942`) already exist.  On 32-bit systems they would map
between a 32-bit handle and the tagged 64-bit cell instead of being
casts.  This also affects VM operands such as `H_ATOM` and
`H_SMALLINT`, which are `code`.


## Risks and costs

  - The encoding change itself.  After step 2 it is confined to
    `pl-data.h`, `consPtr()`/`valPtr()` in `pl-inline.h` and GC:
    `pl-gc.c` still uses `STG_*` as stack designators in the relocation
    chains and the trail tagging.  Code still switches on `tag()` with
    `TAG_*` labels and uses `TAGEX_*` labels for `tagex()`, so these must
    keep meaning a coarse type and an exact cell kind.  `type_map` in
    `pl-fli.c` and `tagtypeex[]` in `pl-setup.c` are indexed by tag.
  - External formats: check records, fastheap terms, `.qlf` files and
    saved states for raw tag values.  Packages that hash `atom_t>>7`
    (e.g. `packages/nlp/snowball.c`) keep working but hash on
    tag-free bits of a different width.
  - HWASan uses the top byte on ARM64, so ARM64 HWASan builds must
    strip pointers explicitly.
  - Floats stay indirect.  A double has no spare bits for the GC bits
    and a type marker.  Inline floats need the GC marks in a side
    bitmap, which is a separate project.


## Plan

 1. **Done** (2026-11-02, x86-64 only).  Measure first.
      - Tagged integers created at or above 2^52 (counted in
        `consInt()`): `bench/` 0.1% of 21.3M, the tabling benchmarks
        0.0008% of 605M, ErgoAI cyc150 20 of 61M (from `pl-gmp.c`).
        The tabling handles of `new_tbl_handle()` gave another 5.7M
        in cyc150 and are now limited to 52 bits.  So (B) moves almost
        nothing to indirects.
      - A synthetic loop (`deRef()` of 0–2 references, type dispatch,
        compare) on a randomly linked heap: memory-bound (4M roots)
        current 42–50 ns per pair, (A) 52–58 ns, (B) 43–51 ns.
        Cache-resident (4K roots, gcc) current 5.5–8.2 ns, (A) 2.3 ns,
        (B) 1.9 ns; clang (A) 2.2 ns, (B) 2.3–2.4 ns.  The loop is
        dominated by misses and branch prediction, so this ranks the
        encodings only: (A) is never better than (B) with gcc, and
        loses in the memory-bound run.  ARM64 is not measured.
      - Decision: (B).
 2. **Done** (d755bdc49).  Abstract: route every remaining raw use of
    `LMASK_BITS`, `<<7` and the storage bits through named macros in
    `pl-data.h`, without changing the encoding.  `pl-data.h` now has:
      - `TAGEX_*` names for all `tagex()` values, including the private
        markers (`TAGEX_VARINFO`, `TAGEX_TRIE_*`, `TAGEX_VARIANT_COMPOUND`,
        `TAGEX_VARREF`);
      - `consCompound()`, `consAttVar()`, `consIndirect()`;
        `consTagged()`/`valTagged()` for index cells;
        `consAddr()`/`valAddr()`/`addrDelta()` for address cells;
      - `isGlobalPtr()`, `isIndirectHdr()`/`INDHDR_BIT`, `isLocalPtr()`,
        `isLocalRef()`, `nonFunctorKey()` and `isInlineInt()`; the latter
        is the cheap tagged-or-indirect test for a known integer.

    Verified by comparing the objects of all core sources before and
    after, for release and `O_DEBUG` flags: identical except for
    `assert()` texts and a few rewrites shown to be equivalent.
 3. **Done**, then removed (see the status above).  Switch the encoding behind
    those macros.  `O_TOP_TAGS` selects the layout.  It needs 64-bit
    pointers and is off by default, as it is slower on x86-64 (below).
    The commits: a cleanup that hides the remaining raw tag bits and
    leaves the generated code unchanged (compared per object), the hash
    fix below, and the layout itself.
      - The top byte of a cell that is not a small integer is its
        `tagex()` value (0x00..0x3f since the kind byte, see
        [kind-byte.md](kind-byte.md)), so all `TAG_*`, `STG_*` and
        `TAGEX_*` values and the switches on them are unchanged.
        `tagex(w)` is `w&4 ? TAGEX_TAGGED_INT : w>>56`.  Tests for one
        coarse tag other than var and integer are
        `hasTag(w, t)`: `(w>>56 & 0xc7) == t`, as the mask maps the
        integer top bytes 0x00 and 0xff to 0x00 and 0xc7 (0xe7 before
        the variant bit).  `isAtom()`
        and `isFunctor()` compare the top byte.
      - `consInt(n)` keeps the low 53 bits of `n`, sign-extended, so no
        cell it builds has another top byte, and the existing
        `valInt(consInt(n)) == n` tests decide whether `n` fits.
      - The GC bits moved to bits 0 and 1.  Pointers are aligned and
        integer and index values start at bit 3, so the marking of term
        walks is unaffected.
      - Stack addresses must have a zero top byte; `stack_malloc()` and
        `stack_realloc()` check this (memory tagging on ARM64 would
        violate it).  Raw pointers, small numbers and generations
        (0x7f.., 0x80..0xff..) on the stacks then never pass
        `isAtom()`, which atom-GC uses to scan the local stack.
    Layout dependencies that step 2 missed:
      - GC: `ttag()` read the trail tag from bit 0, now the mark bit, and
        the downward scan tested the header bit, which negative small
        integers (top byte 0xff) also have
        (`isMarkedOrIndHdr()`).
      - The XR table of QLF files relied on bit 0 of atoms and functors
        to keep them apart from pointers, and trie values told records
        from atoms and integers by the low two bits.
      - Packages: semweb packed atoms into 32-bit ids and datums by
        shifting `atom_t`; it now uses `PL_atom_index()` and
        `PL_atom_from_index()`.  snowball hashed `atom_t>>7`.
    Cost, measured 2026-11-02 on x86-64 (AMD Ryzen AI Max+ 395, pinned,
    both builds core plus the packages ErgoAI needs, against the parent
    with the hash fix below applied to both):
      - The first measurement was much worse: the tabling benchmarks
        took 105–106% and 200pDoubleFirst had 32M instead of 7M branch
        mispredictions.  Branch sampling (`perf record -j any`) put them
        in the probe loop of the trie child index: its Fibonacci hash
        spreads consecutive keys only if they step by 1, and small
        integers now step by 8 instead of 128.  Both `child_hash()` and
        `hashIndex()` now shift out `CELL_VAL_SHIFT` first.
      - Then: tabling benchmarks 102.6% and 103.4% in total, ErgoAI
        cyc150 solves A in 29.4 s instead of 28.9–29.1 s and B in
        11.6 s instead of 11.3–11.4 s, `bench/` 100.4% (geometric
        mean; nreverse 108%, mu 107%, zebra and sieve 106%, divide10
        and times10 93%).
      - The extra cost is instructions, not misses: nreverse executes
        5.7% more.  x86-64 has no 64-bit immediates, so masks such as
        `0xe7<<56`, the address mask and constants such as `ATOM_nil`
        and `FUNCTOR_dot2` each need a 10-byte `movabs` into a register,
        where the old layout used 32-bit immediates and a shift.
        Testing the upper 32 bits (`topWord()`) brings nreverse to 4.1%
        more instructions; the constants and the address mask remain.
      - Not measured: ARM64, where Top Byte Ignore would make `valPtr()`
        free and a 64-bit constant with only a top byte is one `movz`.
 4. Add `T_PTRINT`; convert tabling and private tries.
