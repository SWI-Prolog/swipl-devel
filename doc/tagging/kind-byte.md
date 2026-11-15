# The kind byte

Status: **steps 1-3 done** (7c7a5ef6e).

The low byte of a cell holds its _kind_: the coarse tag, the storage
bits and a variant bit, plus the two GC bits.  The kind is what
`tagex()` returns.  Every kind of cell gets its own value, so no test
needs to read indirect data to know what a cell is.  Only `pl-data.h`
depends on where the kind is.  The kind was also kept in the top byte
(`O_TOP_TAGS`), but that was slower on x86-64 and ARM64 and is removed
(see [top-tags.md](top-tags.md)); the tests below that mention it ran
while it existed.

```
 63                                     8 7 6 5 4 3 2 1 0
+----------------------------------------+-+-+-+---+-----+
|                 value                  |F|M|V|stg| tag |
+----------------------------------------+-+-+-+---+-----+
```

## Why at the bottom

Top tags cost 0.5–3% on x86-64, because x86-64 has no 64-bit
immediates: tag masks, the address mask and atom and functor constants
each need a `movabs` into a register (see top-tags.md, step 3).  At the
bottom, masks are 8-bit immediates and atom constants (`index<<8|kind`)
32-bit immediates for indexes below 2^23.

Issues the top-tag work found that depend on where the tags are, and
how this layout deals with them:

  - Assumptions about the position of the value (hashes, `atom>>7` in
    packages): `CELL_VAL_SHIFT`, `valTagged()`, `PL_atom_index()`.
  - Raw tag bits outside `pl-data.h`: hidden behind macros (cleanup
    commit; generated code unchanged).
  - Raw pointers on the stacks must never look like atoms or functors:
    an aligned pointer has 000 in the tag, which no atom or functor has.
  - Header detection: headers are the kinds with `STG_LOCAL`, also with
    the variant bit, so `isIndirectHdr()` is unchanged.
  - Integer range: `TAGGED_INT_BITS` per layout; tests use
    `valInt(consInt(n)) == n`.

## Steps

 1. **Done** (7c7a5ef6e).  Widen from 7 to 8 bits: bit 5 becomes the
    variant bit, the GC bits move to bits 6 and 7.  All existing kinds
    keep their values (variant 0), so all tests keep their meaning.
    Small integers have 56 bits (`max_tagged_integer` is 2^55-1).
    32-bit systems keep 24 bits of value in `atom_t` and `functor_t`:
    at most 16M atoms and 512K functors (was 32M and 1M).
    `TAGEX_COUNT` is 64; `EXBIT()` uses a `word`, `reloc_mask[]` has 64
    entries.  In the top layout `hasTag()` masks `0xc7` and
    `isIndirectHdr()` `0xd8`, to ignore the variant bit.
 2. **Done** (7c7a5ef6e).  Rationals get their own kind: `KIND_MPQ` is
    the integer tag with the variant bit, cells are
    `TAGEX_INDIRECT_MPQ`, headers `KIND_MPQ|STG_LOCAL`.
    `isMPQNum()`, `isMPZNum()` and `isInteger()` test the cell instead
    of reading the size word of the data (`MP_RAT_MASK`).
      - `indKind(w)` is the kind without the storage.  Code that copied
        an indirect with `tag()` lost the variant: GC relocation chains,
        `VM_globalIndirectFromCode()` and `extern_indirect()`.
      - The trie handles indirect keys by kind; both switches gained
        `TAGEX_INDIRECT_MPQ`.
      - `MP_RAT_MASK` remained in the data (step 3 drops it); O_DEBUG
        builds asserted in `get_rational_no_int()` that it agreed with
        the kind.  A script
        passes rationals through records, assert, findall/3,
        copy_term/2, tries, tabling, compiled clauses, `.qlf` files,
        GC, `fast_term_serialized/2` and arithmetic: correct in release,
        O_DEBUG and `O_TOP_TAGS` builds.
    Cost, measured 2026-11-02 against `top-tags-split` (pinned, both
    builds core plus the packages ErgoAI needs): tabling benchmarks
    98.9% and 99.2% in total, `bench/` 100.1% (geometric mean;
    pingpong 85%, det 93%, nreverse 104%), ErgoAI cyc150 the same
    (A 29.0–29.4 s against 29.0 s, B 11.4–11.6 s against
    11.3–11.5 s).

 3. **Done**.  Drop `MP_RAT_MASK`: the size words of MPZ and MPQ data
    hold the signed number of limbs (`mpz_size_stack()`), as the kind
    tells the two apart.  Tested with GMP and LibBF
    (`-DUSE_GMP=OFF -DUSE_LIBBF=ON`) and `O_TOP_TAGS`: all tests, the
    rational script and the hashes below.

## Hashes

term_hash/2 and variant_sha1/2 used to hash numbers by their cell or
indirect data, so the hash depended on the layout: integers that are
small in one layout and big in another, the size words of big numbers
and the GMP or LibBF representation.  Numbered variables were hashed as
their marked cell.  They now hash numbers by value: an integer that
fits in 64 bits as `int64_t`, a larger integer as sign and big-endian
magnitude (`mpz_export()` per byte, which LibBF supports), a rational
as numerator and denominator, a float as its double; variables by
number.  Small integers and other types take the same path as before;
only indirect numbers convert.  GMP, LibBF and `O_TOP_TAGS` builds
produce identical hashes (99 numbers and terms compared).  Hashes of
big integers and of terms with variables changed once.  Tests:
`tests/core_lang/test_hash.pl` (`simple_3`, `simple_6`, `simple_7`,
`variant_sha1:numbered_vars`).

Hashes are also the same on all platforms (441d87431).  The Murmur hash
reads bytes in a fixed order already; the input now is fixed too:
integers (numbers, atom hashes, intermediate hashes, lengths, arities)
as 64- or 32-bit little-endian values and wide text as little-endian
32-bit code points (`MurmurHashWide()`, also for the atom table hash of
wide atoms).  On 32-bit systems term_hash/2 returned the hash as signed;
it is unsigned now.  On little-endian systems with a 4-byte `wchar_t`
the conversions are no-ops.  WASM produces the same values as x86-64
for 21 compared terms and passes `test_hash.pl`, which now expects a
single value per test.  Also tested on big-endian s390x (QEMU user
emulation: 32/32 hash tests, the 21 terms identical, 52/52 tests of a
core build) and Windows (MinGW64 under Wine, 2-byte `wchar_t`: 32/32
hash tests, the 21 terms identical including code points above U+FFFF,
221/221 tests).  The s390x build found that `pl-hash.c` is also
compiled into `mkvmi`, without `PL_malloc()`: `MurmurHashWide()` now
streams the code points through the hash instead of converting them
into a buffer.

## Against master

Measured 2026-11-02, `kind-byte` (before squashing: the kind byte with numbers hashed by value, without the platform independent hashing) against `origin/master`
(f6ab1dd3d), which includes the integer handles for tabling and the
cleanup and hash fix of top-tags-split.  Pinned, both builds core plus
the packages ErgoAI needs, tabling benchmarks twice in both orders:

  - Tabling benchmarks 98.9% and 99.1% in total.  Shuttle 94–99%,
    pingpong 95–97%, DblFstLoop 97–98%; DoubleFirst 98–104%.
  - `bench/` 99.4% (geometric mean); pingpong and fib 95%, divide10
    106% (112% and 101%).
  - ErgoAI cyc150: A 28.9–29.2 s instead of 29.5–29.6 s, B 11.5–11.6 s
    instead of 11.7–11.9 s, peak 9.47 GB instead of 9.48 GB.

A first run showed shuttle at 114–119%.  That build had been updated
incrementally from a commit with top tags on by default; after
rebuilding it the same commit is faster than master.  Benchmark from
clean builds when the layout changes.

Since `O_TOP_TAGS` is gone, tabling handles use the full range of
small integers again (`TBL_HANDLE_BITS` is `TAGGED_INT_BITS-1`; the
serial has 25 bits instead of 22).

## Open

  - Other kinds that are now told apart by their data or context
    (e.g. int64 indirects on systems without GMP/LibBF).
  - ~~Measure `O_TOP_TAGS` on ARM64.~~  Done: ~5% slower; removed.
