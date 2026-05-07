# §1d behaviour-difference inventory: `_PL_char_types[0x80..0xff]` vs `uflags_map[0]`

This document catalogues every code point in the range `0x80..0xff` whose
classification will change when the parser cutoff moves from `<= 0xff` to
`<= 0x7F` (plan A §1) and the legacy `_PL_char_types[]` table is truncated to
128 entries. After the refactor, classification of these code points is
sourced from the Unicode flags table (`uflags_map[0]`) instead of the legacy
ISO-Latin-1 table.

Sources for this comparison:
- Legacy values: `src/os/pl-ctype.c` lines 1023–1048 (the 0x80..0xff portion).
- Unicode values: `src/pl-umap.c` `ucp0x00[256]` lines 35–50.

Legacy class abbreviations: `CT` control, `SP` space (layout), `SO` solo,
`SY` symbol (glueing), `UC` uppercase letter, `LC` lowercase letter.

Unicode flag bits (`src/pl-umap.c:8-15`): `0x40` = `U_OTHER`, `0x10` =
`U_LAYOUT`, `0x20` = `U_SOLO`, `0x02` = `U_ID_CONTINUE`, `0x03` =
`U_ID_CONTINUE | U_ID_START`, `0x07` = same plus `U_UPPERCASE`, `0x22` =
`U_SOLO | U_ID_CONTINUE`.

## Significant changes

### 1. NBSP becomes a stray character — primary motivation for the refactor

| Code | Char | Legacy | Unicode flags | Before | After |
|------|------|--------|---------------|--------|-------|
| U+00A0 | (NBSP) | `SP` | `0x40` (other) | whitespace | stray-character error |

Aligns with `Pattern_White_Space` (UAX #31 R3a) and PIP §4.3. Resolves the
doc-vs-code mismatch documented in plan A §M1.

### 2. NEL becomes layout — was control

| Code | Char | Legacy | Unicode flags | Before | After |
|------|------|--------|---------------|--------|-------|
| U+0085 | (NEL) | `CT` | `0x10` (layout) | rejected | layout |

Pattern_White_Space inclusion. NEL outside quoted material was previously
rejected as a control character; it is now valid layout.

### 3. SOFT HYPHEN becomes stray — was solo

| Code | Char | Legacy | Unicode flags | Before | After |
|------|------|--------|---------------|--------|-------|
| U+00AD | (SHY) | `SO` | `0x40` (other) | solo atom | stray-character error |

Soft hyphen is `Cf` (Format). Per the existing `man/overview.doc:3367-3374`
"Control and unassigned (C\*) characters produce a syntax error … outside
quoted atoms/strings", this is the documented intent.

### 4. Vulgar fractions become stray — were solo

| Code | Char | Legacy | Unicode flags | Before | After |
|------|------|--------|---------------|--------|-------|
| U+00BC | ¼ | `SO` | `0x40` (other) | solo atom | stray |
| U+00BD | ½ | `SO` | `0x40` (other) | solo atom | stray |
| U+00BE | ¾ | `SO` | `0x40` (other) | solo atom | stray |

These are general category `No` (other number). Per PIP §4.1 and the existing
manual paragraph on "Other characters", `No` is deliberately excluded from the
identifier set; only the explicitly listed super- and subscript digits extend
identifiers. The `solo` classification in the legacy table was an accident of
ISO Latin-1 history.

### 5. Superscript digits become id-continue — were solo

| Code | Char | Legacy | Unicode flags | Before | After |
|------|------|--------|---------------|--------|-------|
| U+00B2 | ² | `SO` | `0x02` (id_continue) | solo atom | id_continue (var²) |
| U+00B3 | ³ | `SO` | `0x02` (id_continue) | solo atom | id_continue |
| U+00B9 | ¹ | `SO` | `0x02` (id_continue) | solo atom | id_continue |

The PIP §4.1 super/subscript-digit profile addition. Allows `X²`, `Y₁` etc.
Code that wrote `² . ³` expecting two atoms now needs whitespace.

### 6. ASCII-symbol-glueing characters become solo

The legacy table classifies many ISO Latin-1 punctuation marks as `SY`,
which means they glue with adjacent ASCII symbols to form compound atoms
(like `==` or `=..`). Per PIP §4.2, Unicode symbols are *solo*: they form
single-code-point atoms and never glue. The following code points stop
glueing:

| Code | Char | Description | Was | Becomes |
|------|------|-------------|-----|---------|
| U+00A1 | ¡ | INVERTED EXCLAMATION | symbol (glue) | solo |
| U+00A2 | ¢ | CENT SIGN | symbol | solo |
| U+00A3 | £ | POUND SIGN | symbol | solo |
| U+00A4 | ¤ | CURRENCY SIGN | symbol | solo |
| U+00A5 | ¥ | YEN SIGN | symbol | solo |
| U+00A6 | ¦ | BROKEN BAR | symbol | solo |
| U+00A7 | § | SECTION SIGN | symbol | solo |
| U+00A8 | ¨ | DIAERESIS | symbol | solo |
| U+00A9 | © | COPYRIGHT | symbol | solo |
| U+00AB | « | LEFT-POINTING DOUBLE ANGLE QUOT MARK | symbol | solo (later: quote pair, §4) |
| U+00AC | ¬ | NOT SIGN | symbol | solo |
| U+00AE | ® | REGISTERED | symbol | solo |
| U+00AF | ¯ | MACRON | symbol | solo |
| U+00B0 | ° | DEGREE | symbol | solo |
| U+00B1 | ± | PLUS-MINUS | symbol | solo |
| U+00B4 | ´ | ACUTE ACCENT | symbol | solo |
| U+00B6 | ¶ | PILCROW | symbol | solo |
| U+00B7 | · | MIDDLE DOT | symbol | solo + id_continue |
| U+00B8 | ¸ | CEDILLA | symbol | solo |
| U+00BB | » | RIGHT-POINTING DOUBLE ANGLE QUOT MARK | symbol | solo (later: quote pair, §4) |
| U+00BF | ¿ | INVERTED QUESTION MARK | symbol | solo |
| U+00D7 | × | MULTIPLICATION | symbol | solo |
| U+00F7 | ÷ | DIVISION | symbol | solo |

Most concretely affects code that wrote things like `¬p` expecting it to
glue with adjacent `+`/`-`/`=`/etc. Such code is rare. Unicode operators
declared via `op/3` continue to work.

U+00B7 (MIDDLE DOT) is special: gains `U_ID_CONTINUE`, so it may also appear
*inside* identifiers (e.g. `foo·bar`), in addition to forming a solo atom.

## Unchanged behaviour (no migration impact)

- **U+0080..U+0084, U+0086..U+009F** (C1 controls, except NEL): legacy `CT`,
  Unicode `0x40` (other). Both rejected.
- **U+00AA, U+00B5, U+00BA, U+00DF, U+00C0..U+00D6, U+00D8..U+00DE,
  U+00E0..U+00F6, U+00F8..U+00FF** (Latin letters and ordinal indicators):
  legacy `LC`/`UC`, Unicode `0x03`/`0x07`. Both classify as identifier-start
  with the appropriate atom-vs-variable distinction.

## Summary count

| Behaviour change | Count | Migration impact |
|---|---|---|
| Whitespace ↔ stray (NBSP) | 1 | break for source files with NBSP between tokens |
| Layout ← control (NEL) | 1 | new accept (NEL is now valid layout) |
| Stray ← solo (SHY, ¼½¾) | 4 | break for code using these as standalone atoms |
| Id-continue ← solo (² ³ ¹) | 3 | break for `²` etc. as standalone atom; gain for `var²` |
| Solo ← glueing-symbol | 23 | break for code relying on ISO-Latin-1 symbols glueing |
| Unchanged | 224 | — |

## How the refactor moves the boundary

Before:
```c
#define CharTypeW(c, t, w) \
    ((unsigned)(c) <= 0xff ? CharTypeA(c, t) : (uflagsW(c) & (w)))
```
For c in 0x80..0xff: `_PL_char_types[c]` (the legacy ISO Latin-1 table) is
consulted.

After:
```c
#define CharTypeW(c, t, w) \
    ((unsigned)(c) <= 0x7F ? CharTypeA(c, t) : (uflagsW(c) & (w)))
```
For c in 0x80..0xff: `uflags_map[0][c]` (the Unicode flags first page) is
consulted.

Both tables happen to live in C source today; the refactor removes the
0x80..0xff entries from `_PL_char_types[]`, leaving the Unicode-derived
table as the single source of truth for non-ASCII classification.
