# PIP: Unicode support for Prolog

**Status:** Draft
**Authors:** Jan Wielemaker (SWI-Prolog Solutions b.v.)
**Audience:** Prolog Improvement Proposal (PIP) working group

## 1. Motivation

ISO Prolog (ISO/IEC 13211-1) specifies the Prolog source syntax in
ASCII. Modern programs routinely use Unicode in atoms, strings,
identifiers, and even operators, but the way each implementation
extends the source syntax to Unicode is ad-hoc, undocumented, or
implementation-specific. This proposal sketches a coherent, common
direction for Unicode handling in standard Prolog, drawing on
implementation experience from SWI-Prolog and Ciao and on the wider
ecosystem of languages that have addressed the same questions
(Python, Rust, Julia, C#, Swift, Ada).

The proposal covers ten areas:

1. Source-text identifier syntax (UAX #31).
2. Tokenisation of symbol and punctuation characters (solo vs.
   glueing).
3. **Paired delimiters**: Unicode bracket pairs (`Ps`/`Pe`) and
   quote pairs (`Pi`/`Pf`) as standalone term and string syntaxes.
4. Whitespace.
5. Escape sequences for code points (`\u`, `\U`).
6. Code-point semantics of `atom_codes/2`.
7. **Numbers**: ASCII at the source level, any Unicode `Nd` block
   at runtime conversion, with same-block-per-number and ASCII
   syntax characters.
8. Grapheme clusters and string-level Unicode operations.
9. Column-based stream positioning and column-aware `format/2`.
10. **Pluggable Unicode normalisation** for the term reader and
    writer (opt-in, off by default).

The proposal does **not** require unconditional NFC normalisation
of every atom — Prolog atoms double as byte-faithful identifiers
and as containers for arbitrary text, so silently rewriting them is
unsafe (see §4.1).  Confusable detection (UTS #39) is out of scope.

## 2. Background: Unicode terminology

UAX #31 ("Unicode Identifier and Pattern Syntax") defines the
properties most relevant here:

- `ID_Start`, `ID_Continue` — base identifier sets, derived from
  General Category.
- `XID_Start`, `XID_Continue` — the same sets, modified to be closed
  under NFKC normalisation. UAX #31 recommends `XID_*` for new
  language designs.
- `Pattern_White_Space` — an immutable, deliberately small set of
  whitespace code points: `U+0009..U+000D, U+0020, U+0085, U+200E,
  U+200F, U+2028, U+2029`. Note that `U+00A0` (NBSP) is **not** in
  this set.

Unicode general categories used below: `Lu` (uppercase letter), `Ll`
(lowercase letter), `Lt` (titlecase letter), `Lm` (modifier letter),
`Lo` (other letter), `Nd` (decimal digit), `Nl` (letter number), `No`
(other number), `Sm/Sc/Sk/So` (symbols), `Pc/Pd/Ps/Pe/Pi/Pf/Po`
(punctuation).

## 3. Comparison: Ciao vs. SWI-Prolog

Both implementations classify each Unicode code point into a small
syntax class. The data structures differ:

- **Ciao** (`core/engine/unicode_gen/unicode_gen.c`) uses an
  *exclusive enum*: every code point belongs to exactly one of
  `LAYOUT`, `LOWERCASE`, `UPPERCASE`, `DIGIT`, `SYMBOL`, `PUNCT`,
  `IDCONT`, `INVALID`.
- **SWI-Prolog** (`src/Unicode/prolog_syntax_map.pl`) uses a *bitmask*:
  each code point may carry any combination of `id_start`,
  `id_continue`, `uppercase`, `layout`, `symbol`, `solo`, `other`,
  `decimal`.

The category-by-category mapping is summarised below; cells marked
**diff** are deliberate divergences worth discussing in the working
group.

| Unicode | Ciao | SWI-Prolog (current `Unicode` branch) |
|---|---|---|
| Zs, Zl, Zp | `LAYOUT` | (not specifically classified — see Pattern_White_Space) |
| Cc bidi WS/S/B (TAB, LF, ...) | `LAYOUT` | `layout` (via Pattern_White_Space) |
| Other Cc, Cf, Cs, Co, Cn | `INVALID` | `other` |
| Sm, Sc, Sk, So | `SYMBOL` | `solo` (**diff**: Ciao glues, SWI does not) |
| Pc, Pd, Po | `SYMBOL` | `solo` (**diff**) |
| Ps, Pe | `SYMBOL` | `bracket` paired delimiter, see §4.3 (**diff**) |
| Pi, Pf | `SYMBOL` | `quote` paired delimiter, see §4.3 (**diff**) |
| Lu (with XID_Start) | `UPPERCASE` | `id_start` + `uppercase` |
| Lt (with XID_Start) | `UPPERCASE` | `id_start` (**diff**: Ciao starts variable, SWI starts atom) |
| Ll, Lo, Lm, Nl (with XID_Start) | `LOWERCASE` | `id_start` |
| XID_Continue \\ XID_Start | `IDCONT` | `id_continue` |
| No | `IDCONT` (**diff**) | `other` (not in identifier set) |
| Me | `INVALID` | `other` |
| Nd | `IDCONT` | `id_continue` + `decimal` |

The points where the two implementations actually disagree are:

1. **`No`** (superscript and subscript digits, fractions, Roman
   numerals, circled digits, ...): Ciao puts the whole category in
   `IDCONT`, SWI-Prolog explicitly does not — but extends
   `XID_Continue` with the digit-shaped subset (super- and
   subscript digits) only.
2. **`Lt`** (titlecase letters): Ciao starts a variable, SWI-Prolog
   (in this proposal) starts an atom because the Lu-only uppercase
   rule is simpler and more predictable than the broader derived
   `Uppercase` property.
3. **Sm/Sc/Sk/So + Pc/Pd/Po**: Ciao glues runs of these into
   compound atoms (like ASCII `==`); SWI-Prolog treats each as a
   solo atom (see §4.2).
4. **Ps/Pe and Pi/Pf**: Ciao glues these together with other
   symbols. SWI-Prolog treats each pair as a paired delimiter that
   produces a `'<open><close>'/1` compound: brackets wrap a term,
   quotes wrap literal text per the `double_quotes` flag (see §4.3).

## 4. Recommendations

### 4.1 Identifier handling

- **Use `XID_Start` and `XID_Continue`** as the base identifier set.
  These are the UAX #31-recommended sets; they exclude a small
  number of code points that are not closed under NFKC and are
  preferable for new language designs. (Most language standards
  written or revised after ~2010 use `XID_*`: Rust, Julia, C#,
  Swift, Ada.)
- **Allow super- and subscript digits as identifier continuation**,
  by an explicit profile addition to `XID_Continue`:
  `U+00B2, U+00B3, U+00B9, U+2070, U+2074..U+2079` (superscripts) and
  `U+2080..U+2089` (subscripts). This permits variables of the form
  `X²`, `Y₁`, which are common in mathematical and physical code.
  Julia is the precedent.
- **Determine variable vs. atom** from the start character: a
  variable starts with `_` or with a code point in general category
  `Lu`. Everything else identifier-like starts an atom. This is
  simpler than reading the derived `Uppercase` property and means
  titlecase letters (`Lt`) start atoms.
- **Do not normalise atoms automatically on every creation path.**
  Prolog atoms double as byte-faithful identifiers and as containers
  for arbitrary text (filenames, network input, JSON keys).
  Silently rewriting them on every `atom_codes/2`, `atom_concat/3`,
  or stream read would be a leaky abstraction. Make normalisation
  an explicit, opt-in operation: standard library predicates of the
  form `unicode_nfc/2`, `unicode_nfd/2`, `unicode_nfkc/2`,
  `unicode_nfkd/2`, `unicode_nfkc_casefold/2`, plus a per-call
  option on the term reader and force-quoting of denormalised text
  by the writer (see §4.9).  Implementations may use `utf8proc` (a
  small, MIT-licensed C library originating in Julia) for the
  underlying transformation.

### 4.2 Symbols

- **Unicode symbols and non-bracket / non-quote punctuation form solo atoms.**
  General categories `Sm`, `Sc`, `Sk`, `So`, `Pc`, `Pd`, `Po` each
  form a single-code-point atom; they do not glue with adjacent
  symbols. ASCII keeps its existing behaviour so that established
  operator tokens (`==`, `=..`, `:-`, `\+`, ...) continue to parse.
- **Operators built from Unicode symbols are declared with `op/3`.**
  Math-heavy code can declare `≤`, `≥`, `∈`, `∪`, `∩`, `→`, etc. as
  ordinary operators. Implementations that wish to ship a default
  operator table for common mathematical symbols may do so as a
  library, not as part of the core grammar.

The opening / closing punctuation (`Ps`/`Pe`) and initial / final
quotation classes (`Pi`/`Pf`) are *not* solo; they are paired
delimiters with their own syntax described in §4.3.

### 4.3 Paired delimiters: brackets and quotes

The opening / closing punctuation classes `Ps`/`Pe` and the initial /
final quotation classes `Pi`/`Pf` are *paired delimiters*. An
opening character followed by content and the matching closing
character reads as a unary compound whose functor is the two
delimiter code points joined, generalising `{Term}` ⇒ `'{}'(Term)`
to the full Unicode pair set. Brackets wrap a Prolog **term**;
quotes wrap **literal text**.

#### 4.3.1 Brackets (`Ps`/`Pe`)

```
?- read_term_from_atom('〈foo, bar〉', T, []).
T = '〈〉'((foo, bar)).
?- read_term_from_atom('⟦x+y⟧', T, []).
T = '⟦⟧'(x+y).
```

The content is parsed as a Prolog term — operators are honoured,
nesting works, and the ASCII bracket behaviour (`{T}` ⇒ `'{}'(T)`)
falls out as a special case. The pair table is sourced from Unicode
`BidiMirroring.txt` filtered to `Ps`/`Pe` (about 60 pairs in Unicode
17, including angle, corner, ceiling, floor, mathematical,
ornamental, fullwidth and CJK brackets).

#### 4.3.2 Quotes (`Pi`/`Pf`)

```
?- read_term_from_atom('«hello, world»', T, []).
T = '«»'("hello, world").
?- read_term_from_atom('“abc”', T, []).
T = '“”'("abc").
```

The content is treated as **literal text**, with the same escape-
sequence support as ASCII quoted strings (`\n`, `\t`, `\uXXXX`,
`\UXXXXXXXX`, ...). The contained value is converted to the type
selected by the `double_quotes` flag — string by default, also
atom, codes, or chars — and wrapped in the `'<open><close>'/1`
compound. The pair table comes from the `Pi`/`Pf` entries of
`BidiMirroring.txt` (eight pairs covering `«»`, `‹›`, and a handful
of Supplemental Punctuation marks) plus the standard left/right
curly pairs U+2018/U+2019 (`‘’`) and U+201C/U+201D (`“”`), which
have `Bidi_Mirrored=No` and are absent from `BidiMirroring.txt`.

#### 4.3.3 `code_type/2` accessors

The classification predicates expose paren and quote relationships:

```
?- char_type('⟨', paren(C)).
C = '⟩'.
?- char_type(O, paren('」')).
O = '「'.
?- char_type('«', quote(C)).
C = '»'.
?- char_type('"', quote(C)).
C = '"'.       % ASCII quotes have Close = Char.
```

`paren(Close)` covers ASCII `()`, `[]`, `{}` plus every Unicode
`Ps`/`Pe` pair. `quote(Close)` covers the ASCII quotes `'`, `"`,
`` ` `` (where `Close` equals `Char`) plus every Unicode `Pi`/`Pf`
pair. Both directions of the mapping are reversible.

#### 4.3.4 Error semantics

Mismatched closes (e.g. `«hello]`) and unmatched opens (e.g. `«hello`
without a closing `»`) raise `syntax_error/1`. There is no
fallback to a standalone solo atom; an opening delimiter never
appears in a bare term position.

### 4.4 White space

- **Layout characters are exactly `Pattern_White_Space`** (UAX #31
  R3a). The set is small and immutable, so a tokenizer written
  against it today behaves identically under any future Unicode
  release.
- **`U+00A0` (NBSP) is not whitespace.** It is deliberately
  excluded from `Pattern_White_Space`. Programs that paste from
  word processors will occasionally encounter NBSP in the wrong
  place; reporting it as a stray character is the right behaviour.

#### 4.4.1 Line termination

Of the eleven `Pattern_White_Space` code points, seven are
line-terminator-like and end a line of source text:

    U+000A LF     U+000B VT     U+000C FF     U+000D CR
    U+0085 NEL    U+2028 LINE SEPARATOR       U+2029 PARAGRAPH SEPARATOR

Conformant implementations should recognise this same set wherever
a line-ending matters in source text:

- **`%` line comments terminate** on any of the seven (so a comment
  that contains `U+0085` ends at the NEL, not at the next ASCII
  LF).
- **The source-position line counter increments** on each.
- **Backslash-newline continuation in quoted strings** — the form
  `\<EOL><blank>*` — accepts any of the seven as the `<EOL>`. The
  `\<U+0085>` and `\<U+2028>` forms behave the same as `\<LF>`.

The remaining four `Pattern_White_Space` members — `U+0020` SPACE
and the bidi marks `U+200E` LRM and `U+200F` RLM — are layout but
**not** line-enders.

User code can ask for either set via `char_type/2` /
`code_type/2`:

- **`prolog_layout`** — the eleven `Pattern_White_Space` code
  points (the parser's notion of layout).
- **`prolog_end_of_line`** — the seven line-terminator-like
  members of that set.
- **`end_of_line`** — kept at the original ISO/POSIX definition,
  the four ASCII control codes LF, VT, FF, CR. Code that needs
  the wider set should use `prolog_end_of_line`.

The `prolog_*` prefix follows the existing convention for
parser-specific predicates (`prolog_var_start`,
`prolog_atom_start`, `prolog_identifier_continue`,
`prolog_symbol`).

#### 4.4.2 Stray characters in source text

In token-start position — i.e. wherever layout is allowed — any
code point that is **not** in one of the recognised syntax classes
(layout, decimal, identifier-start, identifier-continue, solo,
bracket open, quote open) raises `syntax_error(illegal_character)`.
Concretely, the following are stray:

- C0 / C1 controls, surrogates, and unassigned code points (general
  category `Cc` / `Cs` / `Cn`).
- Noncharacter code points (`U+FDD0..U+FDEF`, `U+FFFE`, `U+FFFF`,
  and the analogous endpoints in higher planes).
- The `Zs` / `Zl` / `Zp` separator classes that are not in
  `Pattern_White_Space`: NBSP `U+00A0`, OGHAM SPACE MARK `U+1680`,
  the various typographic spaces `U+2000..U+200A`, NARROW NO-BREAK
  SPACE `U+202F`, MEDIUM MATHEMATICAL SPACE `U+205F`, IDEOGRAPHIC
  SPACE `U+3000`, the line/paragraph separators inside `Zl` / `Zp`
  not already in Pattern_White_Space, etc.
- `Cf` format characters not in `Pattern_White_Space` and not in
  `Other_ID_Continue`: SOFT HYPHEN `U+00AD`, ZERO WIDTH SPACE
  `U+200B`, the variation selectors, ...
- Enclosing combining marks (`Me`).
- `No` "other-number" code points outside the explicit super- /
  subscript-digit profile (vulgar fractions, Roman numerals,
  circled / parenthesised digits, ...).

Non-spacing combining marks (`Mn`, `Mc`) are likewise stray at
token-start position — they cannot start a token — but are in
`XID_Continue`, so they absorb into a preceding identifier.

#### 4.4.3 Inside quoted material

Inside single-quoted atoms, double-quoted strings, back-quoted
text, the new Unicode quote pairs (§4.3.2), `%` line comments,
and `/* ... */` block comments, **any Unicode scalar value** is
accepted verbatim (the surrogate range `U+D800..U+DFFF` is
unreachable in well-formed UTF-8 anyway). This matches every
comparable language — Python, Rust, Swift, Go, JavaScript, Java,
C++23, Haskell, OCaml, ... — and keeps `atom_codes/2` symmetric
with the source-text reader: any list of scalars that
`atom_codes(A, Codes)` will accept must round-trip through
`writeq(A) ⇒ read_term/2`.

The escape sequences `\uXXXX` and `\UXXXXXXXX` (§4.5) exist for
portability and explicit clarity, never as the only way to embed
a code point. The single exception to byte-faithful acceptance is
the bidirectional override / isolate range (`U+202A..U+202E` and
`U+2066..U+2069`), which is rejected as a Trojan-source defense
(CVE-2021-42574). The writer is responsible for force-quoting
atoms whose content includes zero-width or otherwise visually-
unstable code points (see §4.11) so that `writeq` round-trips
faithfully.

### 4.5 Escape sequences

- **`\uXXXX`** — exactly four hexadecimal digits, denoting a
  Unicode scalar value in the BMP.
- **`\UXXXXXXXX`** — exactly eight hexadecimal digits, denoting a
  Unicode scalar value up to `U+10FFFF`.

These are the de facto convention across Unicode-aware languages
(C, C++, Rust, Python, JSON, Java, ...). Most Prolog
implementations already accept them; this proposal merely
standardises the form.

### 4.6 `atom_codes/2`

The list argument of `atom_codes/2` (and analogously `string_codes/2`,
`char_code/2`, `atom_chars/2`) is a list of **Unicode scalar values**,
that is, integers in `0..0x10FFFF` excluding `0xD800..0xDFFF`. UTF-16
surrogate halves never appear; a single Unicode scalar is one list
element regardless of UTF-8/UTF-16 encoding details. This rules out
implementations that expose UTF-8 byte sequences via this predicate,
which leaks an encoding choice into the language.

### 4.7 Numbers

Two layers, with different rules:

- **Source text (`read_term/[2,3]`).** Numeric literals use ASCII
  digits `0..9` only. Non-ASCII `Nd` code points may appear inside
  identifiers (where they are `id_continue`) but cannot start a
  number or extend an ASCII number. This keeps the source-level
  grammar single-script and matches the UAX #31 R6 recommendation
  for programming-language number syntax.
- **Runtime conversion (`atom_number/2`, `number_codes/2`,
  `number_chars/2`, `number_string/2`).** These accept decimal
  digits from any Unicode `Nd` block (Devanagari, Eastern Arabic,
  Fullwidth, ...). Two constraints apply:
  1. **All digits in a single number must come from the same
     block.** That includes the integer part and the fractional
     part of a float, the mantissa and exponent of a float, and
     the numerator and denominator of a rational. `१२३.४५` parses;
     `१२३.45` does not.
  2. **The non-digit syntax characters are always ASCII.** The
     sign (`+`, `-`), the rational separator (`r` or `/`), the
     floating-point decimal point (`.`), and the exponent letter
     (`e` or `E`) must use their ASCII forms; the look-alikes
     `−` (U+2212), `．` (U+FF0E), `Ｅ` (U+FF25) are rejected.

Examples:

```
?- number_string(N, "+१२३").       N = 123.
?- number_string(N, "−१२३").        false.       % U+2212 minus
?- number_string(F, "१२३.४५").     F = 123.45.
?- number_string(F, "१२३e५").      F = 1.23e7.
?- number_string(R, "१२३r४५").     R = 41 rdiv 15.
?- number_string(N, "1२").         false.       % mixed blocks
```

The character-code form `0'<C>` is unaffected by the same-block
rule: it produces the integer code point of any single Unicode
scalar `<C>`. In source text the term reader interprets the usual
escape sequences (`0'\n`, `0'·`, `0'\U0001F600`); the runtime
conversion family treats `0'<C>` as the literal next code point
without escape interpretation. Combining marks and other multi-
code-point sequences are not representable as a single integer.

`0x`, `0o` and `0b` radix literals stay ASCII-only; the radix
sigil is ASCII and the digits inside follow.

### 4.8 Graphemes

A *grapheme cluster* (UAX #29) is a user-perceived character — a
base letter plus its combining marks, an emoji ZWJ sequence, a
regional-indicator pair, and so on. It can span multiple code points
and need not have a single-code-point precomposed form. Prolog
should provide a standard library predicate `atom_graphemes/2` (and
its `string_graphemes/2` analogue) that relates an atom to a list of
single-grapheme atoms. This is what user code wants when iterating
over "characters" for display or editing purposes; iterating over
code points is rarely what is intended.

### 4.9 Stream positions and `format/2` columns

For text streams, `line_position/2` and the `position` field of
`stream_property/2` should be measured in **display columns**, not
bytes or code points. Combining marks contribute zero columns; CJK
and emoji "wide" characters contribute two. The same definition
governs alignment in `format/2` directives `~t`, `~|`, and `~+`,
and is exposed to the C API as `PL_wcwidth(int code)`.

The width of each code point is **derived from Unicode property
data** — `EastAsianWidth.txt` (UAX #11) plus the general category —
at table-build time and stored alongside the syntax classifier in
the same per-code-point byte. This is locale-independent and
identical on every supported platform; the Unicode version that
drove the table is reported by the read-only flag
`unicode_syntax_version` (§4.9).

#### 4.9.1 Relation to POSIX `wcwidth(3)`

The return-value contract is the **same as POSIX**
`wcwidth(3)`/`<wchar.h>`: −1 for non-printable, 0 for combining /
zero-width, 1 for normal, 2 for wide. Code that already uses
`wcwidth` against the POSIX convention drops in unchanged.

The differences are deliberate:

- **Locale-independent.** POSIX `wcwidth` is permitted to depend on
  `LC_CTYPE`; in the `C`/`POSIX` locale on glibc, for instance,
  `wcwidth` returns −1 for every non-ASCII code point.
  `PL_wcwidth(c)` is a pure function of `c`.
- **Cross-platform identical.** Every supported platform — Linux,
  macOS, Windows, WASM — gives the same answer. Notably,
  `wcwidth` does not exist at all on standard Windows; SWI-Prolog
  used to fall back to Markus Kuhn's `mk_wcwidth.c`, and that
  reference table is now superseded by the per-code-point table
  emitted from current Unicode data.
- **Versioned.** The width data is pinned to the Unicode release
  reported by `unicode_syntax_version`; querying the flag answers
  "which Unicode are these widths from?". POSIX `wcwidth` exposes
  no equivalent.
- **Full 32-bit code points.** The argument is `int` so non-BMP
  characters (emoji, supplementary planes) work on Windows, where
  `wchar_t` is 16-bit and a naive cast silently truncates.
- **East Asian Ambiguous → 1.** UAX #11 `A` (Ambiguous) is
  rendered as one column. POSIX leaves this implementation-defined;
  some glibc CJK locales return 2. SWI-Prolog follows the
  Western convention to keep alignment portable across locales —
  Kuhn's reference does the same.
- **Cc/Cn/Cs.** Control characters (`Cc`, including DEL and the
  C1 range) → −1. Unassigned (`Cn`) and surrogate (`Cs`) code
  points get the Unicode default per UAX #11 (1, or 2 in the
  CJK Ideograph default-W blocks). POSIX `wcwidth` is
  implementation-defined for these.
- **No `mk_wcwidth_cjk` variant.** Kuhn's `_cjk` variant bumps
  Ambiguous to 2; the SWI tree no longer carries it. Code that
  needs CJK-style ambiguous-as-wide rendering should layer that
  on top, e.g. by post-processing a `unicode_property/2` query
  for `east_asian_width(a)`.

In short, `PL_wcwidth(c)` is approximately "POSIX `wcwidth` with a
locale that always tracks current Unicode and resolves Ambiguous as
narrow". The POSIX-compatible return-value convention preserves
backward compatibility for foreign code that previously linked
against `mk_wcwidth.c`.

### 4.10 Reporting the Unicode version

Implementations should expose the Unicode version that drives the
source-syntax classifier as a read-only Prolog flag — proposed name
`unicode_syntax_version`, an atom such as `'17.0.0'`. This lets
portable code check at runtime which character set the parser was
built against. Implementations that bundle a separate Unicode data
set for normalisation, grapheme segmentation, and property queries
(e.g. via `utf8proc`) should document that version separately
through their library API; cross-reference between the two.

### 4.11 Pluggable Unicode normalisation in reader and writer

NFC normalisation is genuinely useful for the term reader and
writer, so the proposal recommends two opt-in mechanisms backed by
a kernel callback that an external normalisation library plugs in
to.  The kernel itself ships no normalisation logic; it just
provides the registration point and the policy.

**Kernel hook.**  The kernel exposes a single function-pointer
callback through the public C API:

```c
typedef int (*PL_atom_normalize_t)(unsigned char *in, size_t *len);

PL_atom_normalize_t PL_atom_normalize_hook(PL_atom_normalize_t new);
```

The hook normalises a UTF-8 byte sequence in place to the canonical
form chosen by the implementation (NFC in our reference
implementation).  The result for NFC is always shorter than or
equal to the input, so the caller's buffer is sufficient.  The hook
returns the new length through `*len`; an application that needs
NFD/NFKC/NFKD continues to use the explicit library predicates.

The reference implementation registers `utf8proc_map(... |
UTF8PROC_STABLE | UTF8PROC_COMPOSE)` from a wrapper around utf8proc
loaded as `library(unicode)`.  When that library is not loaded the
kernel hook is `NULL`.

**Reader option.**  `read_term/2,3` and `read_clause/2,3` accept a
new option `normalize(Bool)`.  When `true`, the text of *unquoted*
atoms (and unquoted functor names) is run through the hook before
the atom is interned, so two source forms that are canonically
equivalent yield the same atom.  Quoted atoms and string literals
are left alone — they are byte-faithful by design.  Requesting
`normalize(true)` without a registered hook raises
`existence_error(hook, unicode_normalize)` so the user is told
exactly what is missing.

**Prolog flag.**  A read/write Boolean flag `unicode_normalize`
provides the default value of the option, so `read_term/2`,
`read_clause/2`, and consult-style loaders pick it up without
per-call wiring.  The flag has an *active setter*: setting it to
`true` while no hook is registered triggers a kernel-side load of
the normalisation library, propagating any
`existence_error(source_sink, ...)` raised by `use_module/1` if
the library is unavailable.  This keeps the binding to a specific
library out of the language definition while still giving users a
one-line way to turn the feature on.

**Writer NFC quoting.**  Under `quoted(true)` (i.e.\ `writeq/1`,
`portray_clause/1`, etc.), the writer additionally force-quotes
atoms that contain at least one Unicode code point with
`wcwidth(c) == 0` (combining marks, ZWJ, variation selectors).
Such atoms are visually surprising as bare identifiers, since the
combining mark normally attaches to the preceding base character;
quoting them makes the denormalisation visible and ensures that
`writeq`/`read` round-trips produce comparable atoms on the
receiving side.  This rule is independent of the kernel
normalisation hook — it relies only on `wcwidth` — so it works in
any implementation, normalisation library or not.

The split between read-time normalisation and write-time NFC
quoting is deliberate.  Read-time normalisation requires a Unicode
data table; quoted writing requires only a small wcwidth table.  An
implementation can ship the latter without committing to the
former.

## 5. Open questions

The working group will need to decide:

1. **Titlecase letters (`Lt`).** Variable-start (Ciao) or atom-start
   (this proposal)? The simplicity of "uppercase = `Lu`" weighs
   against the broader, but more surprising, derived `Uppercase`
   property.
2. **Math symbols (`Sm`).** Solo (this proposal) or glueable? The
   strongest argument for glueing is mathematical Prolog (constraint
   languages, theorem provers) where compact composite operators
   are useful. The strongest argument for solo is consistency with
   the rest of the symbol/punctuation classes and the fact that
   `op/3` already provides the alternative.
3. **`Pattern_Syntax` boundary for solo.** UAX #31 R3a recommends
   that syntax characters come from the immutable `Pattern_Syntax`
   property (~3,000 code points). This proposal's `solo` is broader
   (Sm/Sc/Sk/So/Pc/Pd/Po, ~7,000 code points), trading immutability
   for an easier "everything looks like a symbol" rule. Tightening
   to `Pattern_Syntax` is an option.
4. **Curated Pi/Pf pairs.** The standard left/right curly quotes
   U+2018/U+2019 (`‘’`) and U+201C/U+201D (`“”`) have
   `Bidi_Mirrored=No` and so are absent from `BidiMirroring.txt`;
   §4.3.2 adds them by hand. Other Pi/Pf pairs that share the same
   property — e.g. CJK-shaped quotation marks — are not currently
   curated. Whether and how to extend the curated list is a working-
   group call.
5. **Confusables (UTS #39).** Out of scope for this proposal.
   Implementations may surface confusable detection through a
   linter or library; building it into the grammar is more aggressive
   than is warranted.

## 6. Reference implementations

- **utf8proc** (https://github.com/JuliaStrings/utf8proc, MIT licence)
  — small (~200 KB compiled, single C source plus generated tables),
  provides NFC/NFD/NFKC/NFKD, case folding, `XID_*` membership,
  general category, grapheme break, display width. Used by Julia,
  PostgreSQL, and SWI-Prolog (`packages/utf8proc/`).
- **SWI-Prolog `Unicode` branch** — this proposal's design is
  implemented end-to-end on the `Unicode` branch of
  https://github.com/SWI-Prolog/swipl-devel.
- **Ciao** — `core/engine/unicode_gen/` shows the alternative,
  exclusive-enum approach.

## 7. Acknowledgements

Discussion with Manuel Hermenegildo (Ciao) prompted the
side-by-side comparison in §3 and identified the `No`, `Lt`, and
symbol-glueing divergences. Daniel Lundin and the utf8proc
maintainers provided the C library underlying the SWI-Prolog
implementation. The wider language-design literature (Python's
PEP 3131, Rust RFC 2457, Julia's identifier rules, UAX #31 itself)
informs the recommended defaults.
