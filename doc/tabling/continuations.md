# Reinstalling a continuation as a whole

Since 56e5c654c, `'$tbl_reset'/6` resumes a suspended goal by pushing all
environments of the continuation list at once (`push_continuation_list()`
in src/pl-cont.c) instead of running `call_continuation/1`, which
reinstalls one environment at a time.  An old design decision says the
incremental version avoids quadratic behaviour.  This note checks that
claim and whether it applies to tabling.

## History and literature

  - 66e3eae81 (2013-03-22, suggested by Bart Demoen): "Pushing the whole
    continuation in call_continuation leads to quadratic behaviour."  It
    replaced the C `call_continuation/1`, which pushed all frames
    (outermost first, chained through `parent`), by the tail-recursive
    Prolog `call_continuation/1` calling `'$call_one_tail_body'/1` per
    chunk.  The comment in boot/init.pl dates from this commit.
  - Schrijvers, Demoen, Desouter and Wielemaker, *Delimited continuations
    for Prolog*, ICLP 2013 / TPLP 13(4-5), section 4.2 "Alternative
    implementations": "Reinstalling the whole continuation in one go at
    call continuation.  This can lead to repeatedly scanning (a copy of)
    the same continuation, and sometimes leads to changing the runtime
    complexity of the program from linear to quadratic."  Details are
    referred to KU Leuven report CW631 (Demoen et al. 2013), which is not
    publicly accessible.

## Why

Let a resumed continuation consist of k chunks and let it shift again
after entering j of them.

  - Incremental: the environments are the j entered chunks plus the
    frame of `call_continuation/1`, whose environment holds the
    remaining chunks as the term `Rest`.  `put_continuation()` scans j+1
    frames; `Rest` is shared, not scanned.  Capture and the next resume
    are O(j).
  - Whole: all k environments are real frames, so the capture scans k
    frames and the next resume pushes all of them: O(k).

A generator that shifts from a non-tail-recursive loop has k = n at the
n-th shift and j = 1, so n shifts cost O(n) incrementally and O(n²) as
a whole.

## Tabling

Tabling stores each captured continuation in the worklist with
`term_to_fastheap()` and restores it for every resumption with
`put_fastheap()`.  Both are Θ(|K|), the size of the whole continuation
term, including a shared `Rest`.  Pushing all environments is O(|K|)
too (the arguments are linked, not copied), and a capture after
reinstalling everything is O(k) ≤ O(|K|) followed by the Θ(|K|) copy.
So per suspension and per resumption, both strategies cost Θ(|K|) up to
a constant factor: reinstalling the whole continuation does not change
the complexity of tabled programs.  Plain `reset/3` still uses the
incremental `call_continuation/1`.

## Measurements

Same build, with a temporary switch selecting the strategy (pinned,
times in ms).  Programs: scratchpad `gen.pl` and `tab.pl`, described
below.

Generator, `gen(N,Max) :- N < Max, !, shift(N), N1 is N+1, gen(N1,Max),
done(N).`, driven by `reset/3` in a loop:

| shifts | incremental | whole  |
|-------:|------------:|-------:|
|  5,000 |         7.0 |    831 |
| 10,000 |         8.1 |  3,029 |
| 20,000 |        16.3 | 12,554 |
| 40,000 |        25.6 | 52,827 |

The tail-recursive version is linear both ways (0.7-9.2 ms).

Tabling: `r(X) :- max(M), walk(M), X = 0.` with
`walk(N) :- r(_), N1 is N-1, walk(N1), done(N).`, i.e., a non-tabled,
non-tail-recursive loop that calls the incomplete table at increasing
depth:

| depth | whole (`'$tbl_reset'`) | incremental |
|------:|-----------------------:|------------:|
|   250 |                    6.7 |         6.5 |
|   500 |                   23.1 |        24.4 |
| 1,000 |                   88.4 |        97.0 |
| 2,000 |                  330.9 |       391.2 |

Both are quadratic because of the copies in the worklist; the whole
version is slightly faster.

## Related fix

The first `push_continuation_list()` re-walked the list from the start
for each element, which is quadratic in the number of chunks.
c778d6822 collects the elements once.
