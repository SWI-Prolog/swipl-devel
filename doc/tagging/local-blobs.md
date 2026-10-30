# Thread-local blobs

Status: **design.**  Steps 1–4 of the [plan](#plan) are done.

Many C objects that Prolog refers to are used by one thread only:
tabling worklists and SCC components, private answer tries, Janus
Python objects, most streams and clause references.  Now and then such
a reference leaves the thread: another thread inspects a table (IDG
viewer), the reference is stored in the database or sent in a message.
This note proposes blobs that start life owned by their thread, are
reclaimed by that thread without global atom-GC (AGC), and are
_promoted_ to ordinary shared blobs when they escape.  Promotion does
not change the `atom_t`, so there is only ever one representation of a
reference.


## The problem

| object | handed to Prolog as | issue |
|--------|---------------------|-------|
| worklist, SCC component (`pl-tabling.c`) | raw integer (`PL_unify_pointer()`) | `get_worklist()` only `assert()`s the magic; `get_scc()` reads the magic of possibly freed memory.  A stale integer is a use-after-free. |
| private answer trie | trie blob, created lazily by `trie_symbol()` | each private table that reaches Prolog costs an atom that only AGC reclaims |
| answer trie node | raw integer | unchecked |
| Janus Python object | blob | created in bulk, mostly temporary: AGC pressure |

Tables can be abolished at any time, also while the thread is
backtracking into them, and other threads may inspect them.

## What blobs cost today

  - A `PL_BLOB_NOCOPY` blob without `PL_BLOB_UNIQUE` (e.g. the trie
    blob) is not in the atom table.  `lookupBlob()` takes a slot from
    the thread's reserved batch (`reserveAtom()`) and fills it in.
    Creation is cheap.
  - `PL_BLOB_UNIQUE|PL_BLOB_NOCOPY` hashes on the `->name` pointer, so
    "the blob for this C object" is a lock-free hash lookup.  This is
    what clause references could use.
  - Every creation does `ATOMIC_INC(&GD->statistics.atoms)`, a cache line
    shared by all threads, and every 128 creations `considerAGC()`.
  - Reclaiming a blob needs a global AGC: all threads are scanned.
  - Since Janus, a blob can be _discarded_: `PL_free_blob()` calls the
    `release` hook and sets `->name` to `NULL`.  The atom lives on, but
    `PL_blob_data()` returns `NULL`, so a stale reference raises an
    existence error rather than reaching freed memory.  Object lifetime
    and atom lifetime are thus independent.


## Design

### State

A new state bit `ATOM_LOCAL_REFERENCE` in `Atom->references`, next to
`ATOM_RESERVED`/`VALID`/`MARKED`/`DESTROY`.  Local atoms live in aligned
chunks of 64 atom slots owned by one thread; a global table maps a chunk
to its owner and its `used` and `marked` bitmaps (see step 1 of the
plan).  Each thread counts its live local atoms (`LD->atoms.local`).
If that count is 0, all escape checks below are skipped.

### Creation

`lookupBlob()` with a flag (or a blob-type flag `PL_BLOB_LOCAL`) that
sets `ATOM_LOCAL_REFERENCE` and counts in `LD->atoms.local` instead of
`GD->statistics.atoms`.  `PL_BLOB_UNIQUE` blobs go into the atom table
as now, so that a lookup for the same C object finds them, also from
another thread (see promotion).  Objects without a unique blob type
store their atom themselves (`trie->symbol`, `wl->symbol`, ...).

### Roots of a local atom

While an atom is local, only its owner can reach it, from:

  - the owner's stacks;
  - the owner's findall bags (`markAtomsFindall(ld)`);
  - counted references from storage of the owner thread;
  - `ld->atoms.unregistering`.

### Promotion (escape)

A local atom becomes shared by clearing `ATOM_LOCAL_REFERENCE` with the
CAS on `references` that the operation does anyway.  From then on it is
an ordinary blob, reclaimed by AGC.  Escapes:

  - **Registration.**  `PL_register_atom()` (clauses, records, trie keys
    that register, global data) promotes.  Thread-local containers may
    instead use a local registration that counts without promoting.
  - **Unique lookup from another thread.**  `lookupBlob()` finds the
    local atom of another owner: `bump_atom_references()` promotes.
  - **Object accessors in another thread.**  E.g. `trie_symbol()` called
    by a thread that follows an IDG edge into the tables of another
    thread.  It promotes the owner's symbol with a CAS that races with
    the owner's sweep, or installs a new, shared atom with a CAS on
    `obj->symbol` (see step 4).
  - **Unregistered copies.**  `R_NOLOCK` records in message queues and the
    keys of trie chunks are marked by AGC rather than registered.
    Sending a message and inserting into a shared trie promote the local
    atoms they copy.  Both already walk the term, so this is a test per
    atom, and none at all if `LD->atoms.local == 0`.

Over-promotion is never unsafe, it only delays reclamation to the next
global AGC.  Under-promotion is a bug: a local atom reachable from
another thread may be freed by its owner.  A debug check in
`PL_unify_atom()`/`PL_get_blob()` that the caller owns any local atom it
sees catches these.

### Local sweep

When `LD->atoms.local` grows past a threshold, which scales with the
stack size as the GC thresholds do, the owner sweeps at a safe point:

 1. Mark the local atoms reachable from its roots, using a local mark
    bit.  The stacks are scanned with `markAtomsOnStacks()` in a new
    _safe_ mode: `markAtomsOnGlobalStack(ld, safe)` trusts the indirect
    headers instead of bounding them, and the local stack scan needs no
    margin above `lTop`.  At a safe point this scan is complete.
 2. Release the unmarked local atoms that have no counted references:
    call the `release` hook and return the slot to the thread's
    reserved batch.

No other thread is involved.  Global AGC skips atoms that are still
local.  When a thread terminates, its local atoms are released.

### Object lifetime: discard

Discarding decouples freeing the C object from reclaiming its atom:

  - Freeing a worklist or component discards its blob: `->name`
    becomes `NULL`.  References to it elsewhere get an existence error.
    Answer tries do not use this: an abolished table hands its trie to
    the symbol, if any, so stale handles keep referring to an empty
    trie as before (step 4).
  - C code that keeps a raw pointer across calls must hold a reference
    to the object itself, as `trie_gen` already does (`acquire_trie()`).
    The object is freed when its reference count drops to 0.  Discarding
    only drops the owner's reference.
  - An inspector in another thread uses the same object reference while
    it walks the table.

The atom slot is then reclaimed lazily, by the local sweep or by AGC.
For objects whose lifetime is owned by Prolog (Janus objects, streams
without an explicit close) the `release` hook of the sweep frees them.


## Users

  - **Worklists and SCC components.**  Local blobs instead of raw
    integers.  They are never promoted in practice.
  - **Private answer tries.**  Local symbols; promoted when inspected
    from another thread or stored.  After abolish, the symbol owns the
    trie.
  - **Janus.**  Python object references are created in bulk and mostly
    temporary.  The sweep runs in the owner thread, which simplifies
    acquiring the GIL in the `release` hook.
  - **Streams.**  Usually opened and closed by one thread.  An alias or
    `stream_property/2` from another thread promotes.
  - **Clause and record references.**  `PL_BLOB_UNIQUE|PL_BLOB_NOCOPY` on
    the clause pointer: a lookup from another thread finds and promotes
    the local atom.
  - **Not:** answer trie nodes.  There are too many; they stay integers
    or are validated through their trie.


## Related: precise AGC at safe points

Global AGC marks the stacks of running threads asynchronously.  This is
conservative and needs `pushVolatileAtom()` for atoms created during
the scan.  An alternative is to let threads that ask for precise AGC do
their own marking at a safe point, as for `thread_signal/2`, using the
same safe mode of `markAtomsOnStacks()`.  With local blobs, most
short-lived blobs never reach global AGC, so this matters less.  The
safe mode is needed for the local sweep anyway.


## Costs and risks

  - Escape paths that bypass registration and the paths listed above.
    This needs an audit of C code that passes an `atom_t` to another
    thread or reads another thread's data.
  - `current_blob/2` and the atom statistics must skip, or promote, the
    local atoms of other threads.
  - The local sweep is linear in the stack size.  The threshold must
    make it rare relative to the number of local atoms created.
  - The chunk table costs one 32-byte descriptor per 64 atom slots that
    were ever used for local atoms.  Chunk slots are reserved for local
    atoms, also while the chunk is in the pool, so ordinary atoms do not
    reuse them.


## Plan

 1. **Done.**  `markAtomsOnStacks()` safe mode; local sweep machinery
    with the `ATOM_LOCAL_REFERENCE` state, without promotion:
      - `ATOM_LOCAL_REFERENCE` takes one bit of the reference count,
        which now has 27 bits (26 since step 4).
      - Blob types with the internal flag `PL_BLOB_LOCAL` (not yet in
        `SWI-Prolog.h`) create local atoms.  Such types cannot be
        `PL_BLOB_UNIQUE` yet.
      - Local atoms live in chunks of 64 consecutive, aligned atom slots.
        `GD->atoms.local_chunks` maps a chunk number to a descriptor with
        the owner and the `avail`, `used` and `marked` bitmaps; only the
        owner writes the bitmaps and descriptors are never freed.  A
        thread takes a chunk from a global pool of returned chunks or by
        extending the atom array to the next aligned chunk; the skipped
        slots become holes for ordinary atoms.  Creating a local atom
        takes the lowest free bit of a chunk (`reserveLocalAtom()`).
      - Marking (`markLocalAtom()`) sets a bit in the chunk's `marked`
        bitmap if the calling thread owns the chunk: a plain write to
        thread-owned memory.  Global AGC neither marks nor collects local
        atoms.
      - The sweep (`collectLocalAtoms()`) is requested through the
        existing `SIG_ATOM_GC` signal of the owner, not a new signal, and
        also runs from `garbage_collect_atoms/0`.  Roots are the stacks,
        `ld->atoms.unregistering`, the findall bags and the thread's
        message queue.  `markAtomsRecord()`, `markAtomsFindall()` and
        `markAtomsThreadMessageQueue()` take the mark function.
      - A sweep costs a scan of the stacks plus a visit of every used
        slot, so it only runs if there is at least one live local atom per
        16 words of stack (`considerLocalSweep()`).  If a gc thread does
        global AGC, `agc_handler()` in other threads only does the local
        sweep (`agcByGCThread()`); before, reusing `SIG_ATOM_GC` made the
        tabling thread run global AGC itself.
      - Local `PL_BLOB_NOCOPY` blobs are created by `lookupLocalBlob()` or,
        without a reference, `newLocalBlob()`: just a slot, no atom table,
        no MurmurHash.  Local atoms are not counted in the `live`/`space`
        statistics of their type, avoiding atomic updates of shared
        memory.
      - The sweep releases `used & ~marked` atoms without references.
        Afterwards, empty chunks go to the pool as long as the thread
        keeps at least its new threshold of slots.
      - `current_blob/2` does not enumerate local atoms, and they are not
        counted in `GD->statistics.atoms` or towards the AGC margin.
      - When a thread terminates, its local atoms without references are
        released; the others become ordinary atoms.  Its chunks go to the
        pool, without the slots of the promoted atoms.
      - Tests: `tests/GC/test_local_blobs.pl`, using
        `'$local_test_blob'/1` and `'$local_atoms'/2`.
 2. **Done.**  Worklists and SCC components as local blobs (no promotion
    needed):
      - Blob types `worklist` and `table_component`
        (`PL_BLOB_LOCAL|PL_BLOB_OWNED|PL_BLOB_NOCOPY`).  The object creates
        its blob on demand (`newLocalBlob()`) and remembers it in
        `symbol`, but holds no reference: the blob lives as long as Prolog
        refers to it.  The release hook, run by the owner's sweep, clears
        `symbol`; a later request creates a new blob.  Handles that exist
        at the same time are the same atom.  For promotion, see step 3.
      - A first version let the object hold a counted reference.  In
        ErgoAI cyc150 that kept millions of blobs alive for the lifetime
        of large SCCs, and after completion they were garbage that no
        sweep reclaimed until the `live` threshold, which had grown with
        them, was reached again.  Triggering sweeps on dropped references
        made it worse (80 s instead of 30 s): each sweep visits every used
        slot.
      - Freeing the object discards the blob (`discardBlob()`: the data
        becomes `NULL`).  A stale handle raises an existence error;
        before, it reached freed memory or a recycled object
        (`tbl_alloc()` reuses freed worklists and components).
      - Worklists and components record their `owner`.  For another
        thread, `unify_table_status()` reports a shared table under
        completion as `incomplete` instead of returning the owner's
        worklist, so handles never leave their thread.
      - Suspensions keep worklist handles in fastheap terms, which
        register their atoms.  The live SDG decodes the caller worklist
        of a suspension as a blob.
      - Answer trie nodes and the other raw pointers of `pl-tabling.c`
        remain integers.
      - Memory: in cyc150 the handles need about 2M extra atom slots at the
        peak.  The base's atom array is dense (6.45M slots for 6.44M
        atoms), so the slots extend it past 2^23 and `allocateAtomBlock()`
        allocates and initialises a block of 8.4M slots (about 400 MB),
        almost all unused.  Since "Do not initialise new blocks of the atom
        array", new atom blocks are mapped,
        untouched zeroed memory, which brings the peak to 9.97 GB (base
        9.83 GB) and the solve time to 29.7 s (base 28.2–28.8 s).
      - Cost, measured 2026-10-30 against the parent commit on a quiet
        machine (pinned, best of 5): the tabling benchmarks take 101% in
        total (bintree18 103%, 10kfib 108%); ErgoAI cyc150
        (`diploidnumberfordogs-2`) solves in 30.0–30.3 s instead of
        28.5–28.6 s (+5.5%) with a peak of 10.34 GB instead of 9.83 GB.
        Each fresh table creates two blobs (component and worklist).
      - Tests: `tests/tabling/test_tbl_handles.pl`.
 3. **Done.**  Promotion (`promoteLocalAtom()`): clear
    `ATOM_LOCAL_REFERENCE`, account the atom globally and, if global AGC
    is active, mark it.  The slot stays in the owner's `used` bitmap
    until its sweep finds the atom is no longer local.
      - `PL_register_atom()` and `bump_atom_references()` promote in the
        CAS that adds the reference.  `registerAtomLocally()` adds a
        reference without promoting, for data of the owner thread.  Users:
        global variables (`nb_setval/2` and friends are thread-local) and
        `term_to_fastheap_ex(t, true)`, which tabling's suspensions use.
        Event hooks (`prolog_listen/2`) and IDG monotonic dependencies
        keep promoting fastheap terms.
      - Still promoting although the data is thread-local, so only
        over-promotion: clauses of `thread_local/1` predicates and the
        registered keys of private tries (`acquire_key()`).  Step 4 should
        revisit the latter.
      - Unregistered copies promote: keys of chunked trie nodes
        (`mark_new_key()`; only private tries use chunks) and the records
        of message queues (`create_thread_message()`).
      - In `O_DEBUG` builds with `validate_api`, `check_atom_t()` raises
        an API error for a local atom of another thread.  Doing this for
        `validate_api` alone, which is on by default, cost about 3% in
        ErgoAI cyc150 (about 67 instructions per table).
      - `promoteAtomIfLocal()` first tests the owner of the atom's chunk:
        the chunk table is far denser than the atom array.
      - Cost: ErgoAI cyc150 solves in 30.4–30.8 s, the same as after
        step 2 (29.8–30.7 s); the base takes 28.5–28.9 s.
      - Normally the owner promotes: until it stores the atom where other
        threads can reach it, nobody else can see it.  The exception is
        a thread that inspects the tables of another thread and takes a
        trie handle (step 4).
      - `PL_BLOB_OWNED` blobs belong to a C object that remembers them,
        such as a tabling worklist.  Promotion adds a reference for the
        object in the same CAS, which the object drops when it is freed
        and discards the blob.  Global AGC thus never reclaims such a
        blob while the object lives, the object's `symbol` stays valid
        (a promoted handle is still `==` to later handles), and the
        release hook only runs in the owner's sweep.  A first version
        without this had to keep the hook from running in other threads,
        which cannot be decided safely, and gave promoted handles a new
        identity.
      - Object accessors from another thread and unique lookups remain
        for steps 4 and 5: no local blob type uses them yet.
      - Tests: `promote_assert`, `promote_message` and `promote_answer`
        in `tests/GC/test_local_blobs.pl`, using `'$local_atom'/1`;
        `gvar_local` there; and `promoted_identity` in
        `tests/tabling/test_tbl_handles.pl`, which promotes with
        `recorda/3` and drops that reference with `erase/1`.
        Whether a sweep reclaims an unpromoted trie key depends on stale
        words found by the conservative stack scan, so `promote_answer`
        tests the promotion itself.
 4. **Done.**  Private answer tries have local symbols:
      - Private answer tries get `TRIE_LOCAL`.  The `trie` blob type is
        now `PL_BLOB_LOCAL|PL_BLOB_OWNED|PL_BLOB_NOCOPY`; the symbols of
        other tries (shared tables, `trie_new/1`, variant tables) are
        created shared (`newSharedBlob()`), holding the reference of the
        trie's owner as before.  A second blob type for local tries would
        make `blob_type_property/2` list `trie` twice.
      - The variant table node owns a private answer trie.  Its local
        symbol has no reference for the trie, so the owner's sweep
        reclaims it like a worklist handle: the release hook clears
        `trie->symbol` and a later request creates a new symbol.  Before,
        each table that reached Prolog had a global atom, owned by the
        node, that only AGC reclaimed after the table was abolished.
      - Abolish does not discard.  The node hands the trie to its symbol
        (`TRIE_ORPHANED`, `release_local_trie()`), which destroys it when
        reclaimed: by the owner's sweep if it is still local, by AGC
        otherwise.  Stale handles thus still refer to an empty trie, as
        before.  Without a symbol, the trie is destroyed immediately.
      - Handing over must not race with a promotion by another thread,
        which would add the node's reference after the node dropped it.
        `disownBlob()` sets the new state bit `ATOM_DISOWNED_REFERENCE`
        with a CAS if the atom is still local, after which promotion adds
        no reference for the object, and else drops the object's
        reference.  This also fixes a leak of step 3: an owner that
        promoted a discarded worklist handle, e.g., by asserting it,
        added a reference nobody dropped.  `discard_tbl_handle()` now uses
        it as well.  The bit takes one more bit of the reference count.
      - Another thread (`local_trie_symbol()`, owner test:
        `trie->alloc_pool == LD->tabling.node_pool`) promotes the
        owner's symbol (`shareLocalBlob()`, which validates type and data
        and races with the owner's sweep through the CAS on the
        references) or installs a shared one with a CAS on
        `trie->symbol`.  It then checks `trie->symbol` again: the owner
        may have reclaimed the atom and reused its slot, in which case we
        may have promoted an unrelated atom, which is safe.  A destroyed
        trie gets `ATOM_nil` as symbol with a CAS, so no other thread
        installs a symbol for it; such a thread gets a discarded blob.
      - Over-promotion avoided: `droppable_table()` (components) and
        `tt_add_table()` (transactions) register locally.
        `'$idg_edge'/3` and friends kept the symbol of the next
        dependent in their foreign state across a redo without a
        reference; they now keep the trie and ask its symbol when they
        unify it.
      - The keys of private tries still promote (`acquire_key()`,
        `mark_new_key()`): the sweep does not scan tries for roots.
      - The local stack scan keeps its margin above `lTop`: at a safe
        point in a call, the arguments of the frame being built are
        there.  It finds stale words of earlier frames; tests that need
        a symbol to be reclaimed first overwrite that area.
      - Cost, measured 2026-10-30 against step 3 (quiet machine): the
        tabling benchmarks take 100.0% in total, none differs by more
        than 4%; ErgoAI cyc150 solves in 29.5–30.4 s instead of
        30.2–30.8 s, with a peak of 9.66 GB instead of 9.97 GB (the base
        before step 1: 9.83 GB).
      - Tests (`tests/tabling/test_tbl_handles.pl`): `trie_local`,
        `trie_no_global_atoms`, `trie_reclaimed`, `trie_abolished`,
        `trie_promoted`, `trie_disowned`, `disowned_worklist` and, for
        another thread taking a handle through the IDG edge of a shared
        incremental table, `trie_steal_existing` and `trie_steal_new`.
        Each detects a mutation of the code it tests.
 5. Other blob types.  Most of these are `PL_BLOB_UNIQUE`, so this
    needs local unique blobs first: the sweep must take them out of the
    atom table, using the bucket protocol of `invalidateAtom()` and
    `destroyAtom()`.  `PL_BLOB_LOCAL` then moves to `SWI-Prolog.h`.
    First for clause and record references, which were copying
    `PL_BLOB_UNIQUE` blobs (a `clause_ref` or `RecordRef*` as key, with
    a malloc() per blob and a hash over the copy):
      - **Done.**  They are now `PL_BLOB_UNIQUE|PL_BLOB_NOCOPY` on the
        `Clause` or `RecordRef` itself, ordered by address (a compare
        hook: for `NOCOPY` blobs, standard order otherwise compares the
        bytes of the clause, which change).  The VM used the copied
        `clause_ref` as frame clause: a restarted continuation now
        pushes a `clause_ref` below its frame, where AGC finds the blob
        in its key and the stack shifter relocates the frame's
        `clause`; a compiled trie clause carries a `clause_ref` after its
        code (`trie_clause_cref()`).  Measured against the previous
        commit: `clause/3` enumeration 10–28% faster, `recorded/3` 8%,
        `assertz/2`+`erase/1` and `reset/3` 3–5%; tabling benchmarks
        98.5% in total; ErgoAI cyc150 unchanged.
      - Next: local unique blobs (see below).
    Candidates:
      - core: streams, clause and record references, closures
        (`pl-wrap.c`), undo (`pl-undo.c`), zippers (`pl-zip.c`);
      - Janus: `PY_OBJECT`;
      - xpce: `pce_blob` (`UNIQUE|NOCOPY`, with a `gc_margin`), whose
        lifetime interacts with xpce's own reference counting;
      - JPL: `jref_blob` (`UNIQUE`).  Its release hook deletes a JNI
        global reference, so running it in the owner thread, which is
        attached to the JVM, is an advantage;
      - packages: archive, bdb, memfile, socket, alarm (clib), pcre,
        semweb snapshots and turtle parsers, ssl/crypto contexts, tipc,
        yaml emitters.

    Each needs its own review of the escape paths, e.g. xpce objects
    passed to the GUI thread.
