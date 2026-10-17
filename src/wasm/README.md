# WASM Specific code

This directory contains the WASM specific code.   For building the WASM
version, see https://www.swi-prolog.org/build/WebAssembly.md.

## Benchmarking

Using node.js, we  can benchmark from the build directory  in the same
way as we can benchmark any version.

   node src/swipl.js -O ../bench/run.pl -s 10

To benchmark  inside the browser,  we can  compile the benchmark  to a
self contained .qlf file using these commands in the `bench` directory:

    swipl port/tools/modularize.pl --dir=port/programs/swi \
          --include-all=include_all.pl programs/*.pl
    swipl -Dstatic --no-threads -O qlf compile --include run.pl

Next, we can symlink run.qlf from  this directory.  After that, we can
run from the WASM build directory

    swipl ../src/wasm/server.pl

And open the indicated URL.  Run the WASM shell and run

    ?- ['http://localhost:8080/wasm/bench.qlf'].
	?- set_prolog_flag(heartbeat, 1000000).
	?- run(0.1).

### Tiered compilation

WASM engines first compile functions with a baseline compiler and
replace them with optimized code in the background.  V8 (Node.js,
Chrome) has no on-stack replacement for WASM: a function uses its
optimized code from its next call.  The default VM is a single large
function, PL_next_solution(), that runs a whole benchmark in one call
when started as `run(1)` from the command line.  Depending on timing,
this may run as baseline code for the entire benchmark, which makes it
about twice as slow and makes results vary between runs.  Interactive
use re-enters PL_next_solution() for each query and heartbeat yield,
and thus runs optimized code.  To compare VM variants, disable the
baseline compiler:

  - Node.js and Chrome: `node --no-liftoff`,
    `chromium --js-flags=--no-liftoff`
  - Firefox: set `javascript.options.wasm_baselinejit` to `false` in
    `about:config`

### VM variants

By default the WASM version compiles the VM as a single function using
a `switch`.  `-DWASM_TAIL_CALL=ON` compiles each VM instruction into a
function that calls the next using a guaranteed tail call
(`-mtail-call`, requiring Chrome 112, Firefox 121, Safari 18.2 or
Node 20).  With optimized code, bench/ `run(1)` takes (Emscripten
4.0.15) relative to the switch:

| Engine                       | Tail calls |
|------------------------------|-----------:|
| Node 22 (`--no-liftoff`)     |       140% |
| Chromium 155 (`--no-liftoff`)|       162% |
| Firefox 158 (Ion only)       |       120% |

`-DVMI_FUNCTIONS=ON` without tail calls returns to a dispatch loop for
each instruction and is about twice as slow as the switch.
