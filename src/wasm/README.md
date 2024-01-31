# WASM Specific code

This directory contains the WASM specific code.   For building the WASM
version, see https://www.swi-prolog.org/build/WebAssembly.md.

## Benchmarking

Using node.js, we  can benchmark from the build directory  in the same
way as we can benchmark any version.

   node src/swipl.js -O ../bench/run.pl -s 10

To benchmark  inside the browser,  we can  compile the benchmark  to a
self contained .qlf file using this command in the `bench` directory:

    swipl -Dstatic -O qlf -c --include run.pl

Next, we can symlink run.qlf from  this directory.  After that, we can
run from the WASM build directory

    swipl ../src/wasm/server.pl

And open the indicated URL.  Run the WASM shell and run

    ?- ['http://localhost:8080/wasm/bench.qlf'].
	?- set_prolog_flag(heartbeat, 1000000).
	?- run(0.1).
