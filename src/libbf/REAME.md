# LibBF

## Data representation for integers:

  - bf->expn
    Most significant bit
  - bf->tab[bf->len-1] .. bf->tab[0]
    Data limbs.  Highest bit of bf->tab[bf->len-1] is always 1.

## Specific topics

### Random number generation

  - https://github.com/ESultanik/mtwister
    Public domain version of the twister in C.

## (Re-)design considerations

  - We need a per-thread context.
    - Pass LD into ar_*() functions
    - Shared
      Can we allocate result variable limbs directly on the
      global stack target position?
      - Need to know the init target is the result
        (valueExpression knows)
      - Cannot do GC as it moves bignums shared on the stack.
    - BF Context
      - Add alloc size to context?
        - Over allocate
	- Do not schrink too quickly
	  --> double speed on gcd!
      - Maintain double-linked list of allocations, starting
        from a small fixed buffer.
  - Add new signature for bignum functions
    int f(ctx, ...), return 0 on success, and the BF codes
    (abstracted on various failures (undefined, memory
    overflow)
    - bn_xxx()
  - Use gcc builtins for overflow checks.  Provide native
    C as alternative.