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
