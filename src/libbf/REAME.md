# LibBF

## Data representation for integers:

  - bf->expn
    Most significant bit
  - bf->tab[bf->len-1] .. bf->tab[0]
    Data limbs.  Highest bit of bf->tab[bf->len-1] is always 1.
