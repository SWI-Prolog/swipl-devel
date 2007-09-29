---+ YAP compatible library

When :- expects_dialect(yap) is active, this library is searched before
the SWI-Prolog library directory.

Ultimately the libraries should become very close. This approach however
allows us to deal with incompatible  libraries   in  the same system and
appears a perfect short-term work-around for incompatible libraries.
