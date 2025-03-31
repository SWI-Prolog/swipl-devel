\section{SWI-Prolog and 32-bit machines}
\label{sec:32bits}

\index{bits, 32/64}%
Most today's platforms are native 64 bit and 64 bit applications are
to be preferred.  The current version of SWI-Prolog primarily targets
64 bit platforms.  32-bit platforms are still supported as they are
used on embedded devices and the WASM (Web Assembly, see
\secref{wasm-version}) still has poor support for 64 bits.

While the (currently) stable 9.2 series still has a real 32 bit
version were Prolog data structures are based on 32 bit \jargon{word}
units, the 9.3 development series represents all Prolog data as 64 bit
units, regardless of the hardware's pointer size.  This provides
better uniformity and avoids the 128Mb stack limit of the 32-bit 9.2
series.

Choices in the data representation such as the placement and number of
\jargon{tag} bits are still based on 32-bit units.  This is expected
to change in due time, simplifying the code and improving performance.
