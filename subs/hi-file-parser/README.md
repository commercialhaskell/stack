# hi-file-parser

Provide data types and functions for parsing the binary `.hi` files produced by
GHC. Intended to support multiple versions of GHC, so that tooling can:

* Support multiple versions of GHC
* Avoid linking against the `ghc` library
* Not need to use `ghc`'s textual dump file format.

Note that this code was written for Stack's usage initially, though it is
intended to be general purpose.
