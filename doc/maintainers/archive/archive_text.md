Stack provides an integration with [Nix](http://nixos.org/nix), providing you
with the same two benefits as the first Docker integration discussed above:

* more reproducible builds, since fixed versions of any system libraries and
  commands required to build the project are automatically built using Nix and
  managed locally per-project. These system packages never conflict with any
  existing versions of these libraries on your system. That they are managed
  locally to the project means that you don't need to alter your system in any
  way to build any odd project pulled from the Internet.
* implicit sharing of system packages between projects, so you don't have more
  copies on-disk than you need to.

When using the Nix integration, Stack downloads and builds Haskell dependencies
as usual, but resorts on Nix to provide non-Haskell dependencies that exist in
the Nixpkgs.

Both Docker and Nix are methods to *isolate* builds and thereby make them more
reproducible. They just differ in the means of achieving this isolation. Nix
provides slightly weaker isolation guarantees than Docker, but is more
lightweight and more portable (Linux and macOS mainly, but also Windows). For
more on Nix, its command-line interface and its package description language,
read the [Nix manual](http://nixos.org/nix/manual). But keep in mind that the
point of Stack's support is to obviate the need to write any Nix code in the
common case or even to learn how to use the Nix tools (they're called under the
hood).

For more information, see the [Nix-integration](../../nix_integration.md)
documentation.
