<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Version scheme

A Stack package or executable may have a version with three or four components:
`X.Y.Z` or `X.Y.Z.A`.

## Development or stable versions

* Versions with an _even_ `Y` component are development versions (the `master`
  branch)

* Versions with an _odd_ `Y` component are stable versions (the `stable` branch,
  or in a `rc/vX.Y` release candidate branch for not-yet-released versions)

## Unreleased or released versions

* Versions with an _even_ `Z` component are unreleased versions (including
  release candidates)

* Versions with an _odd_ `Z` component are released versions

* Except for the `release` branch, all branches must have an even `Z` component

* Except for the `release`, `stable` and `rc/vX.Y` release candidate branches,
  all branches will have a `0` `Z` component

## Use of a fourth component

* Release candidate binaries will be released with an odd `A` component

* Hackage-only dependency compatibility patch releases add a `A` component
  (e.g. `v2.5.1.1`, in the `release` branch)

* Pre-release unstable binaries will be released with the date as the `A`
  component (e.g. `3.6.0.20241228`)

## Examples

* `3.5.0.0`: `v3.5.*` series pre-release branch (`rc/v3.5` branch)

* `3.5.0.1`: first release candidate for first release of `v3.5.*` series
  (`rc/v3.5` branch)

* `3.5.0.2`: continuing development on pre-release branch

* `3.5.0.3`: second release candidate for first release of `v3.5.*` series
  (`rc/v3.5` branch)

* `3.5.1`: first release of the `3.5.*` series (`release` branch)

* `3.5.2.1`: first release candidate for second release of `3.5.*` series
  (`rc/v3.5` branch)

* `3.5.3`: second release of `3.5.*` series (`release` branch)

* `3.5.3.1`: first Hackage-only patch of `3.5.3` (`release` branch)

* `3.5.3.2`: second Hackage-only patch of `3.5.3` (`release` branch)

* `3.6.0`: unstable development code (`master` branch)

* `3.6.0.20241228`: pre-release snapshot of unstable version (`master` branch)
