# Changelog for pantry


## Unreleased changes

**Changes since v0.1.1.0**

Release notes:

Major changes:

Behavior changes:

Other enhancements:

Bug fixes:


## v0.1.1.0

**Changes since 0.1.0.0**

Bug fixes:

* Fix to allow dependencies on specific versions of local git repositories. See
  [#4862](https://github.com/commercialhaskell/stack/pull/4862)

Behavior changes:

* By default, do not perform expiry checks in Hackage Security. See

  [#4928](https://github.com/commercialhaskell/stack/issues/4928).

Other changes:

* Rename `pantry-tmp` package back to `pantry`, now that we have gained
  maintainership (which had been used by someone else for a candidate-only test
  that made it look like the name was free but prevented uploading a real
  package).


## 0.1.0.0

* Initial release
