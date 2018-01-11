The test case here is weird enough to warrant an explanation. What we
_really_ want to test is whether building the lts-3.12 snapshot's
semigroupoids package with rev-1 works. See
https://github.com/fpco/stackage/issues/3185. However, that test
requires that we use an older GHC, and as Manny commented:

> Having integration tests with old resolvers will cause them to fail
> on Linux distributions with GCC with PIE enabled by default (which
> is the latest versions of most distributions now), since older GHC
> versions don't support it. I'm not sure what we should do about
> this, since it obviously does make sense to be able to test against
> old snapshots sometimes.

So I'm instead testing a totally different case here which repros the
same issue. If we use a custom snapshot with incompatible `stm` and
`async` versions, we want Stack to trust the build plan and allow a
`--dry-run` to succeed. But if we do this via `extra-deps`, we want it
to fail.
