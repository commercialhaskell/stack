  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 15. In conclusion

Stack is not the only tool available for building Haskell code. If you are
happy building with other tools, you may not need Stack. If you are experiencing
problems with other tools, give Stack a try.

If you are a new user who has no experience with other tools, we recommend
Stack. It aims to be easy to use and its defaults match modern best practices in
Haskell development.

Other key features of Stack include:

<div class="grid cards" markdown>

-   __Sandboxing__

    ---

    A 'sandbox' is a development environment that is isolated from other parts
    of the system. The concept of sandboxing is built into Stack.

-   __Snapshots__

    ---

    A snapshot specifies a GHC versions and a set of package versions that work
    well together. Stack uses snapshots to define precisely the set of package
    versions available for a project.

-   __Reproducibility__

    ---

    Stack goes to great lengths to ensure that `stack build` today does the
    same thing tomorrow. Changing the build plan is always an explicit decision.

-   __Building dependencies__

    ---

    Stack automatically builds dependencies.

</div>
