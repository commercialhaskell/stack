Stack Contribution Notes 

Goal Maximize the amount of binary caching Stack is able to do.

We have both immutable and mutable package sources. 

Local file paths are mutable. We first state than anything mutable, and anything that depends on a mutable package source, cannot be cached. 
Immutable package sources, where the package contents come from Hackage, some archive, or a Git/Mercurial repository, can be cached.

We keep snapshot database and local database logic the same. But now, instead of rebuilding packages in local databases, or having special logic for the precompiled cache, we have a simple algorithm we follow every time we build an immutable package:
* Check if it exists in ~/.stack/immutable
* If not, build it
* Copy the registration from the ~/.stack/immutable package database to the appropriate snapshot or local database

Snoyman’s Explanation of Code/TODO List

* In the ConstructPlan module, we determine which packages are going to be used, and of those which are already installed, and which need to be built

* We're now going to also track for each of these packages whether it's an "all immutable" package, meaning that it and its dependencies come from an immutable package source

* I didn't do it yet, but we need to check for the already installed packages whether they exist under ~/.stack or not. If they don't, we should rebuild them, since we don't want a package in some other directory being depended on and then getting moved/deleted.

* We need to replace the usage of precompiled cache functions in Stack.Build.Execute with functions that perform the new caching. This should basically come down to:
    * If it's not "all immutable", don't do anything at all
    * If it's all immutable, and it hasn't been built yet, build it and register in the correct location in ~/.stack (which hasn't been set up yet, your call on exactly what the directory structure looks like)
    * Either it was previously built, or we just built it. Either way: reregister it from the cache location into the snapshot/local database. You can reuse the same approach as the precompiled cache.


Questions:

How does Stack determine right now which packages need to be recompiled? The way I understand it is __some__ packages are stored in the cache. Before Stack recompiles any package it checks the cache to see if it’s already been compiled. Anything that has been changed (different version of a package, new dependency, etc.) in the project is recompiled (and the immutable dependencies are added to the cache).

Build.hs has configure options for how to build projects. What are the configure options and what, if anything, needs to added for immutable packages? My guess is nothing. If we determine it’s immutable we don’t need to build it, which happens in ConstructPlan, and those packages are added to the cache.

It seems like the majority of the changes should be made in ConstructPlan.hs, specifically `addFinal`, `addDep`, `installPackage`, `installPackageGivenDeps`, and `addPackageDeps` (I think). Based on my digging around it seems like 1 change would happen in Build.hs and a couple of changes in the Packages.hs file, but maybe not for this particular issue. I want to make sure I stay in the scope of this problem.

What is the package index in "look up in the package index and see if there's a recommendation available" and what does that mean in terms of adding code to accomplish seeing if there’s a recommendation? The first part of the question might lead me to answer the second part. 


In the ConstructPlan module, we determine which packages are going to be used, and of those which are already installed, and which need to be built
ConstructPlan.hs determines which packages are immutable.

Cache.hs determines what is stored in cache and how it is created. 

What is pdr?


Thoughts:

I think I need to add code/look at:

Execute.hs
getPrecompiled

Cache.hs
read writePrecompiledCache and readPrecompiledCache. Done.

ConstructPlan.hs
line 368 packageIsImmutable — check existence of library by file path.
line 424 check assumption?
line 481 TODO written

Make sure the installed map only contains packages in the cache.

line 550 look at pdr more in depth
line 567 maybe we want to look at piiLocation ps and see if that tells us if cache is in ~/.stack and if not what that type is and maybe if we can add something there
line 613 pdr data constructors
line 643 addPackageDeps function.

Build.hs 
line 453 just read this after Construct.

ConfigureOpts

readPrecompiledCache
-- Since commit ed9ccc08f327bad68dd2d09a1851ce0d055c0422
-- pcLibrary paths are stored as relative to the stack
-- root. Therefore, we need to prepend the stack root when
-- checking that the file exists. For the older cached paths, the
-- file will contain an absolute path, which will make `stackRoot
-- </>` a no-op.

let mkAbs' = (toFilePath stackRoot FP.</>)

-- | Where the package's source is located: local directory or package index
data PackageSource
  = PSFiles LocalPackage InstallLocation
  -- ^ Package which exist on the filesystem (as opposed to an index tarball)
  | PSIndex InstallLocation (Map FlagName Bool) [Text] PackageIdentifierRevision
  -- ^ Package which is in an index, and the files do not exist on the
  -- filesystem yet.

PackageIdentifierRevision probably determines whether thing is recompiled if it’s an immutable.

Build 481 ??????

is ctx on line 395 in ConstructPlan the cache plan? I think so.

What is ADRFound
data AddDepRes
    = ADRToInstall Task
    | ADRFound InstallLocation AllImmutable Installed

addDep

:l test/integration/tests/3860-better-cache/

:l test/integration/tests/3860-better-cache/Main.hs te
st/integration/lib/StackTest.hs

What are the semantics of how we handle PLFilePaths

λ> hspec $ test "runghc" [("STACK_EXE", "stack")] currdir "~/.stack" "Desktop/better-cache/home" "Desktop/better-cache/stack" "3431-precompiled-works"

stack ghci stack:test:stack-integration-test

hspec $ test "runghc" [("STACK_EXE", "/usr/local/bin/stack"), (
"HOME", "/Users/jhaigh")] currdir "/Users/jhaigh/.stack" "/Users/j
haigh/Desktop/better-cache/home" "/Users/jhaigh/Desktop/better-cac
he/stack" "3431-precompiled-works"

:main -m 3860-better-cache

hspec $ test "runghc" [("STACK_EXE", "/usr/local/bin/stack"), (
"HOME", "/Users/jhaigh")] currdir "/Users/jhaigh/.stack" "/Users/j
haigh/Desktop/better-cache/home" "/Users/jhaigh/Desktop/better-cac
he/stack" "3860-better-cache"


From inside the files subdir of an integration test:

$ stack runghc -- -i../../../lib ../Main.hs




