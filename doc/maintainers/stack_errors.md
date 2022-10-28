<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

## Stack's errors

In connection with considering Stack's support of the
[Haskell Error Index](https://errors.haskell.org/) initiative, this page seeks
to take stock of the errors that Stack itself can raise, by reference to the
`master` branch of the Stack repository. Last updated: 2022-10-22.

*   `Main.main`: catches exceptions from action `commandLineHandler`.

    `ExitCode`

    `throwIO`

*   `Main.main`: catches exceptions from action `run`.

    ~~~text
    <exception>
    ~~~

    `exitWith` or `exitFailure`

    The following types are instances of `Control.Exception.Exception` and
    `Show`. Some data constructors have strict fields but that is not documented
    below:

    -   `Control.Concurrent.ExecuteException`

        ~~~haskell
        = InconsistentDependencies
        ~~~

    -   `Main.MainException`

        ~~~haskell
        = InvalidReExecVersion String String
        | InvalidPathForExec FilePath
        ~~~

    -   `Stack.Build.QueryException`

        ~~~haskell
        = SelectorNotFound [Text]
        | IndexOutOfRange [Text]
        | NoNumericSelector [Text]
        | CannotApplySelector Value [Text]
        ~~~

    -   `Stack.Build.CabalVersionException`

        ~~~haskell
        = AllowNewerNotSupported Version
        | CabalVersionNotSupported Version
        ~~~

        The `String` is a message.

    -   `Stack.BuildPlan.BuildPlanException`

        ~~~haskell
        = UnknownPackages (Path Abs File) (Map PackageName (Maybe Version, Set PackageName)) (Map PackageName (Set PackageIdentifier))
        | SnapshotNotFound SnapName
        | NeitherCompilerOrResolverSpecified Text
        ~~~

    -   `Stack.Clean.StackCleanException`

        ~~~haskell
        = NonLocalPackages [PackageName]
        ~~~

    -   `Stack.Coverage.CoverageException`

        ~~~haskell
        = NonTestSuiteTarget PackageName
        | NoTargetsOrTixSpecified
        | NotLocalPackage PackageName
        ~~~

    -   `Stack.Ghci.Ghci.Exception`

        ~~~haskell
        = InvalidPackageOption String
        | LoadingDuplicateModules
        | MissingFileTarget String
        | Can'tSpecifyFilesAndTargets
        | Can'tSpecifyFilesAndMainIs
        | GhciTargetParseException [Text]
        ~~~

    -   `Stack.List.ListException`

        ~~~haskell
        = CouldNotParsePackageSelectors [String]
        ~~~

    -   `Stack.Ls.LsException` *

        ~~~haskell
        = ParseFailure [Value]
        ~~~

    -   `Stack.New.NewException`

        ~~~haskell
        = FailedToLoadTemplate TemplateName FilePath
        | FailedToDownloadTemplate TemplateName VerifiedDownloadException
        | AlreadyExists (Path Abs Dir)
        | MissingParameters PackageName TemplateName (Set String) (Path Abs File)
        | InvalidTemplate TemplateName String
        | AttemptedOverwrites [Path Abs File]
        | FailedToDownloadTemplatesHelp HttpException
        | BadTemplatesHelpEncoding String UnicodeException
        | Can'tUseWiredInName PackageName
        ~~~

    -   `Stack.Nix.StackNixException`

        ~~~haskell
        = CannotDetermineProjectRoot
        ~~~

    -   `Stack.Package.CabalFileNameParseFail`

        ~~~haskell
        = CabalFileNameParseFail FilePath
        | CabalFileNameInvalidPackageName FilePath
        ~~~

    -   `Stack.PackageDump.PackageDumpException`

        ~~~haskell
        = MissingSingleField Text (Map Text [Line])
        | Couldn'tParseField Text [Line]
        ~~~

    -   `Stack.Script.StackScriptException`

        ~~~haskell
        = MutableDependenciesForScript [PackageName]
        | AmbiguousModuleName ModuleName [PackageName]
        ~~~

    -   `Stack.SDist.CheckException`

        ~~~haskell
        = CheckException (NonEmpty PackageCheck)
        ~~~

    -   `Stack.Setup.SetupException`

        ~~~haskell
        = UnsupportedSetupCombo OS Arch
        | MissingDependencies [String]
        | UnknownCompilerVersion (Set Text) WantedCompiler (Set ActualCompiler)
        | UnknownOSKey Text
        | GHCSanityCheckCompileFailed SomeException (Path Abs File)
        | WantedMustBeGHC
        | RequireCustomGHCVariant
        | ProblemWhileDecompressing (Path Abs File)
        | SetupInfoMissingSevenz
        | DockerStackExeNotFound Version Text
        | UnsupportedSetupConfiguration
        | InvalidGhcAt (Path Abs File) SomeException
        ~~~

    -   `Stack.Unpack.UnpackException`

        ~~~haskell
        = UnpackDirectoryAlreadyExists (Set (Path Abs Dir))
        | CouldNotParsePackageSelectors [String]
        ~~~

    -   `Stack.Build.ConstructPlan.NotOnlyLocal`

        ~~~haskell
        = NotOnlyLocal [PackageName] [Text]
        ~~~

    -   `Stack.Config.Docker.StackDockerConfig.Execeiption`

        ~~~haskell
        = ResolverNotSupportedException (Maybe Project) (Maybe AbstractResolver)
        ~~~

    -   `Stack.Config.Nix.StackNixException`

        ~~~haskell
        = NixCannotUseShellFileAndPackagesException
        ~~~

    -   `Stack.Types.Build.StackBuildException`

        ~~~haskell
        = Couldn'tFindPkgId PackageName
        | CompilerVersionMismatch (Maybe (ActualCompiler, Arch)) (WantedCompiler, Arch) GHCVariant CompilerBuild VersionCheck (Maybe (Path Abs File)) Text
        | Couldn'tParseTargets [Text]
        | UnknownTargets (Set PackageName) (Map PackageName Version) (Path Abs File)
        | TestSuiteFailure PackageIdentifier (Map Text (Maybe ExitCode)) (Maybe (Path Abs File)) ByteString
        | TestSuiteTypeUnsupported TestSuiteInterface
        | ConstructPlanFailed String
        | CabalExitedUnsuccessfully ExitCode PackageIdentifier (Path Abs File) [String] (Maybe (Path Abs File)) [Text]
        | SetupHsBuildFailure ExitCode (Maybe PackageIdentifier) (Path Abs File) [String] (Maybe (Path Abs File)) [Text]
        | ExecutionFailure [SomeException]
        | LocalPackageDoesn'tMatchTarget PackageName Version Version
        | NoSetupHsFound (Path Abs Dir)
        | InvalidFlagSpecification (Set UnusedFlags)
        | InvalidGhcOptionsSpecification [PackageName]
        | TargetParseException [Text]
        | SomeTargetsNotBuildable [(PackageName, NamedComponent)]
        | TestSuiteExeMissing Bool String String String
        | CabalCopyFailed Bool String
        | LocalPackagesPresent [PackageIdentifier]
        | CouldNotLockDistDir (Path Abs File)
        ~~~

    -   `Stack.Types.Compiler.CompilerException`

        ~~~haskell
        = GhcjsNotSupported
        | PantryException PantryException
        ~~~

    -   `Stack.Types.Config.ConfigException`

        ~~~haskell
        = ParseConfigFileException (Path Abs File) ParseException
        | ParseCustomSnapshotException Text ParseException
        | NoProjectConfigFound (Path Abs Dir) (Maybe Text)
        | UnexpectedArchiveContents [Path Abs Dir] [Path Abs File]
        | UnableToExtractArchive Text (Path Abs File)
        | BadStackVersionException VersionRange
        | NoMatchingSnapshot (NonEmpty SnapName)
        | ResolverMismatch RawSnapshotLocation String
        | ResolverPartial RawSnapshotLocation String
        | NoSuchDirectory FilePath
        | ParseGHCVariantException String
        | BadStackRoot (Path Abs Dir)
        | Won'tCreateStackRootInDirectoryOwnedByDifferentUser (Path Abs Dir) (Path Abs Dir)
        | UserDoesn'tOwnDirectory (Path Abs Dir)
        | ManualGHCVariantSettingsAreIncompatibleWithSystemGHC
        | NixRequiresSystemGhc
        | NoResolverWhenUsingNoProject
        | DuplicateLocalPackageNames [(PackageName, [PackageLocation])]
        | NoLTSWithMajorVersion Int
        | NoLTSFound
        | MultiplePackageIndices [PackageIndexConfig]
        ~~~

    -   `Stack.Types.Config.ParseAbsolutePathException`

        ~~~haskell
        = ParseAbsolutePathException String String
        ~~~

    -   `Stack.Types.Docker.DockerException`

        ~~~haskell
        = DockerMustBeEnabledException
        | OnlyOnHostException
        | InspectFailedException String
        | NotPulledException String
        | InvalidImagesOutputException String
        | InvalidPSOutputException String
        | InvalidInspectOutputException String
        | PullFailedException String
        | DockerTooOldException Version Version
        | DockerVersionProhibitedException [Version] Version
        | BadDockerVersionException VersionRange Version
        | InvalidVersionOutputException
        | HostStackTooOldException Version (Maybe Version)
        | ContainerStackTooOldException Version Version
        | CannotDetermineProjectRootException
        | DockerNotInstalledException
        | UnsupportedStackExeHostPlatformException
        | DockerStackExeParseException String
        ~~~

    -   `Stack.Types.GhcPkgId.GhcPkgIdParseFail`

        ~~~haskell
        = GhcPkgIdParseFail Text
        ~~~

    -   `Stack.Types.Package.PackageException`

        ~~~haskell
        = PackageInvalidCabalFile (Either PackageIdentifierRevision (Path Abs File)) (Maybe Version) [PError] [PWarning]
        | MismatchedCabalIdentifier PackageIdentifierRevision PackageIdentifier
        ~~~

    -   `Stack.Types.Resolver.BuildPlanTypesException`

        ~~~haskell
        = ParseResolverException Text
        | FilepathInDownloadedSnapshot Text
        ~~~

    -   `System.Process.Pager.PagerException`

        ~~~haskell
        = PagerExitFailure CmdSpec Int
        ~~~

    \* The instance of `Show` is derived.

    Stack also makes use of `UnlifIO.Exception.throwString`, which throws an
    exception of type `UnliftIO.Exception.StringException`:

    ~~~haskell
    = StringException String CallStack
    ~~~

*   `Main.buildCmd`:

    ~~~text
    Error: When building with Stack, you should not use the -prof GHC option
    Instead, please use --library-profiling and --executable-profiling"
    See: https://github.com/commercialhaskell/stack/issues/1015
    ~~~

    `exitFailure`

*   `Main.upgradeCmd`:

    ~~~text
    You cannot use the --resolver option with the upgrade command
    ~~~

    `exitFailure`

*   `Main.execCmd`:

    ~~~text
    Could not find package id of package <name>
    ~~~

    `exitFailure`

*   `Main.execCmd`:

    ~~~text
    No executables found.
    ~~~~

    `exitFailure`

*   `Options.Applicative.Builder.Extra.enableDisableFlagsNoDefault`:

    ~~~text
    enableDisableFlagsNoDefault.last
    ~~~

    `StringException`

    `impureThrow`

*   `Stack.Build.ConstructPlan.stripNonDeps`:

    ~~~text
    Unexpected: task cycle for <package>
    ~~~

    `error`

*   `Stack.Build.Execute.getConfigCache`:

    ~~~text
    singleBuild: invariant violated, missing package ID missing: <id>
    ~~~

    `error`

*   `Stack.Build.Execute.singleBuild`:

    ~~~text
    Invariant violated: cannot have an all-in-one build that also has a final
    build step.
    ~~~

    `error`

*   `Stack.Build.Execute.singleBuild`:

    ~~~text
    singleBuild: invariant violated: multiple results when describing installed package (<name>, <description>)
    ~~~

    `error`

*   `Stack.Build.Execute.singleBuild`: catches exceptions in `cabal ...`

    `throwM`

*   `Stack.Build.Source.getFileDigestMaybe`: catches exceptions in
    `liftM Just . withSourceFile fp $ getDigest`

    `throwM`

*   `Stack.Build.Execute.singleTest`:

    ~~~text
    template-haskell is a wired-in GHC boot library but it wasn't found
    ~~~

    `error`

*   `Stack.Build.Haddock.openHaddocksInBrowser`:

    ~~~text
    No local or snapshot doc index found to open.
    ~~~

    `throwString`

*   `Stack.BuildPlan.checkBundleBuildPlan`:

    ~~~text
    Bug: Duplicate packages are not expected here
    ~~~

    `error`

*   `Stack.Clean.clean`:

    ~~~text
    Exception while recursively deleting <dir>"
    <execption>
    Perhaps you do not have permission to delete these files or they are in use?
    ~~~

    `exitFailure`

*   `Stack.Config.configFromConfigMonoid`:

    ~~~text
    Stack's 'programs' path contains a space character and has no alternative
    short ('8 dot 3') name. This will cause problems with packages that use the
    GNU project's 'configure' shell script. Use the 'local-programs-path'
    configuration option to specify an alternative path. The current path is:
    <path>
    ~~~

*   `Stack.ConfigCmd.cfgCmdSet`:

    ~~~text
    config command used when no project configuration available
    ~~~

    `throwString`

*   `Stack.Config.Nix.nixCompiler`:

    ~~~text
    GHC major version not specified
    ~~~

    `StringException`

*   `Stack.Config.Nix.nixCompiler`:

    ~~~text
    Only GHC is supported by stack --nix
    ~~~

    `StringException`

*   `Stack.Config.Nix.nixCompiler`:

    ~~~text
    Only GHC is supported by stack --nix
    ~~~

    `StringException`

*   `Stack.Config.Nix.nixCompilerVersion`:

    ~~~text
    GHC major version not specified
    ~~~

    `StringException`

*   `Stack.Config.Nix.nixCompilerVersion`:

    ~~~text
    Only GHC is supported by stack --nix
    ~~~

    `StringException`

*   `Stack.Config.Nix.nixCompilerVersion`:

    ~~~text
    Only GHC is supported by stack --nix
    ~~~

    `StringException`

*   `Stack.Constants.wiredInPackages`:

    ~~~text
    Parse error in wiredInPackages
    ~~~

    `error`

*   `Stack.Coverage.generateHpcReport`: catches exceptions from
    `findPackageFieldForBuiltPackage`

*   `Stack.Coverage.generateHpcReportInternal`:

    ~~~text
    Didn't find .tix for <report> - expected to find it at <path>.
    ~~~

*   `Stack.Coverage.generateHpcReportInternal`:

    ~~~text
    <exception>
    Error occurred while producing <report>"
    ~~~

*   `Stack.Coverage.generateHpcReportInternal`:

    ~~~text
    Error: The <report> did not consider any code. One possible cause of this is
    if your test-suite builds the library code (see Stack issue #1008). It may
    also indicate a bug in Stack or the hpc program. Please report this issue if
    you think your coverage report should have meaningful results.
    ~~~

*   `Stack.Coverage.readTixOrlog`:

    ~~~text
    Error while reading tix: <exeception>
    ~~~

*   `Stack.Coverage.readTixOrlog`:

    ~~~text
    Failed to read tix file <path>
    ~~~

*   `Stack.Coverage.updateTixFile`:

    ~~~text
    Failed to read <file>
    ~~~

*   `Stack.Dot.createDepLoader`:

    ~~~text
    Invariant violated: Expected to find <packageId> in global DB
    ~~~

    `error`

*   `Stack.Ghci.buildDepsAndInitialSteps`: catches exeception from
    `buildLocalTargets`

    ~~~text
    <exception>
    ~~~

*   `Stack.GhcPkg.createDatabase`:

    ~~~text
    Unable to create package database at <path>
    ~~~

*   `Stack.Hoogle.hoogleCmd`:

    ~~~text
    No Hoogle database. Not building one due to --no-setup
    ~~~

    `exitWith (ExitFailure (-1))`

*   `Stack.Init.cabalPackagesCheck`:

    ~~~text
    Package name as defined in the Cabal file must match the Cabal file name.
    Please fix the following packages and try again:
    <packages>
    ~~~

    `error`

*   `Stack.Init.cabalPackagesCheck`: has the possibility of passing a message.
    Not currently used.

    ~~~text
    <message>
    ~~~

    `error`

*   `Stack.Init.getSnapshots'`:

    ~~~text
    Unable to download snapshot list, and therefore could not generate a
    stack.yaml file automatically
    This sometimes happens due to missing Certificate Authorities on your
    system. For more information, see:

        https://github.com/commercialhaskell/stack/issues/234"

    You can try again, or create your stack.yaml file by hand. See:"

        http://docs.haskellstack.org/en/stable/yaml_configuration/"

    Exception was: <exception>
    ~~~

    `throwString`

*   `Stack.Init.getWorkingResolverPlan`:

    ~~~text
    Bug: No packages to ignore
    ~~~

    `error`

*   `Stack.Init.getWorkingResolverPlan`:

    ~~~text
    getWorkingResolverPlan.head
    ~~~

    `error`

*   `Stack.Init.initProject`:

    ~~~text
    Error: Stack configuration file " <path>  exists, use '--force' to overwrite
    it.
    ~~~

    `throwString`

*   `Stack.Lock.loadYamlThrow`:

    `Data.Yaml.AesonException`

    `throwIO`

*   `Stack.Lock.lockCachedWanted`:

    ~~~text
    You indicated that Stack should error out on writing a lock file
    I just tried to write the following lock file contents to <path>
    <lock_file_contents>
    ~~~

    `exitFailure`

*   `Stack.Package.componentNameToDir`:

    ~~~text
    Invariant violated: component names should always parse as directory names
    ~~~

    `error`

*   `Stack.Package.resolveGlobFiles`:

    `Control.Exception.Base.IOException`

    `throwIO`

*   `Stack.Runners.reexec`:

    ~~~text
    Cannot use both Docker and Nix at the same time
    ~~~

    `throwString`

*   `Stack.Runners.reexec`:

    ~~~text
    Cannot use Nix from within a Docker container
    ~~~

    `throwString`

*   `Stack.Runners.reexec`:

    ~~~text
    Cannot use Docker from within a Nix shell
    ~~~

    `throwString`

*   `Stack.Runners.withConfig`:

    ~~~text
    Error when running shouldUpgradeCheck: <exception>
    ~~~

*   `Stack.Runners.withGlobalProject`:

    ~~~text
    Cannot use this command with options which override the stack.yaml location
    ~~~

    `throwString`

*   `Stack.Script.scriptCmd`:

    Error used because warnings are surpressed.

    ~~~text
    Ignoring override stack.yaml file for script command: <path>
    ~~~

*   `Stack.Script.scriptCmd`:

    Error used because warnings are surpressed.

    ~~~text
    Ignoring SYLGlobalProject for script command
    ~~~

*   `Stack.Script.scriptCmd`:

    ~~~text
    --no-run incompatible with arguments
    ~~~

    `throwString`

*   `Stack.Script.scriptCmd`:

    ~~~text
    --no-run requires either --compile or --optimize
    ~~~

    `throwString`

*   `Stack.SDist.getCabalLbs`:

    ~~~text
    getCabalLbs: cabalfp /= cabalfp': (<cabalfp>, <cabalfp'>)
    ~~~

    `error`

*   `Stack.SDist.getSDistTarball`:

    ~~~text
    Error building custom-setup dependencies: <exception>
    ~~~

*   `Stack.SDist.getSDistTarball`: catches exception from
    `Codec.Archive.Tar.Entry.toTarPath`

    ~~~text
    <exception>
    ~~~

    `throwString`

*   `Stack.Setup.buildGHCFromSource`:

    ~~~text
    Invalid working directory
    ~~~

    `error`

*   `Stack.Setup.buildGHCFromSource`:

    ~~~text
    No Hadrian build script found
    ~~~

    `throwString`

*   `Stack.Setup.buildGHCFromSource`:

    ~~~text
    Can't find hadrian generated bindist
    ~~~

    `error`

*   `Stack.Setup.downloadAndInstallCompiler`:

    ~~~text
    downloadAndInstallCompiler: shouldn't be reached with ghc-git
    ~~~

    `error`

*   `Stack.Setup.downloadFromInfo`:

    ~~~text
    Error: Unknown extension for url: <url>
    ~~~

    `throwString`

*   `Stack.Setup.downloadOrUseLocal`:

    ~~~text
    Error: `url` must be either an HTTP URL or a file path: <url>
    ~~~

    `throwString`

*   `Stack.Setup.downloadStackExe`:

    ~~~text
    Unable to find binary Stack archive for platforms: <platforms>
    ~~~

    `throwString`

*   `Stack.Setup.downloadStackExe`:

    ~~~text
    FIXME: Handle zip files
    ~~~

    `error`

*   `Stack.Setup.downloadStackExe`:

    ~~~text
    Unknown archive format for Stack archive: <url>
    ~~~

    `error`

*   `Stack.Setup.downloadStackExe`:

    ~~~text
    "Stack executable <exe>  not found in archive from <url>
    ~~~

    `error`

*   `Stack.Setup.downloadStackExe`: catches exceptions from `performPathChecking`

*   `Stack.Setup.downloadStackReleaseInfoGitHub`:

    ~~~text
    Could not get release information for Stack from: <url>
    ~~~

    `throwString`

*   `Stack.Setup.ensureCompiler`:

    ~~~text
    Not the compiler version we want
    ~~~

    `throwString`

*   `Stack.Setup.ensureCompiler`:

    ~~~text
    Not the architecture we want
    ~~~

    `throwString`

*   `Stack.Setup.ensureMsys`:

    ~~~text
    MSYS2 not found for <os>
    ~~~

    `throwString`

*   `Stack.Setup.ensureSandboxedCompiler`:

    ~~~text
    Looked for sandboxed compiler named one of: <names>
    Could not find it on the paths <paths>
    Could not find sandboxed compiler
    ~~~

    `throwString`

*   `Stack.Setup.expectSingleUnpackedDir`:

    ~~~text
    Expected a single directory within unpacked <file>
    ~~~

    `throwString`

*   `Stack.Setup.installGHCPosix`:

    ~~~text
    Don't know how to deal with .7z files on non-Windows
    ~~~

    `throwString`

*   `Stack.Setup.installGHCPosix`:

    ~~~text
    <exception>
    Error encountered while <step> GHC with
    <cmd> <args>
    <cmd> <args>
    run in <wd>

    The following directories may now contain files, but won't be used by Stack:
      - <temp_dir>
      - <dest_dir>

    For more information consider rerunning with --verbose flag
    ~~~

    `exitFailure`

*   `Stack.Setup.installMsys2Windows`:

    ~~~text
    Could not delete existing msys directory: <path>
    ~~~

    `throwM`

*   `Stack.Setup.pathsFromCompiler`:

    ~~~text
    Could not find any of: <paths>
    ~~~

    `throwString`

*   `Stack.Setup.pathsFromCompiler`:

    ~~~text
    GHC info is not valid UTF-8: <exception>
    ~~~

    `throwString`

*   `Stack.Setup.pathsFromCompiler`:

    ~~~text
    GHC info does not parse as a list of pairs
    ~~~

    `throwString`

*   `Stack.Setup.pathsFromCompiler`:

    ~~~text
    Key 'Global Package DB' not found in GHC info
    ~~~

    `throwString`

*   `Stack.Setup.pathsFromCompiler`:

    ~~~text
    Key 'Target platform' not found in GHC info
    ~~~

    `throwString`

*   `Stack.Setup.pathsFromCompiler`:

    ~~~text
    Invalid target platform in GHC info: <platform>
    ~~~

    `throwString`

*   `Stack.Setup.pathsFromCompiler`:

    ~~~text
    Cabal library not found in global package database for <compiler>
    ~~~

    `throwString`

*   `Stack.Setup.performPathChecking`:

    ~~~text
    Process exited with <exitcode>: <cmd> <args>
    ~~~

    `error`

*   `Stack.Setup.preferredPlatforms`:

    ~~~text
    Binary upgrade not yet supported on OS: <os>
    ~~~

    `StringException`

    `throwM`

*   `Stack.Setup.preferredPlatforms`:

    ~~~text
    Binary upgrade not yet supported on arch: <arch>
    ~~~

    `StringException`

    `throwM`

*   `Stack.Setup.withUnpackedTarball7z`:

    ~~~text
    <name>  must be a tarball file
    ~~~

    `throwString`

*   `Stack.Setup.withUnpackedTarball7z`:

    ~~~text
    Invalid <name> filename: <file>
    ~~~

    `throwString`

*   `Stack.Storage.User.loadCompilerPaths`:

    ~~~text
    Compiler file metadata mismatch, ignoring cache
    ~~~

    `throwString`

*   `Stack.Storage.User.loadCompilerPaths`:

    ~~~text
    Global package cache file metadata mismatch, ignoring cache
    ~~~

    `throwString`

*   `Stack.Storage.User.loadCompilerPaths`:

    ~~~text
    Global dump did not parse correctly
    ~~~

    `throwString`

*   `Stack.Storage.User.loadCompilerPaths`:

    ~~~text
    Invalid arch: <arch>
    ~~~

    `throwString`

*   `Stack.Types.Build.showBuildError`:

    ~~~text
    Invariant violated: unexpected case in showBuildError
    ~~~

    `error`

*   `Stack.Types.TemplateName.defaultTemplateName`:

    ~~~text
    Bug in Stack codebase, cannot parse default template name: <name>
    ~~~

    `error`

*   `Stack.Upgrade.binaryUpgrade`:

    ~~~text
    Non-success exit code from running newly downloaded executable
    ~~~

    `throwString`

*   `Stack.Upgrade.sourceUpgrade`:

    ~~~text
    No commits found for branch <branch> on repo <repo>
    ~~~

    `throwString`

*   `Stack.Upgrade.sourceUpgrade`:

    ~~~text
    No Stack version found in package indices
    ~~~

    `throwString`

*   `Stack.Upgrade.sourceUpgrade`:

    ~~~text
    Latest version with no revision
    ~~~

    `throwString`

*   `Stack.Upgrade.upgrade`:

    ~~~text
    You must allow either binary or source upgrade paths
    ~~~

    `throwString`

*   `Stack.Upload.uploadBytes`:

    ~~~text
    authentication failure
    Authentication failure uploading to server
    ~~~

    `throwString`

*   `Stack.Upload.uploadBytes`:

    ~~~text
    forbidden upload
    Usually means: you've already uploaded this package/version combination
    Ignoring error and continuing, full message from Hackage below:
    <Hackage_message>
    ~~~

*   `Stack.Upload.uploadBytes`:

    ~~~text
    service unavailable
    This error some times gets sent even though the upload succeeded
    Check on Hackage to see if your package is present
    <Hackage_message>
    ~~~

*   `Stack.Upload.uploadBytes`:

    ~~~text
    unhandled status code: <code>
    <Hackage_message>
    Upload failed on <name>
    ~~~

    `throwString`
