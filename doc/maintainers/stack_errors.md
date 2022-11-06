<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

## Stack's errors

In connection with considering Stack's support of the
[Haskell Error Index](https://errors.haskell.org/) initiative, this page seeks
to take stock of the errors that Stack itself can raise, by reference to the
`master` branch of the Stack repository. Last updated: 2022-11-06.

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

    -   `Stack.Build.ConstructPlan.NotOnlyLocal`

        ~~~haskell
        = NotOnlyLocal [PackageName] [Text]
        ~~~

    -   `Stack.BuildPlan.BuildPlanException`

        ~~~haskell
        = UnknownPackages (Path Abs File) (Map PackageName (Maybe Version, Set PackageName)) (Map PackageName (Set PackageIdentifier))
        | SnapshotNotFound SnapName
        | NeitherCompilerOrResolverSpecified Text
        | DuplicatePackagesBug
        ~~~

    -   `Stack.Clean.CleanException`

        ~~~haskell
        = NonLocalPackages [PackageName]
        | DeletionFailures [(Path Abs Dir, SomeException)]
        ~~~

    -   `Stack.Config.Docker.StackDockerConfig.Execeiption`

        ~~~haskell
        = ResolverNotSupportedException (Maybe Project) (Maybe AbstractResolver)
        ~~~

    -   `Stack.Config.Nix.ConfigNixException`

        ~~~haskell
        = NixCannotUseShellFileAndPackagesException
        | GHCMajorVersionUnspecified
        | OnlyGHCSupported
        ~~~

    -   `Stack.ConfigCmd.ConfigCmdException`

        ~~~haskell
        = NoProjectConfigAvailable
        ~~~

    -   `Stack.Coverage.CoverageException`

        ~~~haskell
        = NonTestSuiteTarget PackageName
        | NoTargetsOrTixSpecified
        | NotLocalPackage PackageName
        ~~~

    -   `Stack.Dot.DotException`

        ~~~haskell
        = DependencyNotFoundBug GhcPkgId
        | PackageNotFoundBug PackageName
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

    -   `Stack.Init.InitException`

        ~~~haskell
        = ConfigFileAlreadyExists FilePath
        | SnapshotDownloadFailure SomeException
        | NoPackagesToIgnore
        | PackagesToIgnoreBug
        | PackageNameInvalid [FilePath]
        ~~~

    -   `Stack.List.ListException`

        ~~~haskell
        = CouldNotParsePackageSelectors [String]
        ~~~

    -   `Stack.Ls.LsException` *

        ~~~haskell
        = ParseFailure [Value]
        ~~~

    -   `Stack.Lock.LockException`

        ~~~haskell
        = WritingLockFileError (Path Abs File) Locked
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

    -   `Stack.PackageDump.PackageDumpException`

        ~~~haskell
        = MissingSingleField Text (Map Text [Line])
        | Couldn'tParseField Text [Line]
        ~~~

    -   `Stack.Runners.RunnersException`

        ~~~haskell
        = CommandInvalid
        | DockerAndNixInvalid
        | NixWithinDockerInvalid
        | DockerWithinNixInvalid
        ~~~

    -   `Stack.SDist.CheckException`

        ~~~haskell
        = CheckException (NonEmpty PackageCheck)
        ~~~

    -   `Stack.Script.ScriptException`

        ~~~haskell
        = MutableDependenciesForScript [PackageName]
        | AmbiguousModuleName ModuleName [PackageName]
        | ArgumentsWithNoRunInvalid
        | NoRunWithoutCompilationInvalid
        ~~~

    -   `Stack.Setup.PerformPathCheckingException`

        ~~~haskell
        = ProcessExited ExitCode String [String]
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
        | MSYS2NotFound Text
        | UnwantedCompilerVersion
        | UnwantedArchitecture
        | SandboxedCompilerNotFound
        | CompilerNotFound [String]
        | GHCInfoNotValidUTF8 UnicodeException
        | GHCInfoNotListOfPairs
        | GHCInfoMissingGlobalPackageDB
        | GHCInfoMissingTargetPlatform
        | GHCInfoTargetPlatformInvalid String
        | CabalNotFound (Path Abs File)
        | HadrianScriptNotFound
        | URLInvalid String
        | UnknownArchiveExtension String
        | Unsupported7z
        | TarballInvalid String
        | TarballFileInvalid String (Path Abs File)
        | UnknownArchiveStructure (Path Abs File)
        | StackReleaseInfoNotFound String
        | StackBinaryArchiveNotFound [String]
        | WorkingDirectoryInvalid
        | HadrianBindistNotFound
        | DownloadAndInstallCompilerError
        | StackBinaryArchiveZipUnsupported
        | StackBinaryArchiveUnsupported Text
        | StackBinaryNotInArchive String Text
        | FileTypeInArchiveInvalid Entry Text
        | BinaryUpgradeOnOSUnsupported OS
        | BinaryUpgradeOnArchUnsupported Arch
        | ExistingMSYS2NotDeleted (Path Abs Dir) IOException
        ~~~

    -   `Stack.Storage.User.StorageUserException`

        ~~~haskell
        = CompilerFileMetadataMismatch
        | GlobalPackageCacheFileMetadataMismatch
        | GlobalDumpParseFailure
        | CompilerCacheArchitectureInvalid Text
        ~~~

    -   `Stack.Unpack.UnpackException`

        ~~~haskell
        = UnpackDirectoryAlreadyExists (Set (Path Abs Dir))
        | CouldNotParsePackageSelectors [String]
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
        | TaskCycleBug PackageIdentifier
        | PackageIdMissingBug PackageIdentifier
        | AllInOneBuildBug
        | MulipleResultsBug PackageName [DumpPackage]
        | TemplateHaskellNotFoundBug
        | HaddockIndexNotFound
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
        | CabalFileNameParseFail FilePath
        | CabalFileNameInvalidPackageName FilePath
        | ComponentNotParsedBug
        ~~~

    -   `Stack.Types.Resolver.BuildPlanTypesException`

        ~~~haskell
        = ParseResolverException Text
        | FilepathInDownloadedSnapshot Text
        ~~~

    -   `Stack.Types.TemplateName.TypesTemplateNameException`

        ~~~haskell
        = DefaultTemplateNameNotParsedBug String
        ~~~

    -   `Stack.Upgrade.UpgradeException`

        ~~~haskell
        = NeitherBinaryOrSourceSpecified
        | ExecutableFailure
        | CommitsNotFound String String
        | StackInPackageIndexNotFound
        | VersionWithNoRevision
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

*   `Stack.Build.Execute.singleBuild`: catches exceptions in `cabal ...`

    `throwM`

*   `Stack.Build.Source.getFileDigestMaybe`: catches exceptions in
    `liftM Just . withSourceFile fp $ getDigest`

    `throwM`

*   `Stack.Config.configFromConfigMonoid`:

    ~~~text
    Stack's 'programs' path contains a space character and has no alternative
    short ('8 dot 3') name. This will cause problems with packages that use the
    GNU project's 'configure' shell script. Use the 'local-programs-path'
    configuration option to specify an alternative path. The current path is:
    <path>
    ~~~

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

*   `Stack.Lock.loadYamlThrow`:

    `Data.Yaml.AesonException`

    `throwIO`

*   `Stack.Package.resolveGlobFiles`:

    `Control.Exception.Base.IOException`

    `throwIO`

*   `Stack.Runners.withConfig`:

    ~~~text
    Error when running shouldUpgradeCheck: <exception>
    ~~~

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

*   `Stack.Setup.downloadStackExe`: catches exceptions from `performPathChecking`

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
