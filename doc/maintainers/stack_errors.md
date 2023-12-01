<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

## Stack's errors

In connection with considering Stack's support of the
[Haskell Error Index](https://errors.haskell.org/) initiative, this page seeks
to take stock of the errors that Stack itself can raise, by reference to the
`master` branch of the Stack repository. Last updated: 2023-10-21.

*   `Stack.main`: catches exceptions from action `commandLineHandler`.

    -   `ExitCode`
    -   `throwIO`

*   `Stack.main`: catches exceptions from action `run`:

    -   `ExitCode` (`exitWith`)
    -   `PrettyException` (`exitFailure`)
    -   `SomeException` (`exitFailure`)

    The following types are instances of `Control.Exception.Exception` and
    `Show`. Some are instances of `Stack.Prelude.PrettyException`. Some data
    constructors have strict fields but that is not documented below:

    -   `Control.Concurrent.ExecuteException`

        ~~~haskell
        [S-2816] = InconsistentDependenciesBug
        ~~~

    -   `GHC.GHC.Utils.GhcPkg.Main.Compat`

        ~~~haskell
        [S-6512] = CannotParse String String String
        [S-3384] | CannotOpenDBForModification FilePath IOException
        [S-1430] | SingleFileDBUnsupported FilePath
        [S-5996] | ParsePackageInfoExceptions String
        [S-3189] | CannotFindPackage PackageArg (Maybe FilePath)
        [S-9323] | CannotParseRelFileBug String
        [S-7651] | CannotParseDirectoryWithDBug String
        ~~~

    -   `Options.Applicative.Builder.Extra.OptionsApplicativeExtraException`

        ~~~haskell
        [S-2797] = FlagNotFoundBug
        ~~~

    -   `Stack.Build.CabalVersionPrettyException`

        ~~~haskell
        [S-5973] = CabalVersionNotSupported Version
        ~~~

    -   `Stack.Build.ConstructPlan.NotOnlyLocal`

        ~~~haskell
        [S-1727] = NotOnlyLocal [PackageName] [Text]
        ~~~

    -   `Stack.BuildPlan.BuildPlanException`

        ~~~haskell
        [S-7571] = UnknownPackages (Path Abs File) (Map PackageName (Maybe Version, Set PackageName)) (Map PackageName (Set PackageIdentifier))
        [S-2045] | SnapshotNotFound SnapName
        [S-8559] | NeitherCompilerOrResolverSpecified Text
        [S-5743] | DuplicatePackagesBug
        ~~~

    -   `Stack.CLI.CliPrettyException`

        ~~~haskell
        [S-4639] = NoArgumentsBug
        ~~~

    -   `Stack.Clean.CleanException`

        ~~~haskell
        [S-9463] = NonLocalPackages [PackageName]
        [S-6321] | DeletionFailures [(Path Abs Dir, SomeException)]
        ~~~

    -   `Stack.Config.Docker.ConfigDockerException`

        ~~~haskell
        [S-8575] = ResolverNotSupportedException (Maybe Project) (Maybe AbstractResolver)
        ~~~

    -   `Stack.Config.Nix.ConfigNixException`

        ~~~haskell
        [S-2726] = NixCannotUseShellFileAndPackagesException
        [S-9317] | GHCMajorVersionUnspecified
        [S-8605] | OnlyGHCSupported
        ~~~

    -   `Stack.ConfigCmd.ConfigCmdException`

        ~~~haskell
        [S-3136] = NoProjectConfigAvailable
        ~~~

    -   `Stack.Constants.ConstantsException`

        ~~~haskell
        [S-6057] = WiredInPackagesNotParsedBug
        ~~~

    -   `Stack.Coverage.CoveragePrettyException`

        ~~~haskell
        [S-6361] = NonTestSuiteTarget PackageName
        [S-2321] | NoTargetsOrTixSpecified
        [S-9975] | NotLocalPackage PackageName
        ~~~

    -   `Stack.Dot.DotException`

        ~~~haskell
        [S-7071] = DependencyNotFoundBug GhcPkgId
        [S-7151] | PackageNotFoundBug PackageName
        ~~~

    -   `Stack.Exec.ExecException`

        ~~~haskell
        [S-1541] = InvalidPathForExec FilePath
        ~~~

    -   `Stack.Exec.ExecPrettyException`

        ~~~haskell
        [S-8251] = PackageIdNotFoundBug String
        [S-2483] | ExecutableToRunNotFound
        [S-8600] | NoPackageIdReportedBug
        ~~~

    -   `Stack.GhcPkg`

        `[S-6716]` used in `unregisterGhcPkgIds`

    -   `Stack.Ghci.GhciException`

        ~~~haskell
        [S-6716] = InvalidPackageOption String
        [S-9632] | LoadingDuplicateModules
        [S-3600] | MissingFileTarget String
        [S-9906] | Can'tSpecifyFilesAndTargets
        [S-5188] | Can'tSpecifyFilesAndMainIs
        ~~~

    -   `Stack.Ghci.GhciPrettyException`

        ~~~haskell
        [S-6948] = GhciTargetParseException [StyleDoc]
        [S-1939] | CandidatesIndexOutOfRangeBug
        ~~~

    -   `Stack.Hoogle.HoogleException`

        ~~~haskell
        [S-9669] = HoogleOnPathNotFoundBug
        ~~~

    -   `Stack.Hoogle.HooglePrettyException`

        ~~~haskell
        [S-1329] = HoogleNotFound StyleDoc
        [S-3025] | HoogleDatabaseNotFound
        ~~~

    -   `Stack.Init.InitException`

        ~~~haskell
        [S-2747] | NoPackagesToIgnoreBug
        ~~~

    -   `Stack.Init.InitPrettyException`

        ~~~haskell
        [S-8332] = SnapshotDownloadFailure SomeException
        [S-8009] | ConfigFileAlreadyExists FilePath
        [S-5267] | PackageNameInvalid [FilePath]
        [S-1833] | NoMatchingSnapshot (NonEmpty SnapName)
        [S-6395] | ResolverMismatch RawSnapshotLocation String
        [S-2422] | ResolverPartial RawSnapshotLocation String
        ~~~

    -   `Stack.List.ListPrettyException`

        ~~~haskell
        [S-4926] = CouldNotParsePackageSelectors [StyleDoc]
        ~~~

    -   `Stack.Lock.LockPrettyException`

        ~~~haskell
        [S-1353] = WritingLockFileError (Path Abs File) Locked
        ~~~

    -   `Stack.Ls.LsException` *

        ~~~haskell
        [S-3421] = ParseFailure [Value]
        ~~~

    -   `Stack.New.NewPrettyException`

        ~~~haskell
        [S-2135] = ProjectDirAlreadyExists String (Path Abs Dir)
        [S-1688] | DownloadTemplateFailed Text String VerifiedDownloadException
        [S-3650] | LoadTemplateFailed TemplateName FilePath
        [S-9582] | ExtractTemplateFailed TemplateName FilePath String
        [S-9490] | TemplateInvalid TemplateName StyleDoc
        [S-5682] | MagicPackageNameInvalid String
        [S-3113] | AttemptedOverwrites [Path Abs File]
        ~~~

    -   `Stack.Nix.NixException`

        ~~~haskell
        [S-7384] = CannotDetermineProjectRoot
        ~~~

    -   `Stack.PackageDump.PackageDumpException`

        ~~~haskell
        [S-4257] = MissingSingleField Text (Map Text [Line])
        [S-2016] | Couldn'tParseField Text [Line]
        ~~~

    -   `Stack.Query.QueryException`

        ~~~haskell
        [S-4419] = SelectorNotFound [Text]
        [S-8422] | IndexOutOfRange [Text]
        [S-4360] | NoNumericSelector [Text]
        [S-1711] | CannotApplySelector Value [Text]
        ~~~

    -   `Stack.Runners.RunnersException`

        ~~~haskell
        [S-7144] = CommandInvalid
        [S-8314] | DockerAndNixInvalid
        [S-8641] | NixWithinDockerInvalid
        [S-5107] | DockerWithinNixInvalid
        ~~~

    -   `Stack.SDist.SDistPrettyException`

        ~~~haskell
        [S-6439] = CheckException (NonEmpty PackageCheck)
        [S-9595] | CabalFilePathsInconsistentBug (Path Abs File) (Path Abs File)
        [S-7875] | ToTarPathException
        ~~~

    -   `Stack.Script.ScriptException`

        ~~~haskell
        [S-4994] = MutableDependenciesForScript [PackageName]
        [S-1691] | AmbiguousModuleName ModuleName [PackageName]
        [S-5067] | ArgumentsWithNoRunInvalid
        [S-9469] | NoRunWithoutCompilationInvalid
        [S-5055] | FailedToParseScriptFileAsDirBug (Path Rel File)
        [S-9464] | FailedToParseFileAsDirBug (Path Abs Dir)
        ~~~

    -   `Stack.Setup.PerformPathCheckingException`

        ~~~haskell
        [S-1991] = ProcessExited ExitCode String [String]
        ~~~

    -   `Stack.Setup.SetupException`

        ~~~haskell
        [S-2076] = WorkingDirectoryInvalidBug
        [S-3967] | StackBinaryArchiveZipUnsupportedBug
        ~~~

    -   `Stack.Setup.SetupPrettyException`

        ~~~haskell
        [S-7441] = GHCInstallFailed SomeException StyleDoc String [String] (Path Abs Dir) (Path Abs Dir) (Path Abs Dir)
        [S-2476] | InvalidGhcAt (Path Abs File) SomeException
        [S-4764] | ExecutableNotFound [Path Abs File]
        [S-9953] | SandboxedCompilerNotFound [String] [Path Abs Dir]
        [S-1852] | UnsupportedSetupCombo OS Arch
        [S-2126] | MissingDependencies [String]
        [S-9443] | UnknownCompilerVersion (Set Text) WantedCompiler (Set ActualCompiler)
        [S-6810] | UnknownOSKey Text
        [S-5159] | GHCSanityCheckCompileFailed SomeException (Path Abs File)
        [S-8948] | RequireCustomGHCVariant
        [S-2905] | ProblemWhileDecompressing (Path Abs File)
        [S-9561] | SetupInfoMissingSevenz
        [S-7748] | UnsupportedSetupConfiguration
        [S-5308] | MSYS2NotFound Text
        [S-5127] | UnwantedCompilerVersion
        [S-1540] | UnwantedArchitecture
        [S-8668] | GHCInfoNotValidUTF8 UnicodeException
        [S-4878] | GHCInfoNotListOfPairs
        [S-2965] | GHCInfoMissingGlobalPackageDB
        [S-5219] | GHCInfoMissingTargetPlatform
        [S-8299] | GHCInfoTargetPlatformInvalid String
        [S-2574] | CabalNotFound (Path Abs File)
        [S-8488] | GhcBootScriptNotFound
        [S-1128] | HadrianScriptNotFound
        [S-1906] | URLInvalid String
        [S-1648] | UnknownArchiveExtension String
        [S-4509] | Unsupported7z
        [S-3158] | TarballInvalid String
        [S-5252] | TarballFileInvalid String (Path Abs File)
        [S-1827] | UnknownArchiveStructure (Path Abs File)
        [S-9476] | StackReleaseInfoNotFound String
        [S-4461] | StackBinaryArchiveNotFound [String]
        [S-6617] | HadrianBindistNotFound
        [S-7227] | DownloadAndInstallCompilerError
        [S-6636] | StackBinaryArchiveUnsupported Text
        [S-7871] | StackBinaryNotInArchive String Text
        [S-5046] | FileTypeInArchiveInvalid Entry Text
        [S-4132] | BinaryUpgradeOnOSUnsupported OS
        [S-3249] | BinaryUpgradeOnArchUnsupported Arch
        [S-4230] | ExistingMSYS2NotDeleted (Path Abs Dir) IOException
        ~~~

    -   `Stack.StackException`

        ~~~haskell
        [S-2186] = InvalidReExecVersion String String
        ~~~

    -   `Stack.Storage.User.StorageUserException`

        ~~~haskell
        [S-8196] = CompilerFileMetadataMismatch
        [S-5378] | GlobalPackageCacheFileMetadataMismatch
        [S-2673] | GlobalDumpParseFailure
        [S-8441] | CompilerCacheArchitectureInvalid Text
        ~~~

    -   `Stack.Templates.TemplatesPrettyException`

        ~~~haskell
        [S-8143] = DownloadTemplatesHelpFailed HttpException
        [S-6670] | TemplatesHelpEncodingInvalid String UnicodeException
        ~~~

    -   `Stack.Types.Build.BuildException`

        ~~~haskell
        [S-7178] = Couldn'tFindPkgId PackageName
        [S-6362] | CompilerVersionMismatch (Maybe (ActualCompiler, Arch)) (WantedCompiler, Arch) GHCVariant CompilerBuild VersionCheck (Maybe (Path Abs File)) Text
        [S-3127] | Couldn'tParseTargets [Text]
        [S-2154] | UnknownTargets (Set PackageName) (Map PackageName Version) (Path Abs File)
        [S-1995] | TestSuiteFailure PackageIdentifier (Map Text (Maybe ExitCode)) (Maybe (Path Abs File)) ByteString
        [S-3819] | TestSuiteTypeUnsupported TestSuiteInterface
        [S-5797] | LocalPackageDoesn'tMatchTarget PackageName Version Version
        [S-3118] | NoSetupHsFound (Path Abs Dir)
        [S-4925] | InvalidGhcOptionsSpecification [PackageName]
        [S-7987] | TestSuiteExeMissing Bool String String String
        [S-8027] | CabalCopyFailed Bool String
        [S-5510] | LocalPackagesPresent [PackageIdentifier]
        [S-7168] | CouldNotLockDistDir (Path Abs File)
        [S-7868] | TaskCycleBug PackageIdentifier
        [S-8923] | PackageIdMissingBug PackageIdentifier
        [S-7371] | AllInOneBuildBug
        [S-6739] | MultipleResultsBug PackageName [DumpPackage]
        [S-3121] | TemplateHaskellNotFoundBug
        [S-6901] | HaddockIndexNotFound
        [S-5452] | ShowBuildErrorBug
        [S-2696] | CallStackEmptyBug
        ~~~

    -   `Stack.Types.Build.BuildPrettyException`

        ~~~haskell
        [S-4804] = ConstructPlanFailed [ConstructPlanException] (Path Abs File) (Path Abs Dir) ParentMap (Set PackageName) (Map PackageName [PackageName])
        [S-7282] | ExecutionFailure [SomeException]
        [S-7011] | CabalExitedUnsuccessfully ExitCode PackageIdentifier (Path Abs File) [String] (Maybe (Path Abs File)) [Text]
        [S-6374] | SetupHsBuildFailure ExitCode (Maybe PackageIdentifier) (Path Abs File) [String] (Maybe (Path Abs File)) [Text]
        [S-8506] | TargetParseException [StyleDoc]
        [S-7086] | SomeTargetsNotBuildable [(PackageName, NamedComponent)]
        [S-8664] | InvalidFlagSpecification (Set UnusedFlags)
        [S-8100] | GHCProfOptionInvalid
        [S-1727] | NotOnlyLocal [PackageName] [Text]
        ~~~

    -   `Stack.Types.Compiler.CompilerException`

        ~~~haskell
        [S-7903] = GhcjsNotSupported
        [S-7972] | PantryException PantryException
        ~~~

    -   `Stack.Types.Config.Exception.ConfigException`

        ~~~haskell
        [S-8981] | ParseCustomSnapshotException Text ParseException
        [S-2206] | NoProjectConfigFound (Path Abs Dir) (Maybe Text)
        [S-4964] | UnexpectedArchiveContents [Path Abs Dir] [Path Abs File]
        [S-2040] | UnableToExtractArchive Text (Path Abs File)
        [S-1641] | BadStackVersionException VersionRange
        [S-8773] | NoSuchDirectory FilePath
        [S-3938] | ParseGHCVariantException String
        [S-8530] | BadStackRoot (Path Abs Dir)
        [S-7613] | Won'tCreateStackRootInDirectoryOwnedByDifferentUser (Path Abs Dir) (Path Abs Dir)
        [S-8707] | UserDoesn'tOwnDirectory (Path Abs Dir)
        [S-3605] | ManualGHCVariantSettingsAreIncompatibleWithSystemGHC
        [S-6816] | NixRequiresSystemGhc
        [S-5027] | NoResolverWhenUsingNoProject
        [S-3803] | NoLTSWithMajorVersion Int
        [S-5472] | NoLTSFound
        ~~~

    -   `Stack.Types.Config.Exception.ConfigPrettyException`

        ~~~haskell
        [S-6602] = ParseConfigFileException (Path Abs File) ParseException
        [S-7462] | StackWorkEnvNotRelativeDir String
        [S-3251] | MultiplePackageIndices [PackageIndexConfig]
        [S-5470] | DuplicateLocalPackageNames [(PackageName, [PackageLocation])]
        ~~~

    -   `Stack.Types.Config.ParseAbsolutePathException`

        ~~~haskell
        [S-9437] = ParseAbsolutePathException String String
        ~~~

    -   `Stack.Types.Docker.DockerException`

        ~~~haskell
        [S-3223] = DockerMustBeEnabledException
        [S-9779] | OnlyOnHostException
        [S-9105] | InspectFailedException String
        [S-6626] | NotPulledException String
        [S-5841] | InvalidImagesOutputException String
        [S-9608] | InvalidPSOutputException String
        [S-2240] | InvalidInspectOutputException String
        [S-6092] | PullFailedException String
        [S-6218] | DockerTooOldException Version Version
        [S-8252] | DockerVersionProhibitedException [Version] Version
        [S-6170] | BadDockerVersionException VersionRange Version
        [S-5827] | InvalidVersionOutputException
        [S-7112] | HostStackTooOldException Version (Maybe Version)
        [S-5832] | ContainerStackTooOldException Version Version
        [S-4078] | CannotDetermineProjectRootException
        [S-7058] | DockerNotInstalledException
        [S-6894] | UnsupportedStackExeHostPlatformException
        [S-1512] | DockerStackExeParseException String
        ~~~

    -   `Stack.Types.GhcPkgId.GhcPkgIdParseFail`

        ~~~haskell
        [S-5359] = GhcPkgIdParseFail Text
        ~~~

    -   `Stack.Types.Package.PackageException`

        ~~~haskell
        [S-8072] = PackageInvalidCabalFile (Either PackageIdentifierRevision (Path Abs File)) (Maybe Version) [PError] [PWarning]
        [S-5394] | MismatchedCabalIdentifier PackageIdentifierRevision PackageIdentifier
        [S-2203] | CabalFileNameParseFail FilePath
        [S-8854] | CabalFileNameInvalidPackageName FilePath
        [S-4623] | ComponentNotParsedBug
        ~~~

    -   `Stack.Types.Resolver.TypesResolverException`

        ~~~haskell
        [S-8787] = ParseResolverException Text
        [S-4865] | FilepathInDownloadedSnapshot Text
        ~~~

    -   `Stack.Types.Storage.StoragePrettyException`

        ~~~haskell
        [S-8835] = StorageMigrationFailure Text (Path Abs File) SomeException
        ~~~

    -   `Stack.Types.TemplateName.TypesTemplateNameException`

        ~~~haskell
        [S-7410] = DefaultTemplateNameNotParsedBug String
        ~~~

    -   `Stack.Unpack.UnpackPrettyException`

        ~~~haskell
        [S-3515] = UnpackDirectoryAlreadyExists (Set (Path Abs Dir))
        [S-2628] | CouldNotParsePackageSelectors [String]
        ~~~

    -   `Stack.Upgrade.UpgradePrettyException`

        ~~~haskell
        [S-8761] = ResolverOptionInvalid
        [S-3642] | NeitherBinaryOrSourceSpecified
        [S-8716] | ExecutableFailure
        [S-7114] | CommitsNotFound String String
        [S-9668] | StackInPackageIndexNotFound
        [S-6648] | VersionWithNoRevision
        ~~~

    -   `Stack.Upload.UploadPrettyException`

        ~~~haskell
        [S-2256] = AuthenticationFailure
        [S-6108] | ArchiveUploadFailure Int [String] String
        [S-2837] | DocsTarballInvalid [(String, Path Abs File)]
        [S-3179] | ItemsInvalid [FilePath]
        [S-3030] | NoItemSpecified String
        [S-5908] | PackageDirectoryInvalid [FilePath]
        [S-7274] | PackageIdNotSpecifiedForDocsUploadBug
        [S-5860] | PackageIdSpecifiedForPackageUploadBug
        [S-5955] | TarGzFileNameInvalidBug String
        ~~~

    -   `System.Process.Pager.PagerException`

        ~~~haskell
        [S-9392] = PagerExitFailure CmdSpec Int
        ~~~

    \* The instance of `Show` is derived.

*   `Stack.Build.Execute.singleBuild`: catches exceptions in `cabal ...`

    `throwM`

*   `Stack.Build.Source.getFileDigestMaybe`: catches exceptions in
    `liftM Just . withSourceFile fp $ getDigest`

    `throwM`

*   `Stack.Config.configFromConfigMonoid`:

    ~~~text
    [S-8432] Stack's 'programs' path contains a space character and has no alternative
    short ('8 dot 3') name. This will cause problems with packages that use the
    GNU project's 'configure' shell script. Use the 'local-programs-path'
    configuration option to specify an alternative path. The current path is:
    <path>
    ~~~

*   `Stack.Coverage.generateHpcReport`: catches exceptions from
    `findPackageFieldForBuiltPackage`

    ~~~text
    <exception>
    ~~~

*   `Stack.Coverage.generateHpcReportInternal`:

    ~~~text
    [S-4634] Didn't find .tix for <report> - expected to find it at <path>.
    ~~~

*   `Stack.Coverage.generateHpcReportInternal`:

    ~~~text
    [S-8215] <exception>
    Error occurred while producing <report>"
    ~~~

*   `Stack.Coverage.generateHpcReportInternal`:

    ~~~text
    [S-6829] Error: The <report> did not consider any code. One possible cause of this is
    if your test-suite builds the library code (see Stack issue #1008). It may
    also indicate a bug in Stack or the hpc program. Please report this issue if
    you think your coverage report should have meaningful results.
    ~~~

*   `Stack.Coverage.readTixOrlog`:

    ~~~text
    [S-3521] Error while reading tix: <exeception>
    ~~~

*   `Stack.Coverage.readTixOrlog`:

    ~~~text
    [S-7786] Failed to read tix file <path>
    ~~~

*   `Stack.Coverage.updateTixFile`:

    ~~~text
    [S-2887] Failed to read <file>
    ~~~

*   `Stack.Ghci.buildDepsAndInitialSteps`: catches exeception from
    `buildLocalTargets`

    ~~~text
    <exception>
    ~~~

*   `Stack.GhcPkg.createDatabase`:

    ~~~text
    [S-9735] Unable to create package database at <path>
    ~~~

*   `Stack.Lock.loadYamlThrow`:

    `Data.Yaml.AesonException`

    `throwIO`

*   `Stack.Package.resolveGlobFiles`:

    `Control.Exception.Base.IOException`

    `throwIO`

*   `Stack.Runners.withConfig`:

    ~~~text
    [S-7353] Error when running shouldUpgradeCheck: <exception>
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

*   `Stack.SDist.getSDistTarball`:

    ~~~text
    [S-8399] Error building custom-setup dependencies: <exception>
    ~~~

*   `Stack.Setup.downloadStackExe`: catches exceptions from `performPathChecking`

    ~~~text
    <exception>
    ~~~

*   `Stack.Upload.uploadBytes`:

    ~~~text
    [S-2804] forbidden upload
    Usually means: you've already uploaded this package/version combination
    Ignoring error and continuing, full message from Hackage below:
    <Hackage_message>
    ~~~

*   `Stack.Upload.uploadBytes`:

    ~~~text
    [S-4444] service unavailable
    This error some times gets sent even though the upload succeeded
    Check on Hackage to see if your package is present
    <Hackage_message>
    ~~~
