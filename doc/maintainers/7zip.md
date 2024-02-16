<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Upgrading 7-Zip

When installing GHC or MSYS2 on Windows, Stack will also install
[7-Zip](https://www.7-zip.org/). 7-Zip is a file archiver and is used by Stack
to extract files from archives. This section explains the steps required to
upgrade the 7-Zip version used by Stack. The 7-Zip functionality used by Stack
is mature and stable. It is anticipated that the Stack-supplied 7-Zip will not
need to be updated frequently. On 10 September 2022, it was updated from 7-Zip
9.20 (released on 18 November 2010) to 7-Zip 22.01 (released on 15 July 2022).

1.  Download the latest installer for 64-bit x64 Windows from 7-Zip's website.

2.  Run the installer and install to the default location
    (`C:\C:\Program Files\7-Zip`). The four relevant files from those installed
    will be:

    ~~~text
    7z.exe  # 7-Zip Console
    7z.dll  # 7-Zip Engine
    license.txt  # 7-Zip License
    readme.txt  # 7-Zip Overview
    ~~~

3.  In the
    [commercialhaskell/stackage-content](https://github.com/commercialhaskell/stackage-content)
    GitHub repository, create a new draft release tagged and named `7z-XX.YY`,
    where `XX.YY` is the 7-Zip version number.

4.  Upload the four relevant files in step 2 above into the draft release.

5.  Provide a description for the release. For example:

    ~~~text
    7-Zip 22.01 (2022-07-15) for Windows 64-bit x64.
    ~~~

6.  Publish the release.

7.  Changes need to be made to the
    [stackage-content/stack/stack-setup-2.yaml](https://github.com/commercialhaskell/stackage-content/blob/master/stack/stack-setup-2.yaml)
    file, to switch over to using the newly uploaded files. For example
    (extract):

    ~~~yaml
    sevenzexe-info:
        url: "https://github.com/commercialhaskell/stackage-content/releases/download/7z-22.01/7z.exe"
        content-length: 545280
        sha256: 254cf6411d38903b2440819f7e0a847f0cfee7f8096cfad9e90fea62f42b0c23

    sevenzdll-info:
        url: "https://github.com/commercialhaskell/stackage-content/releases/download/7z-22.01/7z.dll"
        content-length: 1814016
        sha256: 73578f14d50f747efa82527a503f1ad542f9db170e2901eddb54d6bce93fc00e
    ~~~

    The `content-length:` key's value is the size of the file in bytes. It can
    be obtained from the `Length` field of the `dir` command. The `sha256:`
    key's value can be obtained from the commands (in PowerShell):

    ~~~text
    (Get-FileHash 7z.exe -Algorithm SHA256).Hash.ToLower()
    (Get-FileHash 7z.dll -Algorithm SHA256).Hash.ToLower()
    ~~~

    The `sha256:` key only accepts lowercase hash results as values.

8.  The changed `stack-setup-2.yaml` file should be tested locally. This can be
    done by:

    * temporarily disabling the existing local copy of 7-Zip by changing the
      name of the `7z.exe` and `7z.dll` files in the `stack path --programs`
      directory;

    * identifying a version of GHC not already installed in the
      `stack path --programs` directory; and

    * executing the command:

        ~~~text
        stack --snapshot <snapshot> setup --setup-info-yaml <path to local copy of stack-setup-2.yaml>
        ~~~

      where `<snapshot>` requires the missing version of GHC.

    If all is well, the command should proceed to download the missing version
    of GHC, download the `7z.exe` and `7z.dll` files, and use the 7-Zip version
    to extract files from the GHC archive.

9.  Raise a pull request on `commercialhaskell/stackage-contents` for the
    changes to the locally-tested `stack-setup-2.yaml` file.
