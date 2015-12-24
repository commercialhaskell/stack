REM TODO: move this logic into release.hs
setlocal
path C:\Program Files\Git\usr\bin;%PATH%
set STACK_ROOT=C:\.stack
set TMP=C:\tmp
set TEMP=C:\tmp
set RELEASE_SCRIPT=%APPDATA%\local\bin\stack-release-script.exe
if exist %RELEASE_SCRIPT% del %RELEASE_SCRIPT%
cd etc\scripts
stack --install-ghc install
if errorlevel 1 exit /b
cd ..\..
%RELEASE_SCRIPT% --no-test-haddocks --arch=i386 --upload-label="Windows 32-bit" release
if errorlevel 1 exit /b
%RELEASE_SCRIPT% --no-test-haddocks --arch=x86_64 --upload-label="Windows 64-bit" release
if errorlevel 1 exit /b
