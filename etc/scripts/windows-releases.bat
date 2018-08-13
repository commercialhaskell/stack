REM TODO: move this logic into release.hs
REM Note: STACK_ROOT, TMP, and TEMP must be set to short paths otherwise this is unlikely to work
setlocal
stack %~dp0\release.hs --no-test-haddocks --arch=x86_64 %1 %2 %3 %4 %5 %6 %7 %8 %9 release
if errorlevel 1 exit /b
stack %~dp0\release.hs --no-test-haddocks --arch=i386 %1 %2 %3 %4 %5 %6 %7 %8 %9 upload
if errorlevel 1 exit /b
