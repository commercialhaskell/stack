{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Config.ConfigureScript
License     : BSD-3-Clause
-}

module Stack.Config.ConfigureScript
  ( ensureConfigureScript
  ) where

import           Path ( (</>) )
import           Path.IO ( doesFileExist )
import           Stack.Constants ( osIsWindows, relFileConfigure )
import           Stack.DefaultColorWhen ( defaultColorWhen )
import           Stack.Prelude
import           RIO.Process ( HasProcessContext, withWorkingDir )

ensureConfigureScript ::
     (HasProcessContext env, HasTerm env)
  => Path b Dir
  -> RIO env ()
ensureConfigureScript dir = do
  let fp = dir </> relFileConfigure
  exists <- doesFileExist fp
  unless exists $ do
    prettyInfoL
      [ flow "Trying to generate"
      , style Shell "configure"
      , "with"
      , style Shell "autoreconf"
      , "in"
      , pretty dir <> "."
      ]
    let autoreconf = if osIsWindows
                       then readProcessNull "sh" ["autoreconf", "-i"]
                       else readProcessNull "autoreconf" ["-i"]
        -- On Windows 10, an upstream issue with the `sh autoreconf -i`
        -- command means that command clears, but does not then restore, the
        -- ENABLE_VIRTUAL_TERMINAL_PROCESSING flag for native terminals. The
        -- following hack re-enables the lost ANSI-capability.
        fixupOnWindows = when osIsWindows (void $ liftIO defaultColorWhen)
    withWorkingDir (toFilePath dir) $ autoreconf `catchAny` \ex -> do
      fixupOnWindows
      prettyWarn $
           fillSep
             [ flow "Stack failed to run"
             , style Shell "autoreconf" <> "."
             ]
        <> blankLine
        <> flow "Stack encountered the following error:"
        <> blankLine
        <> string (displayException ex)
      when osIsWindows $ do
        prettyInfo $
             fillSep
               [ flow "Check that executable"
               , style File "perl"
               , flow "is on the path in Stack's MSYS2"
               , style Dir "\\usr\\bin"
               , flow "folder, and working, and that script files"
               , style File "autoreconf"
               , "and"
               , style File "aclocal"
               , flow "are on the path in that location. To check that"
               , style File "perl" <> ","
               , style File "autoreconf"
               , "or"
               , style File "aclocal"
               , flow "are on the path in the required location, run commands:"
               ]
          <> blankLine
          <> indent 4 (style Shell $ flow "stack exec where.exe -- perl")
          <> line
          <> indent 4 (style Shell $ flow "stack exec where.exe -- autoreconf")
          <> line
          <> indent 4 (style Shell $ flow "stack exec where.exe -- aclocal")
          <> blankLine
          <> fillSep
               [ "If"
               , style File "perl" <> ","
               , style File "autoreconf"
               , "or"
               , style File "aclocal"
               , flow "is not on the path in the required location, add them \
                      \with command (note that the relevant package name is"
               , style File "autotools"
               , "not"
               , style File "autoreconf" <> "):"
               ]
          <> blankLine
          <> indent 4
               (style Shell $ flow "stack exec pacman -- --sync --refresh mingw-w64-x86_64-autotools")
          <> blankLine
          <> fillSep
               [ flow "Some versions of"
               , style File "perl"
               , flow "from MSYS2 are broken. See"
               , style Url "https://github.com/msys2/MSYS2-packages/issues/1611"
               , "and"
               , style Url "https://github.com/commercialhaskell/stack/pull/4781" <> "."
               , "To test if"
               , style File "perl"
               , flow "in the required location is working, try command:"
               ]
          <> blankLine
          <> indent 4 (style Shell $ flow "stack exec perl -- --version")
          <> blankLine
    fixupOnWindows
