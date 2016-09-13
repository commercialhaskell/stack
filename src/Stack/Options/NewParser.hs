module Stack.Options.NewParser where

import qualified Data.Map.Strict                   as M
import           Data.Monoid.Extra
import           Options.Applicative
import           Stack.Init
import           Stack.New
import           Stack.Options.GlobalParser
import           Stack.Types.PackageName
import           Stack.Types.TemplateName

-- | Parser for @stack new@.
newOptsParser :: Parser (NewOpts,InitOpts)
newOptsParser = (,) <$> newOpts <*> initOptsParser
  where
    newOpts =
        NewOpts <$>
        packageNameArgument
            (metavar "PACKAGE_NAME" <> help "A valid package name.") <*>
        switch
            (long "bare" <>
             help "Do not create a subdirectory for the project") <*>
        optional (templateNameArgument
            (metavar "TEMPLATE_NAME" <>
             help "Name of a template or a local template in a file or a URL.\
                  \ For example: foo or foo.hsfiles or ~/foo or\
                  \ https://example.com/foo.hsfiles")) <*>
        fmap
            M.fromList
            (many
                 (templateParamArgument
                      (short 'p' <> long "param" <> metavar "KEY:VALUE" <>
                       help
                           "Parameter for the template in the format key:value")))
