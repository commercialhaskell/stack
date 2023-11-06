{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Options.NewParser
  ( newOptsParser
  ) where

import qualified Data.Map.Strict as M
import           Options.Applicative
                   ( Parser, help, idm, long, metavar, short, switch )
import           Options.Applicative.Builder.Extra ( boolFlags )
import           Stack.Init ( InitOpts )
import           Stack.New ( NewOpts (..) )
import           Stack.Options.InitParser ( initOptsParser )
import           Stack.Prelude
import           Stack.Types.PackageName ( packageNameArgument )
import           Stack.Types.TemplateName
                   ( templateNameArgument, templateParamArgument )

-- | Parser for @stack new@.
newOptsParser :: Parser (NewOpts, InitOpts)
newOptsParser = (,) <$> newOpts <*> initOptsParser
 where
  newOpts = NewOpts
    <$> packageNameArgument
          (  metavar "PACKAGE_NAME"
          <> help "A valid package name."
          )
    <*> switch
          (  long "bare"
          <> help "Do not create a subdirectory for the project."
          )
    <*> boolFlags True
          "init"
          "the initialisation of the project for use with Stack."
          idm
    <*> optional (templateNameArgument
          (  metavar "TEMPLATE_NAME"
          <> help "Name of a template - can take the form\
                  \ [[service:]username/]template with optional service name\
                  \ (github, gitlab, or bitbucket) and username for the \
                  \service; or, a local filename such as foo.hsfiles or ~/foo; \
                  \or, a full URL such as https://example.com/foo.hsfiles."
          ))
    <*> fmap M.fromList (many (templateParamArgument
          (  short 'p'
          <> long "param"
          <> metavar "KEY:VALUE"
          <> help "Parameter for the template in the format key:value."
          )))
