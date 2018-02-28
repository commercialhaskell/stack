module Main where-- Stack.Hack

import qualified Data.Text as Text
import Stack.Dot -- (listDependencies)
import Stack.Types.Config.Build(defaultBuildOptsCLI)
import Stack.Runners
import RIO.Process
import RIO.Prelude

doOpts = DotOpts True True Nothing mempty mempty mempty True True
ldoOpts = ListDepsOpts doOpts (Text.pack ",") False

main = do
     -- deps <- runRIO $ listDependencies ldoOpts
     void $ withMiniConfigAndLock undefined $ do
          liftIO (print "hi")
