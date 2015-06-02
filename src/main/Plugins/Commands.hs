-- | Using Plugins with SimpleOptions
module Plugins.Commands
  ( commandsFromPlugins
  , commandFromPlugin
  , findPlugins
  , PluginException(..)
  ) where

import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Trans.Writer (Writer)
import Data.Text (unpack)
import Plugins
import Options.Applicative.Simple

-- | Generate the "commands" argument to simpleOptions
-- based on available plugins.
commandsFromPlugins :: Plugins -> (Plugin -> a) -> EitherT a (Writer (Mod CommandFields a)) ()
commandsFromPlugins plugins f =
  mapM_ (\p -> commandFromPlugin p (f p)) (listPlugins plugins)

-- | Convert a single plugin into a command.
commandFromPlugin :: Plugin -> a -> EitherT a (Writer (Mod CommandFields a)) ()
commandFromPlugin plugin a = addCommand
  (unpack $ pluginName plugin)
  (unpack $ pluginSummary plugin)
  (const a)
  (pure ())
