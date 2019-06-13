module Curator.GithubPings
    ( getGithubPings
    , applyGithubMapping
    ) where

import Curator.Types
import Distribution.PackageDescription
import RIO
import RIO.List (stripPrefix)
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as T

applyGithubMapping :: Constraints -> Set Text -> Set Text
applyGithubMapping cons =
    foldMap (\name -> fromMaybe (Set.singleton name) (Map.lookup name (consGithubUsers cons)))

-- | Determine accounts to be pinged on Github based on various metadata in the
-- package description.
getGithubPings :: GenericPackageDescription -> Set Text
getGithubPings gpd =
        Set.fromList $
        map T.pack $
        goHomepage (homepage $ packageDescription gpd) ++
        concatMap goRepo (sourceRepos $ packageDescription gpd)
  where
    goHomepage t = do
        prefix <-
            [ "http://github.com/"
            , "https://github.com/"
            , "git://github.com/"
            , "git@github.com:"
            ]
        t' <- maybeToList $ stripPrefix prefix t
        let t'' = takeWhile (/= '/') t'
        guard $ not $ null t''
        return t''

    goRepo sr =
        case (repoType sr, repoLocation sr) of
            (Just Git, Just s) -> goHomepage s
            _ -> []
