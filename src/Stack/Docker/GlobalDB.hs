{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings,
             GADTs, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving,
             RankNTypes, NamedFieldPuns #-}

-- | Global sqlite database shared by all projects.
-- Warning: this is currently only accessible from __outside__ a Docker container.
module Stack.Docker.GlobalDB
  (updateDockerImageLastUsed
  ,getDockerImagesLastUsed
  ,pruneDockerImagesLastUsed
  ,DockerImageLastUsed
  ,DockerImageProjectId)
  where

import           Control.Exception (IOException,catch,throwIO)
import           Control.Monad (forM_)
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.List (sortBy, isInfixOf, stripPrefix)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime,getCurrentTime)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           System.Directory (getHomeDirectory,createDirectoryIfMissing)
import           System.FilePath ((</>))

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
DockerImageProject
    imageHash                 String
    projectPath               FilePath
    lastUsedTime              UTCTime
    DockerImageProjectPathKey imageHash projectPath
    deriving Show
|]

-- | Update last used time and project for a Docker image hash.
updateDockerImageLastUsed :: String -> FilePath -> IO ()
updateDockerImageLastUsed imageId projectPath =
  do curTime <- getCurrentTime
     _ <- withGlobalDB (upsert (DockerImageProject imageId projectPath curTime) [])
     return ()

-- | Get a list of Docker image hashes and when they were last used.
getDockerImagesLastUsed :: IO [DockerImageLastUsed]
getDockerImagesLastUsed =
  do imageProjects <- withGlobalDB (selectList [] [Asc DockerImageProjectLastUsedTime])
     return (sortBy (flip sortImage)
                    (Map.toDescList (Map.fromListWith (++)
                                                      (map mapImageProject imageProjects))))
  where
    mapImageProject (Entity _ imageProject) =
      (dockerImageProjectImageHash imageProject
      ,[(dockerImageProjectLastUsedTime imageProject
        ,dockerImageProjectProjectPath imageProject)])
    sortImage (_,(a,_):_) (_,(b,_):_) = compare a b
    sortImage _ _ = EQ

-- | Given a list of all existing Docker images, remove any that no longer exist from
-- the database.
pruneDockerImagesLastUsed :: [String] -> IO ()
pruneDockerImagesLastUsed existingHashes =
  withGlobalDB (do l <- selectList [] []
                   forM_ l (\(Entity k (DockerImageProject{dockerImageProjectImageHash = h})) ->
                              if h `elem` existingHashes
                                then return ()
                                else delete k))

-- | Run an action with the global database.  This performs any needed migrations as well.
withGlobalDB :: forall a. SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
withGlobalDB action =
  do home <- getHomeDirectory
     let dir = home </> ".stackage-build" </> "global"
         db = dir </> "stackage-build.db"
     createDirectoryIfMissing True dir
     runSqlite (T.pack db)
               (do _ <- runMigrationSilent migrateTables
                   action)
         `catch` \ex -> do
             let str = show ex
                 stripSuffix x = fmap reverse . stripPrefix x . reverse
                 str' = fromMaybe str $ stripPrefix "user error (" $
                        fromMaybe str $ stripSuffix ")" str
             if "ErrorReadOnly" `isInfixOf` str
                 then fail $ str' ++
                     " This likely indicates that your DB file, " ++
                     db ++  ", has incorrect permissions or ownership."
                 else throwIO (ex :: IOException)

-- | Date and project path where Docker image hash last used.
type DockerImageLastUsed = (String, [(UTCTime, FilePath)])
