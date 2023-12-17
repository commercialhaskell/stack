{- HLINT ignore -}
{-# LANGUAGE CPP, RecordWildCards, ViewPatterns #-}

module Generate(main) where

import Control.Exception
import Control.Monad
import Data.Semigroup
import Data.Char
import Data.List
import System.Directory
import System.IO


main :: IO ()
main = do
    src <- readFile "System/FilePath/Internal.hs"
    let tests = map renderTest $ concatMap parseTest $ lines src
    writeFileBinaryChanged "tests/filepath-tests/TestGen.hs" $ unlines $
        ["-- GENERATED CODE: See ../Generate.hs"
#ifndef GHC_MAKE
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "{-# LANGUAGE ViewPatterns #-}"
#endif
        , "{-# LANGUAGE CPP #-}"
        , "{-# OPTIONS_GHC -Wno-name-shadowing #-}"
        , "{-# OPTIONS_GHC -Wno-orphans #-}"
        ,"module TestGen(tests) where"
        ,"import TestUtil"
        ,"#if !MIN_VERSION_base(4,11,0)"
        ,"import Data.Semigroup"
        ,"#endif"
        ,"import Prelude as P"
        ,"import Data.String"
        ,"import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )"
        ,"import GHC.IO.Encoding.UTF16 ( mkUTF16le )"
        ,"import GHC.IO.Encoding.UTF8 ( mkUTF8 )"
        ,"import System.OsString.Internal.Types"
        ,"import System.OsPath.Encoding.Internal"
        ,"import qualified Data.Char as C"
        ,"import qualified System.OsPath.Data.ByteString.Short as SBS"
        ,"import qualified System.OsPath.Data.ByteString.Short.Word16 as SBS16"
        ,"import qualified System.FilePath.Windows as W"
        ,"import qualified System.FilePath.Posix as P"
#ifdef GHC_MAKE
        ,"import qualified System.OsPath.Windows.Internal as AFP_W"
        ,"import qualified System.OsPath.Posix.Internal as AFP_P"
#else
        ,"import qualified System.OsPath.Windows as AFP_W"
        ,"import qualified System.OsPath.Posix as AFP_P"
#endif
        ,"instance IsString WindowsString where fromString = WS . either (error . show) id . encodeWithTE (mkUTF16le TransliterateCodingFailure)"
        ,"instance IsString PosixString where fromString = PS . either (error . show) id . encodeWithTE (mkUTF8 TransliterateCodingFailure)"
        ,"#if defined(mingw32_HOST_OS) || defined(__MINGW32__)"
        ,"instance IsString OsString where fromString = OsString . WS . either (error . show) id . encodeWithTE (mkUTF16le TransliterateCodingFailure)"
        ,"#else"
        ,"instance IsString OsString where fromString = OsString . PS . either (error . show) id . encodeWithTE (mkUTF8 TransliterateCodingFailure)"
        ,"#endif"
        ,"tests :: [(String, Property)]"
        ,"tests ="] ++
        ["    " ++ c ++ "(" ++ show t1 ++ ", " ++ t2 ++ ")" | (c,(t1,t2)) <- zip ("[":repeat ",") tests] ++
        ["    ]"]



data PW = P      -- legacy posix
        | W      -- legacy windows
        | AFP_P  -- abstract-filepath posix
        | AFP_W  -- abstract-filepath windows
  deriving Show

data Test = Test
    {testPlatform :: PW
    ,testVars :: [(String,String)]   -- generator constructor, variable
    ,testBody :: [String]
    }


parseTest :: String -> [Test]
parseTest (stripPrefix "-- > " -> Just x) = platform $ toLexemes x
    where
        platform ("Windows":":":x) = [valid W x, valid AFP_W x]
        platform ("Posix"  :":":x) = [valid P x, valid AFP_P x]
        platform x                 = [valid P x, valid W x, valid AFP_P x, valid AFP_W x]

        valid p ("Valid":x) = free p a $ drop 1 b
            where (a,b) = break (== "=>") x
        valid p x = free p [] x

        free p val x = Test p [(ctor v, v) | v <- vars] x
            where vars = nub $ sort [v | v@[c] <- x, isAlpha c]
                  ctor v | v < "x"  = ""
                         | v `elem` val = "QFilePathValid" ++ show p
                         | otherwise = case p of
                                         AFP_P -> if v == "z" then "QFilePathsAFP_P" else "QFilePathAFP_P"
                                         AFP_W -> if v == "z" then "QFilePathsAFP_W" else "QFilePathAFP_W"
                                         _ -> if v == "z" then "" else "QFilePath"
parseTest _ = []


toLexemes :: String -> [String]
toLexemes x = case lex x of
    [("","")] -> []
    [(x,y)] -> x : toLexemes y
    y -> error $ "Generate.toLexemes, " ++ show x ++ " -> " ++ show y


fromLexemes :: [String] -> String
fromLexemes = unwords . f
    where
        f ("`":x:"`":xs) = ("`" ++ x ++ "`") : f xs
        f (x:y:xs) | x `elem` ["[","("] || y `elem` [",",")","]"] = f $ (x ++ y) : xs
        f (x:xs) = x : f xs
        f [] = []


renderTest :: Test -> (String, String)
renderTest Test{..} = (body, code)
    where
        code = "property $ " ++ if null testVars then body else "\\" ++ unwords vars ++ " -> " ++ body
        vars = [if null ctor then v else "(" ++ ctor ++ " " ++ v ++ ")" | (ctor,v) <- testVars]

        body = fromLexemes $ map (qualify testPlatform) testBody



qualify :: PW -> String -> String
qualify pw str
    | str `elem` fpops || (all isAlpha str && length str > 1 && str `notElem` prelude)
      = if str `elem` bs then qualifyBS str  else show pw ++ "." ++ str
    | otherwise = encode str
    where
        bs = ["null", "concat", "isPrefixOf", "isSuffixOf", "any"]
        prelude = ["elem","uncurry","snd","fst","not","if","then","else"
                  ,"True","False","Just","Nothing","fromJust","foldr"]
        fpops = ["</>","<.>","-<.>"]
#ifdef GHC_MAKE
        encode v
          | isString' v = case pw of
                            AFP_P -> "(encodeUtf8 " <> v <> ")"
                            AFP_W -> "(encodeUtf16LE " <> v <> ")"
                            _ -> v
          | isChar' v = case pw of
                            AFP_P -> "(fromIntegral . C.ord $ " <> v <> ")"
                            AFP_W -> "(fromIntegral . C.ord $ " <> v <> ")"
                            _ -> v
          | otherwise = v
        isString' xs@('"':_:_) = last xs == '"'
        isString' _ = False
        isChar' xs@('\'':_:_) = last xs == '\''
        isChar' _ = False
        qualifyBS v = case pw of
                        AFP_P -> "SBS." <> v
                        AFP_W -> "SBS16." <> v
                        _ -> v
#else
        encode v
          | isString' v = case pw of
                            AFP_P -> "(" <> v <> ")"
                            AFP_W -> "(" <> v <> ")"
                            _ -> v
          | isChar' v = case pw of
                            AFP_P -> "(PW . fromIntegral . C.ord $ " <> v <> ")"
                            AFP_W -> "(WW . fromIntegral . C.ord $ " <> v <> ")"
                            _ -> v
          | otherwise = v
        isString' xs@('"':_:_) = last xs == '"'
        isString' _ = False
        isChar' xs@('\'':_:_) = last xs == '\''
        isChar' _ = False
        qualifyBS v = case pw of
                        AFP_P
                          | v == "concat" -> "(PS . SBS." <> v <> " . fmap getPosixString)"
                          | v == "any" -> "(\\f (getPosixString -> x) -> SBS." <> v <> " (f . PW) x)"
                          | v == "isPrefixOf" -> "(\\(getPosixString -> x) (getPosixString -> y) -> SBS." <> v <> " x y)"
                          | v == "isSuffixOf" -> "(\\(getPosixString -> x) (getPosixString -> y) -> SBS." <> v <> " x y)"
                          | otherwise -> "(SBS." <> v <> " . getPosixString)"
                        AFP_W
                          | v == "concat" -> "(WS . SBS16." <> v <> " . fmap getWindowsString)"
                          | v == "any" -> "(\\f (getWindowsString -> x) -> SBS16." <> v <> " (f . WW) x)"
                          | v == "isPrefixOf" -> "(\\(getWindowsString -> x) (getWindowsString -> y) -> SBS16." <> v <> " x y)"
                          | v == "isSuffixOf" -> "(\\(getWindowsString -> x) (getWindowsString -> y) -> SBS16." <> v <> " x y)"
                          | otherwise -> "(SBS16." <> v <> " . getWindowsString)"
                        _ -> v
#endif



---------------------------------------------------------------------
-- UTILITIES

writeFileBinary :: FilePath -> String -> IO ()
writeFileBinary file x = withBinaryFile file WriteMode $ \h -> hPutStr h x

readFileBinary' :: FilePath -> IO String
readFileBinary' file = withBinaryFile file ReadMode $ \h -> do
    s <- hGetContents h
    evaluate $ length s
    pure s

writeFileBinaryChanged :: FilePath -> String -> IO ()
writeFileBinaryChanged file x = do
    b <- doesFileExist file
    old <- if b then fmap Just $ readFileBinary' file else pure Nothing
    when (Just x /= old) $
        writeFileBinary file x
