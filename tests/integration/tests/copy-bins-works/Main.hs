-- | Stack's build command supports the copy-bins flag.

import StackTest
import System.Directory ( createDirectoryIfMissing )

main :: IO ()
main = do
  createDirectoryIfMissing True "bin1"
  stack ["build", "--copy-bins", "--local-bin-path", "bin1"]
  doesExist ("bin1/" <> myPackageExe)
  createDirectoryIfMissing True "bin2"
  stack ["--stack-yaml", "stack-copy-bins.yaml", "build", "--local-bin-path", "bin2"]
  doesExist ("bin2/" <> myPackageExe)
 where
  myPackageExe = "myExe" <> exeExt
