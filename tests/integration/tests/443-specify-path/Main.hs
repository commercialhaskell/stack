-- Stack can specify the directory to which built executable binary files are
-- copied.
--
-- See: https://github.com/commercialhaskell/stack/issues/443

import StackTest
import System.Directory ( createDirectory, getCurrentDirectory )
import System.FilePath ( (</>) )

main :: IO ()
main = do
  -- Default install
  -- A manual test of the default stack install is required

  -- Install in current dir
  stack [ "--local-bin-path", ".", "install" ]
  doesExist myPackageExe

  -- Install in relative path
  createDirectory "bin"
  stack [ "--local-bin-path", "./bin", "install" ]
  doesExist ("./bin/" <> myPackageExe)

  -- Install in absolute path
  tmpDirectory <- fmap (</> "bin-absolute") getCurrentDirectory
  createDirectory tmpDirectory
  stack [ "--local-bin-path", tmpDirectory, "install" ]
  doesExist (tmpDirectory </> myPackageExe)
 where
  myPackageExe = "myPackage" <> exeExt
