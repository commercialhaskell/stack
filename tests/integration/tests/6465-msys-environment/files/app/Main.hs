module Main (main) where

import Foreign.C.Types

foreign import ccall "gsl/gsl_sf_bessel.h gsl_sf_bessel_J0"
  gsl_sf_bessel_J0 :: CDouble -> CDouble

main :: IO ()
main = do
  let x = CDouble 5.0
      result = gsl_sf_bessel_J0 x
  putStrLn $ "J0(" ++ show x ++ ") = " ++ show result
