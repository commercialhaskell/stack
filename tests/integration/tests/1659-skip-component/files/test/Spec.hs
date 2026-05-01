{-# LANGUAGE CPP #-}

main :: IO ()
main = pure ()

-- Avoid problems with CPP and HLint
#ifndef __HLINT__

#error Not going to compile, sorry

#endif
