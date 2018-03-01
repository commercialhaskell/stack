default:  Hack.hs
	stack ghc --package stack  -- --make -isrc -isubs/rio/src -hide-package=cryptohash-0.11.9 -hide-package=rio Hack.hs
