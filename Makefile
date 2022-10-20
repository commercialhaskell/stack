GHC := 9.2.4

.PHONY: \
  cabal-config \

cabal-config: stackage/ghc-$(GHC)/cabal.config

stackage/%/cabal.config: stack.cabal stack.yaml stack-macos.yaml
	./cabal.sh $@