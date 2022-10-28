.PHONY: \
  cabal.config \

cabal.config: stack.cabal stack.yaml stack-macos.yaml
	./etc/scripts/ls-deps-as-cabal-constraints.sh $@