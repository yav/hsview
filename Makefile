.PHONY: default

default: build cabal.sandbox.config
	cabal install

build cabal.sandbox.config:
	cabal sandbox --sandbox=build init


