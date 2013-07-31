.PHONY: test build demo

default: test

test:
	cabal-dev configure --enable-tests && cabal-dev build && cabal-dev test

build:
	cabal-dev configure && cabal-dev build

dev:
	cabal-dev install-deps

ghci:
	cabal-dev configure && cabal-dev build && cabal-dev ghci


demo:
	cabal configure -f demo && cabal build
