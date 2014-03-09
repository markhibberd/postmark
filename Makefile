MFLAGS = -s
MAKEFLAGS = $(MFLAGS)
SANDBOX = .cabal-sandbox

.PHONY: test quick-test build deps ghci demo

default: test

${SANDBOX}:
	cabal sandbox init

test: ${SANDBOX}
	cabal configure --enable-tests && cabal build && cabal test

demo: ${SANDBOX}
	cabal configure -f demo && cabal build

quick-test: ${SANDBOX}
	runhaskell -package-db=${SANDBOX}/*-packages.conf.d -isrc -itest -XNoImplicitPrelude -XNoMonomorphismRestriction test/testdev.hs

build: ${SANDBOX}
	cabal configure && cabal build

deps: ${SANDBOX}
	cabal install --only-dependencies

repl: ${SANDBOX}
	cabal configure && cabal build && cabal repl
