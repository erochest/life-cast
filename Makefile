
CABAL=cabal
FLAGS=--enable-tests

all: init test docs package

init:
	${CABAL} sandbox init
	make deps

test: build
	${CABAL} test

specs: build
	./dist/build/life-case-specs/life-case-specs

run:
	${CABAL} run

# docs:
# generate api documentation
#
# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# install:
# generate executable and put it into `/usr/local`
#
# deploy:
# prep and push

tags: Life.hs
	hasktags --ctags Life.hs

hlint:
	hlint Life.hs specs

clean:
	${CABAL} clean

distclean: clean
	${CABAL} sandbox delete

configure: clean
	${CABAL} configure ${FLAGS}

deps: clean
	${CABAL} install --only-dependencies --allow-newer ${FLAGS}
	make configure

build:
	${CABAL} build

restart: distclean init build

rebuild: clean configure build

.PHONY: all init test run clean distclean configure deps build rebuild hlint
