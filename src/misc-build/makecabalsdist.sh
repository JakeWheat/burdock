cd "_build/cabal-sdist"
cp ../../src/misc-build/burdock.cabal .
rm -f LICENSE
ln -s ../../LICENSE .
rm -f README
ln -s ../../README .
rm -f src
ln -s ../../src .
mkdir -p generated-src/Burdock
(cd generated-src/Burdock;
 rm -f GeneratedBuiltins.hs
 ln -s ../../../generated-hs/Burdock/GeneratedBuiltins.hs .
 rm -f Version.hs
 ln -s ../../../generated-hs/Burdock/Version.hs . )
# the shakefile needs the output from cabal sdist to find the tarball
cabal sdist
