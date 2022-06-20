cd _build/cabal-build
tar xf ../burdock.tar.gz
cd burdock-0.1
cabal install --overwrite-policy=always --installdir ..
# todo: how can you get cabal install to install directly from the
# tarball?
