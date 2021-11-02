#!/usr/bin/env bash

# rebuild the shake exe if needed, then run it with the args passed to
# this script

mkdir -p _build/shakefile-build
mkdir -p _build/bin

# optimisation, because cabal is really slow if the package is
# already installed
if [ ! -f _build/shakefile-packages ]; then
    echo installing shake lib for bootstrapping build exe
    cabal -j install --lib shake --package-env _build/shakefile-packages
fi

ghc -Wall --make Shakefile.hs -rtsopts -threaded -with-rtsopts=-I0 -outputdir=_build/shakefile-build -o _build/bin/shakefile -package-env _build/shakefile-packages

_build/bin/shakefile +RTS -N -RTS "$@"
