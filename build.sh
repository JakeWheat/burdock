#!/usr/bin/env bash

set -e

# rebuild the shake exe if needed, then run it with the args passed to
# this script

mkdir -p _build/packages

# optimisation, because cabal is really slow if the package is
# already installed
if [ ! -f _build/packages/shakefile-packages ]; then
    echo installing shake lib for bootstrapping build exe
    cabal -j install --lib shake --package-env _build/packages/shakefile-packages
fi

ghc -Wall --make Shakefile.hs -rtsopts -threaded -with-rtsopts=-I0 -outputdir=_build/objs/shakefile-build -o _build/shakefile -package-env _build/packages/shakefile-packages

_build/shakefile +RTS -N -RTS -j "$@"
