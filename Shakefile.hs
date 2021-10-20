
-- use the build.sh to run this easily
-- todo: nice way to pass args to running the tasty test exes
-- todo: a way to keep cabal files in sync (used to distribute the
-- language)

-- todo: add some CI testing for the cabal file and for various
-- versions of ghc, etc.

import Development.Shake
--import Development.Shake.Command
import Development.Shake.FilePath
--import Development.Shake.Util
import Data.List (isPrefixOf
                 ,isSuffixOf
                 ,intercalate)

data GhcOptions
    = GhcOptions
    { ghcPackages :: Maybe FilePath
    , ghcSrcs :: [FilePath]
    }

ghcOpts :: GhcOptions
ghcOpts = GhcOptions Nothing []

testPattern :: Maybe String
testPattern = Nothing -- Just "fact"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do

    let installPackageDB :: FilePath -> [String] -> Action ()
        installPackageDB dir pkgs = do
            -- blast anything already there away to keep the deps accurate
            -- it's not painfully slow to rebuild because cabal v2 has cached the builds
            cmd_ "rm -Rf" dir
            cmd_ "cabal install --lib " pkgs "--package-env" dir

    -- todo: separate packages for the tests, the executable and the lib
    phony "install-deps" $ do
        installPackageDB "_build/burdock-packages"
            ["tasty"
            ,"tasty-hunit"
            ,"megaparsec"
            ,"scientific"
            ,"prettyprinter"
            ,"text"
            ,"raw-strings-qq"
            ,"mtl"
            ,"safe-exceptions"]

    let ghc :: GhcOptions -> FilePath -> FilePath -> Action ()
        ghc opts src output = do
            let blddir = output ++ "-build"
            cmd_ "mkdir -p" blddir
            let srcpath = "-i" ++ takeDirectory src
            cmd_ "ghc -threaded -Wall --make" src "-outputdir=" blddir
                 "-o" output
                 (maybe [] (\x -> ["-package-env",x]) (ghcPackages opts))
                 (case ghcSrcs opts of
                      [] -> []
                      x -> ["-i" ++ intercalate ":" x])
                 srcpath

    -- clean everything including package databases
    phony "clean-all" $ do
        removeFilesAfter "_build" ["//*"]


    -- clean everything except the package databases, despite
    -- not recompiling anything to rebuild, it still takes cabal
    -- a relatively long time to rebuild just the package dbs
    phony "clean" $ do
        bldFiles <- getDirectoryContents "_build/"
        let filesToClean1 = map ("_build" </>)
                           --  $ filter (/= "bin")
                           $ filter (not . ("-packages" `isSuffixOf`))
                           $ filter (not . ("." `isPrefixOf`)) bldFiles
        cmd_ "rm -Rf" filesToClean1

    -- todo: use ghc -M to do this better
    let needHsFiles dir = do
            hs <- getDirectoryFiles dir ["//*.hs"]
            let hs' = map (dir </>) hs
            need hs'

    "_build/bin/burdock-tests" %> \out -> do
        needHsFiles "src"
        ghc (ghcOpts {ghcPackages = Just "_build/burdock-packages"})
            "src/BurdockTests.hs"
            out
    
    phony "test" $ do
        need ["_build/bin/burdock-tests"]
        cmd_ "_build/bin/burdock-tests  --color never --ansi-tricks false"
            (maybe "" (\x -> "-p " ++ x) testPattern)
