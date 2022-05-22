
{-

use the ./build script to run this easily

todo list:

the package database handling is not good
  -> if there are multiple versions of a package, it can get stuck,
  I had to zap the ~/.cabal dir to unstick it
  -> it's not limiting the cabal packages correctly still

make it rebuild when:
  when the ghc version changes
    -> it gets stuck at the moment
  when the flags for a compile or link step change,
    including the pkg-config python3 stuff
  when the packages are updated
  can use oracles for this
  create a nice command line interface to change the build settings,
    e.g. debug, profile, release, custom ghc flags

nice way to pass args to running the tasty test exes

a way to keep cabal files in sync (used to distribute the
language?)

add some CI testing for the cabal file and for various
versions of ghc, etc.

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Development.Shake
import Development.Shake.FilePath
    ((</>)
    ,dropDirectory1
    ,dropExtension
    ,(-<.>)
    ,makeRelative
    ,(<.>)
    ,takeDirectory)
import Data.List (isPrefixOf
                 ,isSuffixOf
                 ,intercalate
                 ,nub
                 ,sort)

import Data.Typeable (Typeable)
import Data.Hashable (Hashable)
import Data.Binary (Binary)
import Control.DeepSeq (NFData)

import Control.Monad (when, void)
import Development.Shake.Util
    (needMakefileDependencies
    )

import qualified System.Directory as D

testPattern :: Maybe String
testPattern = Nothing -- Just "fact"

newtype Packages = Packages () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult Packages = String

newtype GhcVersion = GhcVersion () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult GhcVersion = String

main :: IO ()
main = do
  shakeArgs shakeOptions{shakeFiles="_build"} $ do

    want ["_build/burdock"]

    ---------------------------------------

    void $ addOracle $ \(GhcVersion _) -> fromStdout <$> cmd "ghc --numeric-version" :: Action String

    ---------------------------------------
      
    -- clean targets
    
    -- clean everything including package databases
    phony "clean-all" $ do
        need ["clean"]
        removeFilesAfter "_build" ["//*"]

    -- clean everything except the package databases, despite
    -- not recompiling anything to rebuild, it still takes cabal
    -- a relatively long time to rebuild just the package dbs
    phony "clean" $ do
        bldFiles <- getDirectoryContents "_build"
        let filesToClean1 = filter (/="packages") bldFiles
        liftIO $ removeFiles "_build" filesToClean1
        liftIO $ removeFiles "." ["dist-newstyle", "src/pywrap/dist-newstyle"]

    ---------------------------------------

    phony "all" $ do
        need ["_build/burdock-tests"
             ,"_build/burdock"
             ,"_build/DemoFFI"]

    phony "test" $ do
        need ["_build/burdock-tests"]
        cmd_ "_build/burdock-tests  --color never --ansi-tricks false --hide-successes"
            (maybe "" (\x -> "-p " ++ x) testPattern)

    ---------------------------------------
    -- dodgy handling of the packagedb
    -- to be fixed

    let installPackageDB :: FilePath -> [String] -> Action ()
        installPackageDB dir pkgs = do
            -- blast anything already there away to keep the deps accurate
            -- it's not painfully slow to rebuild because cabal v2 has cached the builds
            liftIO $ removeFiles "." [dir]
            cmd_ "cabal -j install --lib " pkgs "--package-env" dir

    let directPackages =
            ["tasty"
            ,"tasty-hunit"
            ,"megaparsec"
            ,"scientific"
            ,"prettyprinter"
            ,"text"
            ,"raw-strings-qq"
            ,"mtl"
            ,"safe-exceptions"
            ,"pretty-show"
            ,"haskeline"
            ,"optparse-applicative"
            ,"filepath"
            ,"uniplate"
            ,"stm"
            ,"async"
            ,"exceptions"
            ,"Glob"
            ]
    -- todo: separate packages for the tests, the executable and the lib?
    phony "install-deps" $ installPackageDB "_build/packages/burdock-packages" directPackages
    void $ addOracle $ \(Packages _) -> pure (unlines directPackages) :: Action String

    "_build/packages/burdock-packages" %> \out -> do
        void $ askOracle (Packages ())
        void $ askOracle (GhcVersion ())
        installPackageDB out directPackages

    ---------------------------------------
    -- website

    phony "clean_website" $ do
        liftIO $ removeFiles "_build" ["website"]

    let mkdirP = liftIO . D.createDirectoryIfMissing True

    phony "website" $ do
        mkdirP "_build/website"
        docs <- getDirectoryFiles "" ["docs//*.rst"]
        let htmls = ["_build/website" </> dropDirectory1 (dropExtension doc) -<.> "html" | doc <- docs]
        need ("_build/website/style.css":htmls)

    "_build/website/style.css" %> \out -> do
        copyFile' "docs/style.css" out

    "_build/website/*.html" %> \out -> do
        let rst = "docs" </> dropDirectory1 (dropDirectory1 $ dropExtension out -<.> "rst")
        need [rst, "docs/header.html"]
        cmd_ "pandoc -s -t html -H docs/header.html --toc " rst "-o"  out "--css=style.css"

    ---------------------------------------
    -- testing website

    phony "clean_website2" $ do
        liftIO $ removeFiles "_build" ["website2"]

    phony "website2" $ do
        mkdirP "_build/website2"
        docs <- getDirectoryFiles "" ["website2//*.rst"]
        let htmls = ["_build/website2" </> dropDirectory1 (dropExtension doc) -<.> "html" | doc <- docs]
        need ("_build/website2/style.css":htmls)

    "_build/website2/style.css" %> \out -> do
        copyFile' "website2/style.css" out

    "_build/website2/*.html" %> \out -> do
        let rst = "website2" </> dropDirectory1 (dropDirectory1 $ dropExtension out -<.> "rst")
        need [rst, "website2/header.html"]
        cmd_ "pandoc -s -t html -H website2/header.html --toc " rst "-o"  out "--css=style.css"


    ---------------------------------------
    -- helpers for building haskell

    let parseMakefileDeps :: String -> [(String,String)]
        parseMakefileDeps str =
            let ls = lines str
            in p [] ls
              where
                p acc [] = reverse acc
                p acc (x:xs)
                    | "#" `isPrefixOf` x = p acc xs
                    | otherwise = case words x of
                          [ofile,":",hsfile] -> p ((ofile,hsfile):acc) xs
                          _ -> error $ "didn't understand makefile line: " ++ show x

    ---------------------------------------

    -- build the exes

    -- todo: figure out how to parameterize these into the build rules
    let objsDir = "_build/objs"
        userGhcOpts = "-package-env _build/packages/burdock-packages"
        srcDirs = ["src/lib"
                  ,"src/pywrap"
                  ,"packages/python-ffi/haskell-src"
                  ,"packages/ffitypes-test/haskell-src"
                  ,"packages/sqlite/haskell-src"
                  ,"src/test/"]
        userCCompileOptsLoad = "pkg-config python3-embed --cflags"
        userLinkOptsLoad = "pkg-config --libs python3-embed"

    let compileHaskellExe exeName mainSource cfiles = do
            need ["_build/packages/burdock-packages"]
            -- use the deps file to find out which haskell .o files
            -- are needed to link this exe
            let depsFile = objsDir </> mainSource <.> "deps"
            need [depsFile]
            makefileDeps <- liftIO (readFile depsFile)
            -- parse the deps list into pairs - object file, file it depends on
            let deps = parseMakefileDeps makefileDeps
            let hsobjs = nub $ sort $ map fst deps
            let cobjs = flip map cfiles $ \cfile -> objsDir </> cfile <.> "o"
            let allObjs = hsobjs ++ cobjs
            need allObjs
            Stdout (userLinkOpts :: String) <- cmd userLinkOptsLoad
            --link it
            cmd_ "ghc -o" exeName allObjs userGhcOpts userLinkOpts
   
    -- the three exes
    "_build/burdock" %> \out -> compileHaskellExe out "src/app/BurdockExe.hs" ["src/pywrap/pywrap-c.c"]
    "_build/DemoFFI" %> \out -> compileHaskellExe out "src/examples/DemoFFI.hs" []
    "_build/burdock-tests" %> \out -> compileHaskellExe out "src/test/BurdockTests.hs" ["src/pywrap/pywrap-c.c"]

    let srcDirOption = "-i" ++ intercalate ":" (srcDirs ++ map (objsDir </>) srcDirs)



    
    ---------------------------------------
    -- build rules for haskell and c files

    "_build//*.hs.deps" %> \out -> do
        need ["_build/packages/burdock-packages"]
        -- the dep file needs rebuilding if any of the .hs files it
        -- references have changed
        -- what's the right idiom for this?
        e <- liftIO $ D.doesFileExist out
        when e $ do
            makefileDeps <- liftIO (readFile out)
            let hsFileDeps = filter (".hs" `isSuffixOf`) $ map snd $ parseMakefileDeps makefileDeps
            need hsFileDeps

        -- generate the deps file from ghc
        let hsFile = makeRelative objsDir (dropExtension out)
            ghcdepsfile = out <.> "tmp"
        mkdirP (takeDirectory out)
        void $ askOracle (GhcVersion ())
        cmd_ "ghc -M" hsFile "-dep-makefile" ghcdepsfile "-dep-suffix=hs." userGhcOpts srcDirOption
        makefileDeps <- liftIO (readFile ghcdepsfile)
        let deps = parseMakefileDeps makefileDeps
        let g fn = if ".hs" `isSuffixOf` fn
                   then fn
                   else objsDir </> fn
            deps' = flip map deps $ \(o,f) -> (g o, g f)
            newdeps = unlines $ flip map deps' $ \(a,b) -> unwords [a,":", b]
        -- liftIO $ putStrLn $ "new deps:\n" ++ newdeps
        liftIO $ writeFile out newdeps
                
    ["_build//*.hs.o","_build//*.hs.hi"] &%> \[out,_] -> do
        need ["_build/packages/burdock-packages"]
        let hsfile = (dropExtension $ dropExtension $ makeRelative objsDir out) <.> "hs"
            depsfile = dropExtension out <.> "deps"
        need [depsfile]
        needMakefileDependencies depsfile
        
        let hifile = (dropExtension out) <.> "hi"
        let d = takeDirectory out
        mkdirP d
        void $ askOracle (GhcVersion ())
        cmd_ "ghc -hisuf .hs.hi -c" hsfile "-o" out "-ohi" hifile userGhcOpts srcDirOption
        
    "_build//*.c.o" %> \out -> do
        let cfile = (dropExtension $ dropExtension $ makeRelative objsDir out) <.> "c"
        need [cfile]
        Stdout (userCCompileOpts :: String) <- cmd userCCompileOptsLoad
        mkdirP (takeDirectory out)
        cmd_ "gcc" cfile "-c -o" out userCCompileOpts

