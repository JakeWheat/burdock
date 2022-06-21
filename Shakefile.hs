{-

use the ./build script to run this easily

to view the flags to the build system, and to view the build targets:

./build --help

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

import Control.Monad (when, void, forM_)
import Development.Shake.Util
    (needMakefileDependencies
    )

import qualified System.Directory as D

import System.Exit (ExitCode(ExitSuccess))


newtype Packages = Packages () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult Packages = String

newtype BurdockVersion = BurdockVersion () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult BurdockVersion = String

main :: IO ()
main = do
  shakeArgs shakeOptions{shakeFiles="_build"} $ do

    want ["_build/burdock"]

    ------------------------------------------------------------------------------

    -- main declarative part, making it more declarative is a WIP

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

    let mainPackageDB = "_build/packages/burdock-packages"

    let objsDir = "_build/objs"
        userGhcOpts = "-v0 -hide-all-packages -package-env " ++ mainPackageDB
        srcDirs = ["src/lib"
                  ,"src/pywrap"
                  ,"src/packages/python-ffi/haskell-src"
                  ,"src/packages/ffitypes-test/haskell-src"
                  ,"src/test/"
                  ,"_build/generated-hs/"
                  ]
        userCCompileOptsLoad = "pkg-config python3-embed --cflags"
        userLinkOptsLoad = "pkg-config --libs python3-embed"
        pythonCFiles = ["src/pywrap/pywrap-c.c"]
        exes = [("build the Burdock interpreter"
                ,"_build/burdock"
                ,"src/app/BurdockExe.hs"
                ,["_build/generated-hs/Burdock/GeneratedBuiltins.hs"]
                ,pythonCFiles
                ,"-threaded")
               ,("build the Burdock interpreter without threading (temporary for Python issues)"
                ,"_build/burdock-unthreaded"
                ,"src/app/BurdockExe.hs"
                ,[]
                ,pythonCFiles
                ,"")                
               ,("build the Burdock tests"
                ,"_build/burdock-hunit-tests"
                ,"src/test/BurdockTests.hs"
                ,[]
                ,pythonCFiles
                ,"-threaded")
               ,("build the Demo user FFI interpreter"
                ,"_build/DemoFFI"
                ,"src/examples/DemoFFI.hs"
                ,[]
                ,[]
                ,"-threaded")
               ,("build the Demo user FFI interpreter"
                ,"_build/GenerateBuiltinsFile"
                ,"src/build/GenerateBuiltinsFile.hs"
                ,[]
                ,[]
                ,"-threaded")
               ]
        allTargetExes = map (\(_,e,_,_,_,_) -> e) exes
        simplePhonies =
            [("build all the exes", "all", allTargetExes)
            ,("build all the exes, build the website, run the tests, build the release"
             ,"really-all"
             ,["all", "website", "test-all", "release-build"])
            ]

    ------------------------------------------------------------------------------

    -- support functions

    let srcDirOption = "-i" ++ intercalate ":" (srcDirs ++ map (objsDir </>) srcDirs)

    let mkdirP = liftIO . D.createDirectoryIfMissing True

    let installPackageDB :: FilePath -> [String] -> Action ()
        installPackageDB dir pkgs = do
            -- blast anything already there away to keep the deps accurate
            -- it's not painfully slow to rebuild because cabal v2 has cached the builds
            liftIO $ removeFiles "." [dir]
            cmd_ "cabal -v0 -j install --lib " pkgs "--package-env" dir

    let doPandoc header rst out =
            cmd_ "pandoc -s -t html -H" header "--toc" rst "-o" out "--css=style.css"

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

    -- cache running a command and saving the output
    -- this is for things that aren't persisted between builds
    -- but can be memoized for a single build
    cacheOptionsLoad <- newCache $ \(co :: String) -> do
        Stdout (res :: String) <- cmd co
        pure res
    
    -- extract all the .o file references from a dep makefile generated
    -- by ghc -> so you pass ghc the main.hs, and then use this on the
    -- output, to get a complete list of all the .o files from .hs files
    --- that are needed to link that main.hs file
    let getAllObjsFromGhcDeps makefileDepsString =
            let deps = parseMakefileDeps makefileDepsString
            in nub $ sort $ map fst deps
    let compileHaskellExe exeName mainSource generatedSources cfiles additionalLinkOpts = do
            need [mainPackageDB]
            need generatedSources
            -- get all the haskell files needed for linking using the deps
            let depsFile = objsDir </> mainSource <.> "deps"
            need [depsFile]
            makefileDeps <- liftIO (readFile depsFile)
            let hsobjs = getAllObjsFromGhcDeps makefileDeps
                cobjs = flip map cfiles $ \cfile -> objsDir </> cfile <.> "o"
                allObjs = hsobjs ++ cobjs
            need allObjs
            userLinkOpts <- cacheOptionsLoad userLinkOptsLoad
            cmd_ "ghc -Wall -o" exeName allObjs userGhcOpts userLinkOpts additionalLinkOpts

    -- if the file exists, read it and pass to the function
    let doIfFileExists fn f = do
            e <- liftIO $ D.doesFileExist fn
            when e (f =<< liftIO (readFile fn))

    let generateVersionString full = do
            -- read the base version file
            v <- liftIO $ readFile "version"
            if not full
                then pure v
                else do
                -- get the git commit
                (Exit c, Stdout out) <- cmd "git rev-parse HEAD"
                let ghash = case c of
                        ExitSuccess -> take 6 out
                        -- doesn't work if git isn't found
                        _ -> "NOGIT"
                Exit c1 <- cmd "git diff-index --quiet HEAD"
                let gd = case c1  of
                        ExitSuccess -> ""
                        _ -> "-changed"
                -- determine if any files have been changed, roughtly
                -- put a different string if there's no git repo
                pure $ v ++ "-" ++ ghash ++ gd
    
    ------------------------------------------------------------------------------

    -- build rules

    forM_ exes $ \(doc,outnm,hsmain,generatedSources,cfiles, linkOpts) ->
        withTargetDocs doc $ outnm %> \out -> compileHaskellExe out hsmain generatedSources cfiles linkOpts

    withTargetDocs "create release tarball" $
        "_build/burdock.tar.gz" %> \out -> do
        fs <- getDirectoryFiles "" ["src//*.c", "src//*.hs", "src/packages//*.hs"]
        
        -- add generated files
        need $ ["LICENSE", "README", "cabal/burdock.cabal"] ++ fs
          ++ ["_build/generated-hs/Burdock/GeneratedBuiltins.hs"
             ,"_build/generated-hs/Burdock/Version.hs"]

        let cabalBuildDir = "_build/cabal-sdist"
        mkdirP cabalBuildDir
        Stdout t <- cmd "sh cabal/makecabalsdist.sh"
        case lines t of
            [_,nm] -> liftIO $ do
                --putStrLn $ "'''" ++ nm ++ "'''"
                ver <- generateVersionString True
                let nm' = "_build/burdock-" ++ ver <.> "tar.gz"
                D.renameFile nm nm'
                D.copyFile nm' out
            _ -> error $ "didn't understand output of cabal sdist: " ++ t

    withTargetDocs "build from release source" $ phony "release-build" $ do
        need ["_build/burdock.tar.gz"]
        mkdirP "_build/cabal-build"
        cmd_ "sh cabal/buildfromcabaltarball.sh"
        ver <- generateVersionString True
        liftIO $ D.copyFile "_build/cabal-build/burdock" ("_build/burdock-release-" ++ ver)


    forM_ simplePhonies $ \(doc, nm, needage) ->
        withTargetDocs doc $ phony nm $ need needage

    withTargetDocs "run the tests" $ phony "test-all" $ do
        need ["_build/burdock-hunit-tests"
             ,"_build/burdock"
             -- temporary hack while python is incompatible threads
             ,"_build/burdock-unthreaded"]
        cmd_ "_build/burdock-hunit-tests --color never --ansi-tricks false --hide-successes"
        cmd_  "_build/burdock-unthreaded src/burdock/tests/run-all-tests-additional.bur"
        cmd_  "_build/burdock src/burdock/tests/run-all-tests.bur"

    withTargetDocs "run the tests" $ phony "test" $ do
        need ["_build/burdock"]
        cmd_  "_build/burdock src/burdock/tests/run-all-tests.bur"

    withTargetDocs "build the website" $ phony "website" $ do
        mkdirP "_build/website"
        docs <- getDirectoryFiles "" ["docs//*.rst"]
        let htmls = ["_build/website" </> dropDirectory1 (dropExtension doc) -<.> "html"
                    | doc <- docs]
            websiteFiles = "_build/website/style.css" : htmls
        -- testing website
        mkdirP "_build/website2"
        docs2 <- getDirectoryFiles "" ["website2//*.rst"]
        let htmls2 = ["_build/website2" </> dropDirectory1 (dropExtension doc) -<.> "html"
                     | doc <- docs2]
            website2Files = if null htmls2
                            then []
                            else ("_build/website2/style.css" : htmls2)
        need (websiteFiles ++ website2Files)

    withTargetDocs "clean everything that any of the targets creates"
        $ phony "clean" $ do
        liftIO $ removeFiles "." ["_build", "dist-newstyle", "src/pywrap/dist-newstyle"]

    withTargetDocs "build the cabal package db for the haskell code"
        $ mainPackageDB %> \out -> do
        void $ askOracle (Packages ())
        installPackageDB out directPackages

    -- question: would this work better if generatedbuiltins.hs was
    -- imported into this build file and used directly?
    withTargetDocs "build generated built-ins" $
        "_build/generated-hs/Burdock/GeneratedBuiltins.hs" %> \out -> do
        fs <- getDirectoryFiles "" ["src/burdock/built-ins//*.bur"
                                   ,"src/packages/python-ffi/bur//*.bur"
                                   ,"src/packages/ffitypes-test/bur//*.bur"]
        need ("_build/GenerateBuiltinsFile" : fs)
        cmd_ "_build/GenerateBuiltinsFile " out fs

    withTargetDocs "generated version file" $
        "_build/generated-hs/Burdock/Version.hs" %> \out -> do
        str <- askOracle (BurdockVersion ())
        liftIO $ writeFile out $ "module Burdock.Version where\nversion :: String\nversion = " ++ show str
  
    withTargetDocs "auto haskell deps" $
        "_build//*.hs.deps" %> \out -> do
        need [mainPackageDB]
        -- hack for the generated file
        -- todo: find a way to do this more automatically and robustly
        when (out `elem` ["_build/objs/src/app/BurdockExe.hs.deps"
                         ,"_build/objs/src/examples/DemoFFI.hs.deps"
                         ,"_build/objs/src/test/BurdockTests.hs.deps"
                         ]) $
             need ["_build/generated-hs/Burdock/GeneratedBuiltins.hs"
                  ,"_build/generated-hs/Burdock/Version.hs"]
        -- the dep file needs rebuilding if any of the .hs files it
        -- references have changed
        -- is this the right idiom for this?
        doIfFileExists out $ \makefileDeps -> 
            let hsFileDeps = filter (".hs" `isSuffixOf`) $ map snd $ parseMakefileDeps makefileDeps
            in need hsFileDeps

        -- generate the deps file from ghc
        let hsFile = makeRelative objsDir (dropExtension out)
            ghcdepsfile = out <.> "tmp"
        mkdirP (takeDirectory out)
        cmd_ "ghc -M" hsFile "-dep-makefile" ghcdepsfile "-dep-suffix=hs." userGhcOpts srcDirOption
        makefileDeps <- liftIO (readFile ghcdepsfile)
        -- todo: helper function for this:
        let deps = parseMakefileDeps makefileDeps
            g fn = if ".hs" `isSuffixOf` fn
                   then fn
                   else objsDir </> fn
            deps' = flip map deps $ \(o,f) -> (g o, g f)
            newdeps = unlines $ flip map deps' $ \(a,b) -> unwords [a,":", b]
        ----
        liftIO $ writeFile out newdeps
                
    withTargetDocs "compile hs file" $ ["_build//*.hs.o","_build//*.hs.hi"] &%> \case
        [out,_] -> do
            need [mainPackageDB]
            let hsfile = (dropExtension $ dropExtension $ makeRelative objsDir out) <.> "hs"
                depsfile = dropExtension out <.> "deps"
            need [depsfile]
            needMakefileDependencies depsfile
            let hifile = (dropExtension out) <.> "hi"
                d = takeDirectory out
            mkdirP d
            cmd_ "ghc -Wall -hisuf .hs.hi -c" hsfile "-o" out "-ohi" hifile userGhcOpts srcDirOption
        _ -> error "impossible build failure: matched two items in rule but didn't get two items to build"

    withTargetDocs "compile c file" $ "_build//*.c.o" %> \out -> do
        let cfile = (dropExtension $ dropExtension $ makeRelative objsDir out) <.> "c"
        need [cfile]
        userCCompileOpts <- cacheOptionsLoad userCCompileOptsLoad
        mkdirP (takeDirectory out)
        cmd_ "gcc" cfile "-c -o" out userCCompileOpts

    withTargetDocs "copy website file" $
        "_build/website/style.css" %> \out -> copyFile' "docs/style.css" out

    withTargetDocs "build website html from rst" $
        "_build/website/*.html" %> \out -> do
            let rst = "docs" </> dropDirectory1 (dropDirectory1 $ dropExtension out -<.> "rst")
            need [rst, "docs/header.html"]
            doPandoc "docs/header.html" rst out

    withTargetDocs "copy website file" $
        "_build/website2/style.css" %> \out -> copyFile' "website2/style.css" out

    withTargetDocs "build website html from rst" $ "_build/website2/*.html" %> \out -> do
        let rst = "website2" </> dropDirectory1 (dropDirectory1 $ dropExtension out -<.> "rst")
        need [rst, "website2/header.html"]
        doPandoc "website2/header.html" rst out

    void $ addOracle $ \(Packages {}) -> pure (unlines directPackages) :: Action String
    void $ addOracle $ \(BurdockVersion {}) -> generateVersionString True :: Action String

