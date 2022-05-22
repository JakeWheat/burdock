
{-

use the ./build script to run this easily

todo list:

make it rebuild when:
  when the ghc version changes
    -> it gets stuck at the moment, better than silently failing
  when the flags for a compile or link step change,
    including the pkg-config python3 stuff

create a nice command line interface to change the build settings,
  e.g. debug, profile, release, custom ghc flags

nice way to pass args to running the tasty test exes

add some CI testing for various versions of ghc, etc.
write some tests to test this specific build script to catch
  some regressions more easily and promptly

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

    ------------------------------------------------------------------------------

    -- main tweakables

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

        -- todo: figure out how to parameterize these into the build rules
    let objsDir = "_build/objs"
        userGhcOpts = "-v0 -hide-all-packages -package-env " ++ mainPackageDB
        srcDirs = ["src/lib"
                  ,"src/pywrap"
                  ,"packages/python-ffi/haskell-src"
                  ,"packages/ffitypes-test/haskell-src"
                  ,"packages/sqlite/haskell-src"
                  ,"src/test/"]
        userCCompileOptsLoad = "pkg-config python3-embed --cflags"
        userLinkOptsLoad = "pkg-config --libs python3-embed"


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

    let compileHaskellExe exeName mainSource cfiles = do
            need [mainPackageDB]
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

    ------------------------------------------------------------------------------

    -- build rules

    "_build/burdock" %> \out ->
        compileHaskellExe out "src/app/BurdockExe.hs" ["src/pywrap/pywrap-c.c"]
    "_build/burdock-tests" %> \out ->
        compileHaskellExe out "src/test/BurdockTests.hs" ["src/pywrap/pywrap-c.c"]
    "_build/DemoFFI" %> \out -> compileHaskellExe out "src/examples/DemoFFI.hs" []

    phony "all" $ need ["_build/burdock-tests"
                       ,"_build/burdock"
                       ,"_build/DemoFFI"]

    phony "test" $ do
        need ["_build/burdock-tests"]
        cmd_ "_build/burdock-tests  --color never --ansi-tricks false --hide-successes"
            (maybe "" (\x -> "-p " ++ x) testPattern)

    phony "website" $ do
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

    phony "really-all" $ need ["all", "website", "test"]

    phony "clean" $ do
        removeFilesAfter "_build" ["//*"]
        liftIO $ removeFiles "." ["dist-newstyle", "src/pywrap/dist-newstyle"]

    mainPackageDB %> \out -> do
        void $ askOracle (Packages ())
        void $ askOracle (GhcVersion ())
        installPackageDB out directPackages

    "_build//*.hs.deps" %> \out -> do
        need [mainPackageDB]
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
                
    ["_build//*.hs.o","_build//*.hs.hi"] &%> \case
      [out,_] -> do
        need [mainPackageDB]
        let hsfile = (dropExtension $ dropExtension $ makeRelative objsDir out) <.> "hs"
            depsfile = dropExtension out <.> "deps"
        need [depsfile]
        needMakefileDependencies depsfile
        
        let hifile = (dropExtension out) <.> "hi"
        let d = takeDirectory out
        mkdirP d
        void $ askOracle (GhcVersion ())
        cmd_ "ghc -hisuf .hs.hi -c" hsfile "-o" out "-ohi" hifile userGhcOpts srcDirOption
      _ -> error "impossible build failure: matched two items in rule but didn't get two items to build"

    "_build//*.c.o" %> \out -> do
        let cfile = (dropExtension $ dropExtension $ makeRelative objsDir out) <.> "c"
        need [cfile]
        Stdout (userCCompileOpts :: String) <- cmd userCCompileOptsLoad
        mkdirP (takeDirectory out)
        cmd_ "gcc" cfile "-c -o" out userCCompileOpts

    "_build/website/style.css" %> \out -> do
        copyFile' "docs/style.css" out

    "_build/website/*.html" %> \out -> do
        let rst = "docs" </> dropDirectory1 (dropDirectory1 $ dropExtension out -<.> "rst")
        need [rst, "docs/header.html"]
        doPandoc "docs/header.html" rst out

    "_build/website2/style.css" %> \out -> do
        copyFile' "website2/style.css" out

    "_build/website2/*.html" %> \out -> do
        let rst = "website2" </> dropDirectory1 (dropDirectory1 $ dropExtension out -<.> "rst")
        need [rst, "website2/header.html"]
        doPandoc "website2/header.html" rst out

    void $ addOracle $ \(GhcVersion _) -> fromStdout <$> cmd "ghc --numeric-version" :: Action String
    void $ addOracle $ \(Packages _) -> pure (unlines directPackages) :: Action String
