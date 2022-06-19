
{-

executable front end
allows executing scripts and also has a repl

running using !#
!#/usr/bin/env burdock

running the repl

running a script:
burdock my_script.bur

running a script written on the command line:
burdock -c "some source code text"

run a script's tests:

burdock my_script.bur --run-tests

This will work better if my_script.bur exits promptly

-}


{-# LANGUAGE ScopedTypeVariables #-}
import qualified Burdock.Interpreter as B

import Control.Monad.Trans
import Control.Monad (void)
import System.Console.Haskeline (InputT
                                ,Interrupt
                                ,historyFile
                                ,withInterrupt
                                ,getInputLine
                                ,runInputT
                                ,defaultSettings
                                --,catch
                                )
import Control.Exception.Safe
    (catch
    ,catchAny
    --,SomeException
    ,displayException
    )


import Options.Applicative (Parser
                           ,strOption
                           ,long
                           ,metavar
                           ,help
                           ,short
                           --,option
                           --,showDefault
                           --,switch
                           --,auto
                           --,value
                           ,execParser
                           ,info
                           ,(<**>)
                           ,helper
                           ,fullDesc
                           ,progDesc
                           ,header
                           ,ParserInfo
                           ,optional
                           ,argument
                           ,str
                           ,switch
                           --,some
                           ,many
                           )
import System.IO (Handle
                 ,hIsTerminalDevice
                 ,stdin
                 ,hGetContents)

import System.Exit (exitFailure)

--import Control.Concurrent.Async (withAsync, wait)
import Control.Exception.Safe (bracket)

-- temp? include all packages in the default interpreter build
import qualified FFITypesTest
import qualified PythonFFI

------------------------------------------------------------------------------


runFile :: FilePath -> TestOptions -> IO ()
runFile fp rTests = do
    src <- readFile fp
    runSrc False (Just fp) rTests src

runHandle :: FilePath -> Handle -> TestOptions -> IO ()
runHandle fp h rTests = do
    src <- hGetContents h
    runSrc False (Just fp) rTests src

runSrc :: Bool -> Maybe String -> TestOptions -> String -> IO ()
runSrc lit fnm (rTests,rallTests,hideSuccesses,hideProgress,hideTestResults) src =
    bracket B.newHandle B.closeHandle $ \h -> do
    addPackages h
    flip catch (handleEx h) $ do
        if rTests || rallTests
            then do
                x <- B.runScript h Nothing [("fn", B.TextV $ maybe "" id fnm)
                                           ,("src", B.TextV src)
                                           ,("spl", B.BoolV $ not hideProgress)
                                           ,("hs", B.BoolV hideSuccesses)
                                           ,("apr", B.BoolV $ not hideTestResults)]
                    "include testing\n\
                    \rs = run-tests-with-opts(\n\
                    \         default-test-opts.{\n\
                    \             test-source:source-test(fn, src),\n\
                    \             show-progress-log:spl,\n\
                    \             hide-successes:hs,\n\
                    \             auto-print-results:apr\n\
                    \             })\n\
                    \has-test-failures(rs)"
                case x of
                    B.BoolV False -> pure ()
                    B.BoolV True -> exitFailure
                    _ -> error $ "expected has-test-failures to return bool, got " ++ show x
            else do
                v <- (if lit then B.runLiterateScript else B.runScript) h fnm [] src
                pv <- B.valueToStringIO h v
                case pv of
                    Nothing -> pure ()
                    Just s -> putStrLn s
  where
    handleEx :: B.Handle -> B.InterpreterException -> IO ()
    handleEx h ex = do
        x <- B.formatException h True ex
        putStrLn $ "Error: " ++ x
        exitFailure

runLiterateFile :: FilePath -> TestOptions -> IO ()
runLiterateFile fp rTests = do
    src <- readFile fp
    runSrc True (Just fp) rTests src

-- repl

process :: B.Handle -> B.Value -> String -> IO ()
process h replH src =
    void (B.runScript
             h
             Nothing [("handle", replH)
                     ,("src", B.TextV src)]
             "include repl\n\
             \repl-execute(handle,src)")
    `catch` (\e -> do
        x <- B.formatException h True e
        putStrLn $ "Error: " ++ x)
    `catchAny` (\e -> putStrLn $ "Error: " ++ displayException e)

repl :: B.Handle -> B.Value -> InputT IO ()
repl h replH = go
  where
    go = withInterrupt (do
        minput <- getInputLine "b > "
        case minput of
            Nothing -> pure ()
            Just input -> do
                liftIO $ process h replH input
                go)
        -- ctrl-c resets to the prompt, doesn't exit the repl
        `catch` (\(_::Interrupt) -> liftIO (putStr "^C") >> go)


doRepl :: IO ()
doRepl = bracket B.newHandle B.closeHandle $ \h -> do
    addPackages h
    replH <- B.runScript h Nothing []
        "include repl\n\
        \create-repl()"
    --putStrLn "test print"
    --a1 <- B.evalExpr h Nothing [] "1"
    --a2 <- B.evalExpr h Nothing [] "true"
    --B.evalFun h "print" [a1]
    --B.evalFun h "print" [a2]
    runInputT st (repl h replH)
  where
    st = defaultSettings {historyFile = Just ".burdockreplhistory"}

addPackages :: B.Handle -> IO ()
addPackages h = do
    B.addFFIPackage h "packages/ffitypes-test" FFITypesTest.ffiTypesFFIPackage
    B.addFFIPackage h "packages/python-ffi" PythonFFI.pythonFFIPackage

------------------------------------------------------------------------------

data MyOpts = MyOpts
  { file :: Maybe String
  , script :: Maybe String
  , runTests :: Bool
  , runAllTests :: Bool
  , hideTestSuccesses :: Bool
  , hideTestProgress :: Bool
  , hideTestResults :: Bool
  , literateMode :: Bool
  , userArgs :: [String]
  }
  deriving Show

myOpts :: Parser MyOpts
myOpts = MyOpts
      <$> optional (argument str (metavar "FILE"))
      <*> optional (strOption
          (short 'c'
           <> metavar "SOURCE"
           <> help "inline code to run"))
      {-<*> option auto
          (long "test-level"
           <> value 1
           <> metavar "INT"
           <> help "test-level 0 = skip, 1= one line, 2 = show failures, 3 = show all")-}
      <*> switch (long "run-tests" <> help "run tests")
      <*> switch (long "run-all-tests" <> help "run all tests (including included modules)")
      <*> switch (long "hide-tests-successes" <> help "only show test failures")
      <*> switch (long "hide-tests-progress" <> help "don't show tests as they are run")
      <*> switch (long "hide-test-results" <> help "don't show any test results, only set non zero exit code if there were any failures")
      <*> switch (long "literate-mode" <> help "parse from literate source")
      <*> many (argument str (metavar "ARGS..." <> help "args to pass to Burdock script"))

myOptsPlus :: ParserInfo MyOpts
myOptsPlus = info (myOpts <**> helper)
      ( fullDesc
     <> progDesc "Burdock script runner and repl, docs at http://jakewheat.github.io/burdock/latest/"
     <> header "Burdock interpreter" )

------------------------------------------------------------------------------

main :: IO ()
main = do
    --B.setNumCapabilities =<< B.getNumProcessors
    -- avoid bound thread, possible that it makes a performance difference
    --withAsync doit wait
    doit
  where
    doit = do
        os <- execParser myOptsPlus
        isTTY <- hIsTerminalDevice stdin
        case os of
            MyOpts {file = Just {}, script = Just {}} -> error "please pass either a file or code to run, not both"
            MyOpts {file = Just f, literateMode = True} -> runLiterateFile f (extractTestOptions os)
            MyOpts {file = Just f} -> runFile f (extractTestOptions os)
            MyOpts {script = Just c} -> runSrc False Nothing (extractTestOptions os) c
            MyOpts {script = Nothing, file = Nothing}
                | not isTTY -> runHandle "stdin" stdin (extractTestOptions os)
                | otherwise -> doRepl
    extractTestOptions os =
        (runTests os
        ,runAllTests os
        ,hideTestSuccesses os
        ,hideTestProgress os
        ,hideTestResults os)

type TestOptions = (Bool, Bool, Bool, Bool, Bool)
