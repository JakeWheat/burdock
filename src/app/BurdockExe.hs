
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

This probably won't work unless my_script.bur usually exits promptly


-}


{-# LANGUAGE ScopedTypeVariables #-}
import qualified Burdock.Interpreter as B

import Control.Monad.Trans
import Control.Monad (when, void)
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
import qualified Sqlite
import qualified PythonFFI

------------------------------------------------------------------------------


runFile :: FilePath -> Bool -> IO ()
runFile fp rTests = do
    src <- readFile fp
    runSrc (Just fp) rTests src

runHandle :: FilePath -> Handle -> Bool -> IO ()
runHandle fp h rTests = do
    src <- hGetContents h
    runSrc (Just fp) rTests src

runSrc :: Maybe String -> Bool -> String -> IO ()
runSrc fnm rTests src = bracket B.newHandle B.closeHandle $ \h -> do
    addPackages h
    flip catch (handleEx h) $ do
        when rTests $ void $ B.runScript h Nothing [] "_system.modules._internals.set-auto-run-tests(true)"
        v <- B.runScript h fnm [] src
        pv <- B.valueToStringIO h v
        case pv of
            Nothing -> pure ()
            Just s -> putStrLn s
        when rTests $ do
            p <- B.allTestsPassed h
            when (not p) exitFailure
  where
    handleEx :: B.Handle -> B.InterpreterException -> IO ()
    handleEx h ex = do
        x <- B.formatException h True ex
        putStrLn $ "Error: " ++ x
        exitFailure

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
    B.addFFIPackage h "packages/sqlite" Sqlite.sqlitePackage
    B.addFFIPackage h "packages/python-ffi" PythonFFI.pythonFFIPackage

------------------------------------------------------------------------------

data MyOpts = MyOpts
  { file :: Maybe String
  , script :: Maybe String
  , runTests :: Bool
  , userArgs :: [String]
  }
  deriving Show

myOpts :: Parser MyOpts
myOpts = MyOpts
      <$> optional (argument str (metavar "FILE"))
      <*> optional (strOption
          (short 'c'
           <> metavar "SOURCE"
           <> help "code to run"))
      {-<*> option auto
          (long "test-level"
           <> value 1
           <> metavar "INT"
           <> help "test-level 0 = skip, 1= one line, 2 = show failures, 3 = show all")-}
      <*> switch (long "run-tests" <> help "Run tests")
      <*> many (argument str (metavar "ARGS..."))

myOptsPlus :: ParserInfo MyOpts
myOptsPlus = info (myOpts <**> helper)
      ( fullDesc
     <> progDesc "Burdock script runner and repl"
     <> header "Burdock" )

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
            MyOpts {file = Just f, runTests = rt} -> runFile f rt
            MyOpts {script = Just c, runTests = rt} -> runSrc Nothing rt c
            MyOpts {script = Nothing, file = Nothing, runTests = rt}
                | not isTTY -> runHandle "stdin" stdin rt
                | otherwise -> doRepl
