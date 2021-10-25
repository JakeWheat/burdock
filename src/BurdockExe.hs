
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

TODO:
set test level, works with all the above:
--test-level LEV
LEV:
0 = skip
1 = one line
2 = show failures
3 = show all (default)
--run-tests LEV
LEV:
0 = none
1 = direct user code only
2 = all code not in another package or built in (default)
3 = all user code including other packages
4 = include built ins and global/internals (for implementation
    debugging and troubleshooting)
TODO: filters on which tests to run

-}


{-# LANGUAGE ScopedTypeVariables #-}
import qualified  Burdock.Interpreter as B

import Control.Monad.Trans
import System.Console.Haskeline (InputT
                                ,Interrupt
                                ,historyFile
                                ,withInterrupt
                                ,getInputLine
                                ,runInputT
                                ,defaultSettings
                                --,catch
                                )
import Control.Exception.Safe (catch, SomeException, displayException)


import Options.Applicative (Parser
                           ,strOption
                           ,long
                           ,metavar
                           ,help
                           ,short
                           ,option
                           --,showDefault
                           --,switch
                           ,auto
                           ,value
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
                           )
import System.IO (Handle
                 ,hIsTerminalDevice
                 ,stdin
                 ,hGetContents)

------------------------------------------------------------------------------


runFile :: FilePath -> IO ()
runFile fp = do
    src <- readFile fp
    runSrc (Just fp) src

runHandle :: FilePath -> Handle -> IO ()
runHandle fp h = do
    src <- hGetContents h
    runSrc (Just fp) src


runSrc :: Maybe String -> String -> IO ()
runSrc fnm src = do
    h <- B.newHandle
    
    v <- B.runScript h fnm [] src
    case B.valueToString v of
        Nothing -> pure ()
        Just s -> putStrLn s

-- repl

process :: B.Handle -> String -> IO ()
process h src = (do
    v <- B.runScript h Nothing [] src
    case B.valueToString v of
            Nothing -> pure ()
            Just s -> putStrLn s)
    `catch` (\(e::SomeException) -> putStrLn $ "Error: " ++ displayException e)

repl :: B.Handle -> InputT IO ()
repl h = go
  where
    go = withInterrupt (do
        minput <- getInputLine "t > "
        case minput of
            Nothing -> pure ()
            Just input -> do
                liftIO $ process h input
                go)
        -- ctrl-c resets to the prompt, doesn't exit the repl
        `catch` (\(_::Interrupt) -> liftIO (putStr "^C") >> go)


doRepl :: IO ()
doRepl = do
    h <- B.newHandle
    runInputT st (repl h)
  where
    st = defaultSettings {historyFile = Just ".burdockreplhistory"}

------------------------------------------------------------------------------

data MyOpts = MyOpts
  { file :: Maybe String
  , script :: Maybe String
  , testLevel :: Int}
  deriving Show

myOpts :: Parser MyOpts
myOpts = MyOpts
      <$> optional (argument str (metavar "FILE"))
      <*> optional (strOption
          (short 'c'
           <> metavar "SOURCE"
           <> help "code to run"))
      <*> option auto
          (long "test-level"
           <> value 1
           <> metavar "INT"
           <> help "test-level 0 = skip, 1= one line, 2 = show failures, 3 = show all")


myOptsPlus :: ParserInfo MyOpts
myOptsPlus = info (myOpts <**> helper)
      ( fullDesc
     <> progDesc "Burdock script runner and repl"
     <> header "Burdock" )

------------------------------------------------------------------------------

main :: IO ()
main = do
    os <- execParser myOptsPlus
    isTTY <- hIsTerminalDevice stdin
    case os of
        MyOpts {file = Just {}, script = Just {}} -> error "please pass either a file or code to run, not both"
        MyOpts {file = Just f} -> runFile f
        MyOpts {script = Just c} -> runSrc Nothing c
        _ | not isTTY -> runHandle "stdin" stdin
        _ -> doRepl
