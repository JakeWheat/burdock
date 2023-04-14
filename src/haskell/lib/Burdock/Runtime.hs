
{-

The runtime is the code that allows running in a burdock context. It's
kind of like an ffi interface to haskell, it doesn't know anything
about syntax.

The interpreter takes syntax and calls functions in the runtime.
Haskell ffi code uses function in the runtime too.

-}

module Burdock.Runtime
    (Value(..)

    ,runBurdock
    ,Runtime
    ,liftIO
    ) where

import Control.Monad.Reader (ReaderT
                            ,runReaderT
                            --,ask
                            --,local
                            ,liftIO
                            --,MonadIO
                            )

data RuntimeState = RuntimeState

type Runtime = ReaderT RuntimeState IO

data Value = Value
    deriving (Eq, Show)

runBurdock :: Runtime a -> IO a
runBurdock f = runReaderT f RuntimeState
