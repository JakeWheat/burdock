
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Burdock.StaticError
    (StaticError(..)
    ,prettyStaticError
    ,prettyStaticErrors
    ) where

import Prelude hiding (error, show, putStrLn)
import Burdock.Utils (show)

import Data.Text (Text)
import qualified Data.Text as T

import Data.Data (Data)
import Burdock.Syntax (SourcePosition)

data StaticError
    = UnrecognisedIdentifier SourcePosition [Text]
    -- current error def, original def
    | IdentifierRedefined SourcePosition SourcePosition Text
    -- current error pos, target def pos
    | AssignToNonVar SourcePosition SourcePosition [Text]
    deriving (Eq,Show, Data)

prettyStaticError :: StaticError -> Text
prettyStaticError = \case
    UnrecognisedIdentifier _ [] -> "internal error with unrecognised identifier"
    UnrecognisedIdentifier pos is -> doMsg pos $ icd is <> " not found (static)"
    IdentifierRedefined csp osp i ->
        T.unlines
        [doMsg csp $ "identifier redefined: " <> i
        ,doMsg osp "previous definition here"]
    AssignToNonVar csp osp is ->
        T.unlines
        [doMsg csp $ "assign to non var: " <> icd is
        ,doMsg osp "target defined here"]
  where
    doMsg pos msg = case pos of
        Nothing -> "<unknown>:" <> msg
        Just (fn,l,c) -> fn <> ":" <> show l <> ":" <> show c <> ": " <> msg
    icd = T.intercalate "."

-- todo: sort by their primary source position
-- if there's no source position, should they come last?
prettyStaticErrors :: [StaticError] -> Text
prettyStaticErrors = T.unlines . map prettyStaticError
