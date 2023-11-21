{-

put this in a separate module because ghc stops being able to allow
out of order definitions at the top level properly when makeLenses is
used, so isolate it here

no idea why it's doing this or if there's a better workaround

-}
{-# LANGUAGE TemplateHaskell #-}
module Burdock.RenameTypes 
    (RenamerEnv(..)
    ,reImportSources
    ,reModuleMetadatas
    ,reProvides
    ,reProvideFroms
    ,reImports
    ,reIncludes
    ,reIncludeFroms
    ,reImportFroms
    ,reBindings
    ,reCurrentOriginIDName
    
    ,reProvideItems
    ,reLoadModules

    ,RenamerBindingEntry(..)
    ,beRenamedName
    ,beSourcePos
    ,beBm
    ,beOrigin
    ,beIsLocal
    ) where

import Burdock.Syntax
    (ProvideItem(..)
    ,ImportSource(..)
    ,SourcePosition
    )

import Data.Text (Text)

import Lens.Micro.TH
    (makeLenses
    )
import Burdock.ModuleMetadata
    (ModuleMetadata
    ,BindingMeta
    ,OriginID
    ,ModuleID
    )

-- tracks the renamer state during renaming
data RenamerEnv
    = RenamerEnv
    {
     -- available modules to import
     _reImportSources :: [(ImportSource,ModuleID)]
      --  the key is the identifier of the module
      -- this is the name and args of the import source
    ,_reModuleMetadatas :: [(ModuleID, ModuleMetadata)]
    -- tracking the prelude statements
    ,_reProvides :: [(SourcePosition, [ProvideItem])]
    ,_reProvideFroms :: [(SourcePosition, [Text], [ProvideItem])]
    ,_reImports :: [(SourcePosition, ImportSource, Text)]
    ,_reIncludes :: [(SourcePosition, ImportSource)]
    ,_reIncludeFroms :: [(SourcePosition, Text, [ProvideItem])]
    ,_reImportFroms :: [(SourcePosition, ImportSource, [ProvideItem])]
    -- temp
    ,_reLoadModules :: [(ModuleID,Text)]

    -- all the bindings used to rename the body, track additional
    -- declarations in the body, and to process applyprovides
    -- the rule is, the apply provides should only look at these bindings,
    -- and the reprovides, reprovidefroms, and not any of the other
    -- prelude statements, all the info needed from these is put into rebindings
    --,_reBindings :: [([Text], RenamerBindingEntry)]
    ,_reBindings :: [Text]
    -- this is used as a origin id
    ,_reCurrentOriginIDName :: Text
    -- all the provide items gathered after prelude processing
    ,_reProvideItems :: Bool
    }

-- represents a single entry used during body renaming
-- and apply provides
data RenamerBindingEntry
    = RenamerBindingEntry
    {_beRenamedName :: [Text]
    ,_beSourcePos :: SourcePosition
    ,_beBm :: BindingMeta
    ,_beOrigin :: OriginID
    -- true for locally introduced idens, used to expand * in
    -- apply provides
    ,_beIsLocal :: Bool
    }
    -- this is when you have an identifier that's bound by a maximum
    -- of one direct name, and then introduced by multiple inconsitent *
    -- expansions. For user convenience, if these are never used, it's
    -- not an error, but if they are used, the error is produced at that
    -- point
    | RenamerBindingAmbiguous [SourcePosition]

makeLenses ''RenamerEnv
makeLenses ''RenamerBindingEntry
