{-# LANGUAGE TemplateHaskell #-}
module Data.Config where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH   (Options (fieldLabelModifier, rejectUnknownFields),
                                  defaultOptions, deriveFromJSON)
import           Data.Text       (Text)
import           Data.Time.Clock (NominalDiffTime)
import           Discord.Types   (GuildId)

data Config
  = Config
    { _log               :: !String          -- "corgi-the-bot.log"
    , _auth              :: !Text            -- "Bot ?????????????????"
    , _testGuilds        :: ![GuildId]       -- ["847304434445713439"]
    , _test              :: !Bool            -- true
    , _pruneAfterSeconds :: !NominalDiffTime -- 600
    , _maxBlocksPerMsg   :: !Int             -- 10
    , _maxCharsPerMsg    :: !Int             -- 2000
    , _reactWait         :: !Text            -- "\u231B"
    , _reactCheck        :: !Text            -- "\u2705"
    , _reactCancel       :: !Text            -- "\u274C"
    , _maxOutput         :: !Int             -- 1048576
    , _sandboxCmd        :: !String          -- "cat"
    , _sandboxConf       :: !String          -- "--"
    }
deriveFromJSON defaultOptions { fieldLabelModifier = drop 1, rejectUnknownFields = True } ''Config
makeLenses ''Config
