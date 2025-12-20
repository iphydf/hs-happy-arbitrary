{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData        #-}
module Language.Alex.Ast where

import           Data.Set     (Set)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data Regex
    = RChar Char
    | RString Text
    | RSet (Set Char) Bool -- Bool is True if inverted
    | RStar Regex
    | RPlus Regex
    | ROpt Regex
    | RSeq [Regex]
    | RAlt [Regex]
    | RMacro Text -- Reference to a macro defined in the file
    deriving (Show, Eq, Ord, Generic)

data Rule = Rule
    { ruleState :: [Text] -- List of start codes, empty means all/default
    , ruleRegex :: Regex
    , ruleCode  :: Text
    , ruleToken :: Maybe Text -- ^ The token name if this rule produces one (e.g. via mkL)
    }
    deriving (Show, Eq, Ord, Generic)

data AlexFile = AlexFile
    { macros :: [(Text, Regex)]
    , rules  :: [Rule]
    }
    deriving (Show, Eq, Ord, Generic)
