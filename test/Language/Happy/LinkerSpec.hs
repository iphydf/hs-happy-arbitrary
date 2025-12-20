{-# LANGUAGE OverloadedStrings #-}
module Language.Happy.LinkerSpec (spec) where

import           Data.Fix              (Fix (..))
import qualified Data.Set
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Language.Alex.Ast     (AlexFile (..), Regex (..))
import qualified Language.Alex.Ast     as Alex
import           Language.Happy.Ast    (NodeF (..))
import           Language.Happy.Lexer  (AlexPosn (..), Lexeme (..))
import           Language.Happy.Linker (LinkConfig (..), defaultLinkConfig,
                                        generateSourceWithStart)
import           Language.Happy.Tokens (LexemeClass (..))
import           Test.Hspec
import           Test.QuickCheck       (generate)

-- Helper to create dummy Lexemes
l :: Text -> Lexeme Text
l x = L (AlexPn 0 0 0) IdName x

spec :: Spec
spec = describe "Linker" $ do
    it "generates code excluding keywords" $ do
        -- Mock Grammar: S -> id
        -- %token id { L _ TokenId _ }
        let happy = Fix $ Grammar
                [] -- pragmas
                [ Fix $ PragmaToken [ Fix $ Token (l "id") (l "{ L _ TokenId _ }") ] ]
                [ Fix $ RuleDefn (l "S") []
                    [ Fix $ RuleLine [ Fix $ Symbol (l "id") [] ] (l "{ $1 }") ]
                ]
                []

        -- Mock Alex:
        -- tokens :-
        --   [a-z]+ { mkL TokenId }
        --   "if"   { mkL TokenIf }
        let alex = AlexFile []
                [ Alex.Rule [] (RPlus (RSet (foldr (calcSet False) mempty ['a'..'z']) False)) " { mkL TokenId }" (Just "TokenId")
                , Alex.Rule [] (RString "if") " { mkL TokenIf }" (Just "TokenIf")
                ]

        -- We expect "if" to NEVER be generated for "id", even though [a-z]+ matches it.
        -- We'll generate a bunch and check.

        let config = defaultLinkConfig

        results <- generate $ sequence [ generateSourceWithStart config 0.01 alex "S" happy | _ <- [1..100::Int] ]

        mapM_ (`shouldNotBe` "if") results

        -- It should generate other strings
        length (filter (/= "") results) `shouldBe` 100

-- Helper for RSet construction (simplified)
calcSet :: Bool -> Char -> Data.Set.Set Char -> Data.Set.Set Char
calcSet _ c s = Data.Set.insert c s
