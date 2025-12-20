{-# LANGUAGE OverloadedStrings #-}
module Language.Happy.MutationSpec where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Language.Happy.Arbitrary (defConfig)
import           Language.Happy.Ast       (Node)
import           Language.Happy.GenTree   (flatten)
import           Language.Happy.Lexer     (Lexeme, runAlex)
import           Language.Happy.Mutation  (genTreeMutation)
import           Language.Happy.Parser    (parseGrammar)
import qualified Language.Happy.Parser    as Parser
import           Language.Happy.Tokens    (LexemeClass (..))
import           Test.Hspec               (Spec, describe, it, shouldSatisfy)
import           Test.QuickCheck          (generate, resize)

parseToken :: Text -> LexemeClass
parseToken = read . Text.unpack . (!! 2) . concatMap (filter (not . Text.null) . Text.splitOn "\t") . Text.splitOn " "

tryParseGrammar :: Monad m => (Node (Lexeme Text) -> m ()) -> m ()
tryParseGrammar _ | BS.null Parser.source = return ()
tryParseGrammar f =
    case runAlex (LBS.fromStrict Parser.source) parseGrammar of
        Left err -> error err
        Right ok -> f ok

spec :: Spec
spec = tryParseGrammar $ \g -> do
    describe "genTreeMutation" $ do
        it "generates a tree successfully" $ do
            tree <- generate $ resize 10 $ genTreeMutation (defConfig parseToken) "Grammar" g
            let tokens = flatten tree
            length tokens `shouldSatisfy` (> 0)
