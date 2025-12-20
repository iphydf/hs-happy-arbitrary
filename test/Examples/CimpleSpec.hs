{-# LANGUAGE OverloadedStrings #-}
module Examples.CimpleSpec where

import qualified Data.ByteString.Lazy  as LBS
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import qualified Data.Text.IO          as Text
import           Language.Alex.Ast     (AlexFile (..))
import           Language.Alex.Parser  (parseAlex)
import           Language.Happy.Lexer  (runAlex)
import           Language.Happy.Linker (defaultLinkConfig,
                                        generateSourceWithStart)
import           Language.Happy.Parser (parseGrammar)
import           System.Directory      (doesFileExist)
import           Test.Hspec            (Spec, describe, it, pendingWith, runIO,
                                        shouldSatisfy)
import           Test.QuickCheck       (generate, resize)

spec :: Spec
spec = do
    let alexPaths = [ "hs-happy-arbitrary/test/examples/Cimple/Lexer.x"
                    , "test/examples/Cimple/Lexer.x"
                    ]
    let happyPaths = [ "hs-happy-arbitrary/test/examples/Cimple/Parser.y"
                     , "test/examples/Cimple/Parser.y"
                     ]

    mAlexSrc <- runIO $ findFirst alexPaths
    mHappySrc <- runIO $ findFirst happyPaths

    describe "Cimple Generator" $ do
        case (mAlexSrc, mHappySrc) of
            (Just alexSrc, Just happySrc) -> do
                let _alex = parseAlex alexSrc
                let happy = case runAlex (LBS.fromStrict $ Text.encodeUtf8 happySrc) parseGrammar of
                        Left err -> error err
                        Right h  -> h

                it "parses the grammar successfully" $ do
                    length (show happy) `shouldSatisfy` (> 0)

                it "generates valid translation units" $ do
                    -- pendingWith "Generator timeouts on full Cimple grammar due to complexity/performance of ReadP Alex parser."
                    length (rules _alex) `shouldSatisfy` (> 0)
                    code <- generate $ resize 10 $ generateSourceWithStart defaultLinkConfig 0.01 _alex "TranslationUnit" happy
                    length code `shouldSatisfy` (> 0)
            _ -> it "skips tests when example files are missing" $ do
                pendingWith "Cimple example files not found"

findFirst :: [FilePath] -> IO (Maybe Text)
findFirst [] = return Nothing
findFirst (p:ps) = do
    exists <- doesFileExist p
    if exists
        then Just <$> (Text.decodeUtf8 . LBS.toStrict <$> LBS.readFile p)
        else findFirst ps

