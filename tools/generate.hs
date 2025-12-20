{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad            (foldM, forM, forM_, replicateM, when)
import qualified Data.ByteString          as BS
import           Data.ByteString.Lazy     (toStrict)
import qualified Data.ByteString.Lazy     as LBS
import           Data.List                (intercalate, sortBy)
import qualified Data.Map                 as Map
import           Data.Ord                 (Down (..), comparing)
import           Data.Semigroup           ((<>))
import qualified Data.Set                 as Set
import           Data.Text                (Text, pack, unpack)
import qualified Data.Text.Encoding       as Text
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO             as Text
import           Language.Alex.Arbitrary  (genRegex, getFixedString)
import           Language.Alex.Ast        (AlexFile (..), Regex (..), Rule (..))
import           Language.Alex.Parser     (parseAlex)
import           Language.Happy.Arbitrary (History, debugGrammar)
import           Language.Happy.Generate  (Algorithm (..), generateCustom)
import           Language.Happy.Grammar   (Sym (..), fromAst)
import           Language.Happy.Lexer     (runAlex)
import           Language.Happy.Linker    (defaultLinkConfig, makeConfig)
import           Language.Happy.Parser    (parseGrammar)
import           Options.Applicative
import           System.Exit              (exitSuccess)
import           System.IO                (BufferMode (..), hSetBuffering,
                                           stdout)
import           System.Timeout           (timeout)
import           Test.QuickCheck          (generate, resize)
import           Text.Printf              (printf)

data Options = Options
    { alexFile   :: FilePath
    , happyFile  :: Maybe FilePath
    , startSym   :: Text
    , size       :: Int
    , iterations :: Int
    , boltzmannZ :: Double
    , algorithm  :: Algorithm
    }

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption
        ( long "alex"
       <> metavar "FILE"
       <> help "Path to Alex lexer file (.x)" )
    <*> optional (strOption
        ( long "happy"
       <> metavar "FILE"
       <> help "Path to Happy grammar file (.y)" ))
    <*> strOption
        ( long "start"
       <> metavar "SYMBOL"
       <> value "TranslationUnit"
       <> showDefault
       <> help "Start symbol for generation" )
    <*> option auto
        ( long "size"
       <> metavar "INT"
       <> value 100
       <> showDefault
       <> help "Size parameter for generation" )
    <*> option auto
        ( long "iterations"
       <> metavar "INT"
       <> value 1
       <> showDefault
       <> help "Number of test cases to generate" )
    <*> option auto
        ( long "z"
       <> metavar "DOUBLE"
       <> value 0.01
       <> showDefault
       <> help "Boltzmann z parameter (controls average size)" )
    <*> option auto
        ( long "alg"
       <> metavar "ALG"
       <> value AlgFair
       <> showDefault
       <> help "Algorithm: AlgDefault, AlgSwarm, AlgMutation, AlgScope, AlgFair" )

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    opts <- execParser optsInfo

    alexBytes <- LBS.readFile (alexFile opts)
    let alexSrc = Text.decodeUtf8With lenientDecode (LBS.toStrict alexBytes)
    let alex = parseAlex alexSrc

    case happyFile opts of
        Nothing    -> runLexerMode alex
        Just hFile -> runGrammarMode alex hFile opts

runLexerMode :: AlexFile -> IO ()
runLexerMode (AlexFile macrosList rulesList) = do
    let macroMap = Map.fromList macrosList
    -- Extract token name and group
    let tokenRules = [ (tokenName, ruleRegex r)
                     | r <- rulesList
                     , Just tokenName <- [ruleToken r]
                     ]
    let rulesByName = Map.fromListWith (++) [ (name, [regex]) | (name, regex) <- tokenRules ]

    results <- forM (Map.toList rulesByName) $ \(name, regexes) -> do
        let combinedRegex = RAlt regexes

        -- Check if it's a fixed string
        output <- case getFixedString macroMap combinedRegex of
            Just fixed -> return $ show fixed
            Nothing -> do
                -- Generate 5 examples
                samples <- generate $ replicateM 5 (genRegex macroMap combinedRegex)
                return $ show samples
        return (unpack name, output)

    let maxLen = if null results then 0 else maximum (map (length . fst) results)

    forM_ results $ \(name, output) -> do
        printf "%-*s %s\n" maxLen name output

runGrammarMode :: AlexFile -> FilePath -> Options -> IO ()
runGrammarMode alex hFile opts = do
    happyContent <- LBS.readFile hFile
    let happy = case runAlex happyContent parseGrammar of
            Left err -> error $ "Failed to parse Happy file: " ++ err
            Right h  -> h

    -- Debug Grammar Weights
    let cfg = Language.Happy.Linker.makeConfig defaultLinkConfig (boltzmannZ opts) alex
    putStrLn "--- Grammar Weight Analysis ---"
    debugGrammar cfg (startSym opts) happy
    putStrLn "-----------------------------"

    let (grammar, _) = fromAst (startSym opts) happy
    let totalRules = Map.size grammar

    let printCoverage covered = do
            let count = Set.size covered
            let pct = if totalRules > 0 then (fromIntegral count / fromIntegral totalRules) * 100.0 :: Double else 0
            printf "Coverage: %d/%d rules (%.2f%%)\n" count totalRules pct

            let allRules = Set.fromList [ t | Sym t _ <- Map.keys grammar ]
            let missing = Set.difference allRules covered
            when (not (Set.null missing)) $ do
                let missingList = map unpack $ Set.toList missing
                let (shown, remaining) = splitAt 6 missingList
                putStrLn $ "Missing Rules: " ++ intercalate ", " shown ++ if null remaining then "" else " ... and " ++ show (length remaining) ++ " more."

    let printStats counts = do
            let covered = Map.keysSet counts
            printCoverage covered

            let totalCount = sum (Map.elems counts)
            if totalCount > 0 then do
                putStrLn "--- Top Rules Used ---"
                let sortedRules = sortBy (comparing (Down . snd)) (Map.toList counts)
                let topRules = take 24 sortedRules

                let chunks _ [] = []
                    chunks n xs = take n xs : chunks n (drop n xs)

                let formatEntry (r, c) =
                        let pct = (fromIntegral c / fromIntegral totalCount * 100.0) :: Double
                        in printf "%-20.20s : %3d (%4.1f%%)" (unpack r) c pct :: String

                forM_ (chunks 3 topRules) $ \chunk -> do
                     putStrLn $ intercalate " | " (map formatEntry chunk)
            else
                putStrLn "No rules used."

    let loop (history, ruleCounts) _ = do
            let gen = resize (size opts) $ generateCustom (algorithm opts) defaultLinkConfig (boltzmannZ opts) alex (startSym opts) happy history

            res <- timeout 5000000 $ do
                (s, nextHistory, usedRules) <- generate gen
                let (out, rest) = splitAt 10000 s
                BS.putStr $ Text.encodeUtf8 $ pack (out ++ "\n")
                return (rest, nextHistory, usedRules)

            case res of
                Nothing -> do
                    putStrLn "Timeout!"
                    return (history, ruleCounts)
                Just (rest, nextHistory, usedRules) -> do
                    let newCounts = foldr (\r m -> Map.insertWith (+) r 1 m) ruleCounts usedRules
                    when (not (null rest)) $ do
                        printStats newCounts
                        exitSuccess
                    return (nextHistory, newCounts)

    (_, finalCounts) <- foldM loop (Map.empty, Map.empty :: Map.Map Text Int) [1 .. iterations opts]
    printStats finalCounts
    return ()

optsInfo :: ParserInfo Options
optsInfo = info (optionsParser <**> helper)
    ( fullDesc
    <> progDesc "Generate arbitrary source code from Happy/Alex specs"
    <> header "happy-arbitrary-generate - Grammar-based fuzzer" )
