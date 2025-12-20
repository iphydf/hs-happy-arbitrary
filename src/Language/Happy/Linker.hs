{-# LANGUAGE OverloadedStrings #-}
module Language.Happy.Linker
    ( LinkConfig (..)
    , defaultLinkConfig
    , generateSource
    , generateSourceWithStart
    , makeConfig -- Exported
    ) where

import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe               (fromMaybe, listToMaybe, mapMaybe)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Language.Alex.Arbitrary  (genRegex)
import           Language.Alex.Ast        (AlexFile (..), Regex (..), Rule (..))
import qualified Language.Alex.Ast        as Alex
import           Language.Happy.Arbitrary (Config (..), genTokens)
import           Language.Happy.Ast       (Node)
import           Language.Happy.Lexer     (Lexeme)
import           Test.QuickCheck          (Gen, elements, oneof, suchThat)

data LinkConfig = LinkConfig
    { happyTokenPattern :: Text -> Maybe Text -- ^ Extract key from Happy code
    , alexTokenPattern  :: Text -> Maybe Text -- ^ Extract key from Alex code
    }

defaultLinkConfig :: LinkConfig
defaultLinkConfig = LinkConfig
    { happyTokenPattern = extractHappy
    , alexTokenPattern  = extractAfter "mkL "
    }

extractHappy :: Text -> Maybe Text
extractHappy text =
    case Text.breakOn "L _ " text of
        (_, match) | Text.null match -> Nothing
        (_, match) ->
            let rest = Text.drop 4 match -- Drop "L _ "
                -- Take until '}'
                content = Text.takeWhile (/= '}') rest
                -- Strip whitespace
                trimmed = Text.strip content
                -- Remove trailing '_' and whitespace
                final = if Text.isSuffixOf "_" trimmed
                        then Text.strip (Text.dropEnd 1 trimmed)
                        else trimmed
            in Just final

extractAfter :: Text -> Text -> Maybe Text
extractAfter start text =
    case Text.breakOn start text of
        (_, match) | Text.null match -> Nothing
        (_, match) ->
            let rest = Text.drop (Text.length start) match
            in Just $ Text.strip $ Text.takeWhile (\c -> c /= ' ' && c /= '}' && c /= '\n' && c /= '`') rest

-- | Generates source code by linking Happy grammar and Alex lexer.
generateSource :: LinkConfig -> Double -> AlexFile -> Node (Lexeme Text) -> Gen String
generateSource cfg z alex happy = generateSourceWithStart cfg z alex "TranslationUnit" happy

-- | Generates source code with a specific start symbol.
generateSourceWithStart :: LinkConfig -> Double -> AlexFile -> Text -> Node (Lexeme Text) -> Gen String
generateSourceWithStart cfg z alex startSymbol happy = do
    let config = makeConfig cfg z alex
    tokenGens <- genTokens config startSymbol happy
    unwords <$> runStateChain tokenGens "0"

runStateChain :: [Text -> Gen (String, Text)] -> Text -> Gen [String]
runStateChain [] _ = return []
runStateChain (f:fs) s = do
    (str, s') <- f s
    strs <- runStateChain fs s'
    return (str:strs)

type LexerState = Text

makeConfig :: LinkConfig -> Double -> AlexFile -> Config (LexerState -> Gen (String, LexerState))
makeConfig cfg z alex =
    let
        macroMap = Map.fromList (macros alex)

        -- Parse rules into (State, TokenClass) -> [(Regex, NextState)]
        -- and Silent Transitions: State -> [(Regex, NextState)]

        -- Helper to normalize state list (empty -> ["0"])
        normStates [] = ["0"]
        normStates s  = s

        processRule :: Rule -> (Map (LexerState, Text) [(Regex, Maybe LexerState)], Map LexerState [(Regex, LexerState)]) -> (Map (LexerState, Text) [(Regex, Maybe LexerState)], Map LexerState [(Regex, LexerState)])
        processRule r (tokMap, sMap) =
            let states = normStates (ruleState r)
                trans = extractTransition (ruleCode r)

                -- Check if it produces a token
                mbKey = alexTokenPattern cfg (ruleCode r)
            in case mbKey of
                Just key ->
                    -- It's a token rule
                    let newEntries = [ ((s, key), [(ruleRegex r, trans)]) | s <- states ]
                    in (Map.unionWith (++) (Map.fromList newEntries) tokMap, sMap)
                Nothing ->
                    -- It might be a silent transition (if it has a transition)
                    case trans of
                        Just nextState ->
                            let newEntries = [ (s, [(ruleRegex r, nextState)]) | s <- states ]
                            in (tokMap, Map.unionWith (++) (Map.fromList newEntries) sMap)
                        Nothing -> (tokMap, sMap) -- Ignore rules that do nothing (skip)

        (tokenMap, silentMap) = foldr processRule (Map.empty, Map.empty) (rules alex)

        -- Identify keywords (literal -> token key)
        keywords :: Map String Text
        keywords = Map.fromList $ mapMaybe getLiteral (rules alex)

        getLiteral r = do
            lit <- case ruleRegex r of
                RString t -> Just (Text.unpack t)
                _         -> Nothing
            key <- alexTokenPattern cfg (ruleCode r)
            return (lit, key)

        -- BFS to find all paths from startState to a state that accepts 'key'
        -- Returns: List of (List of silent rules, Final token rule)
        findAllPaths :: LexerState -> Text -> [([(Regex, LexerState)], (Regex, Maybe LexerState))]
        findAllPaths startState key = bfs Set.empty [(startState, [])]
          where
            bfs :: Set LexerState -> [(LexerState, [(Regex, LexerState)])] -> [([(Regex, LexerState)], (Regex, Maybe LexerState))]
            bfs _ [] = []
            bfs visited ((curr, path):queue)
                | Set.member curr visited = bfs visited queue
                | otherwise =
                    let
                        -- 1. Direct matches at current state
                        matches = fromMaybe [] (Map.lookup (curr, key) tokenMap)
                        currentResults = [ (reverse path, m) | m <- matches ]

                        -- 2. Expand silent transitions
                        newVisited = Set.insert curr visited
                        transitions = fromMaybe [] (Map.lookup curr silentMap)
                        newNodes = [ (next, (reg, next) : path) | (reg, next) <- transitions ]
                    in
                        currentResults ++ bfs newVisited (queue ++ newNodes)

        -- Generator factory
        getTokenGen :: Text -> LexerState -> Gen (String, LexerState)
        getTokenGen happyCode startState =
            case happyTokenPattern cfg happyCode of
                Nothing -> return ("", startState)
                Just key ->
                    case findAllPaths startState key of
                        [] -> return ("/* No path for " ++ Text.unpack key ++ " in " ++ Text.unpack startState ++ " */", startState)
                        paths -> do
                            (silentPath, (finalReg, finalNextOpt)) <- elements paths

                            -- Generate text for silent path
                            silentStrs <- mapM (\(r, _) -> genRegex macroMap r) silentPath

                            -- Generate text for final token
                            finalStr <- genRegex macroMap finalReg `suchThat` (\s ->
                                    case Map.lookup s keywords of
                                        Nothing -> True
                                        Just k  -> k == key
                                )

                            let fullStr = unwords (silentStrs ++ [finalStr])
                            -- Determine final state
                            let finalState = fromMaybe (if null silentPath then startState else snd (last silentPath)) finalNextOpt

                            return (fullStr, finalState)

    in Config { parseToken = getTokenGen, generationZ = z }

extractTransition :: Text -> Maybe Text
extractTransition code =
    let
        -- patterns: `andBegin` STATE, begin STATE
        -- simple search
        findIn s =
            let (_, aft) = Text.breakOn s code
            in if Text.null aft
               then Nothing
               else
                   -- drop s, take next word
                   let rest = Text.strip (Text.drop (Text.length s) aft)
                       word = Text.takeWhile (\c -> c /= ' ' && c /= '}') rest
                   in Just word
    in case findIn "`andBegin`" of
        Just s  -> Just s
        Nothing -> findIn "begin"
