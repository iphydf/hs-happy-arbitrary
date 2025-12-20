{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Language.Happy.Arbitrary
    ( Config (..)
    , defConfig
    , genTokens
    , genTree
    , genTreeFair
    , genTreeGas
    , expandFair -- Exported for re-use
    , RuleDef (..)
    , Sym (..)
    , solveGrammar
    , Weights
    , History -- Exported for re-use
    , expand -- Exported for re-use by other strategies
    , weightedSelect -- Exported for re-use
    , calcOptionWeight -- Exported for re-use
    , getSymbolWeight -- Exported for re-use
    , debugGrammar -- Exported for debugging
    ) where

import           Control.Monad             (replicateM)
import           Control.Monad.State       (StateT, get, lift, put, runStateT)
import           Data.List                 (minimumBy, sort)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Language.Happy.Ast        (Node, RuleDef (..), Sym (..))
import           Language.Happy.GenTree    (GenTree (..), flatten)
import           Language.Happy.Grammar    (Grammar, Production, extractTokens,
                                            fromAst)
import qualified Language.Happy.Grammar    as G
import           Language.Happy.Lexer      (Lexeme)
import           Test.QuickCheck.Arbitrary (arbitrary)
import           Test.QuickCheck.Gen       (Gen, choose)
import qualified Test.QuickCheck.Gen       as Gen

type Weights = Map Sym Double
type History = Map Text Int

solveGrammar :: Double -> Grammar -> Map Text token -> Sym -> Weights
solveGrammar _ _ _ _ = Map.empty

debugGrammar :: Config token -> Text -> Node (Lexeme Text) -> IO ()
debugGrammar cfg start g = do
    let tokens = extractTokens (parseToken cfg) g
    let (grammar, _) = fromAst start g
    let startSym = Sym start []
    let z = generationZ cfg
    let weights = solveGrammar z grammar tokens startSym
    let minCosts = calcMinCosts grammar tokens

    putStrLn $ "Total weights calculated: " ++ show (Map.size weights)
    putStrLn $ "Total min costs calculated: " ++ show (Map.size minCosts)

    let inspect sym = do
            putStrLn $ "\nAnalysis for " ++ show sym ++ ":"
            case Map.lookup sym grammar of
                Just prods -> do
                     mapM_ (\(i, opt) -> do
                        let (w, _) = calcOptionWeight z weights opt
                        let cost = 1 + sum (map (getMinCost minCosts) opt)
                        putStrLn $ "  Opt " ++ show i ++ " (w=" ++ show w ++ ", minCost=" ++ show cost ++ "): " ++ show opt
                      ) (zip [1::Int ..] prods)
                Nothing -> putStrLn "  (Symbol not found in grammar)"

    -- Inspect some common symbols if reachable
    inspect startSym

data Config token = Config
    { parseToken  :: Text -> token
    , generationZ :: Double
    }

defConfig :: (Text -> token) -> Config token
defConfig parseToken = Config{parseToken, generationZ = 0.01}

genTokens :: Config token -> Text -> Node (Lexeme Text) -> Gen [token]
genTokens cfg start g = flatten <$> genTree cfg start g

genTree :: Config token -> Text -> Node (Lexeme Text) -> Gen (GenTree token)
genTree cfg start g = fst <$> genTreeFair cfg Map.empty start g

genTreeFair :: Config token -> History -> Text -> Node (Lexeme Text) -> Gen (GenTree token, History)
genTreeFair cfg initHistory start g = do
    let tokens = extractTokens (parseToken cfg) g
    let (grammar, _) = fromAst start g
    let startSym = Sym start []

    -- Fair ignores Boltzmann weights (z parameter).
    Gen.sized $ \n -> runStateT (expandFair cfg tokens grammar startSym 0 n) initHistory

genTreeGas :: Config token -> Text -> Node (Lexeme Text) -> Gen (GenTree token)
genTreeGas cfg start g = do
    let tokens = extractTokens (parseToken cfg) g
    let (grammar, _) = fromAst start g
    let startSym = Sym start []
    let minCosts = calcMinCosts grammar tokens

    Gen.sized $ \gas -> expandGas cfg tokens grammar minCosts startSym gas

expandGas :: Config token -> Map Text token -> Grammar -> Map Sym Int -> Sym -> Int -> Gen (GenTree token)
expandGas cfg tokens grammar minCosts sym@(Sym name _) gas = do
    -- 1. Check if terminal (in tokens map)
    case Map.lookup name tokens of
        Just token -> return $ Leaf token
        Nothing -> case Map.lookup sym grammar of
            Nothing -> error $ "Unknown symbol: " ++ show sym
            Just prods -> do
                -- 2. Filter productions that fit in the gas budget
                -- Cost of a production = 1 (for this node) + sum(minCost(children))
                let prodCost prod = 1 + sum (map (getMinCost minCosts) prod)

                -- Candidates where minCost <= gas
                let candidates = [ (prod, cost) | prod <- prods, let cost = prodCost prod, cost <= gas ]

                -- If no candidates fit, we MUST pick the smallest one (Urgency!)
                -- Otherwise, we pick one of the candidates.
                chosenProd <- if null candidates
                    then return $ fst $ minimumBy (\(_, c1) (_, c2) -> compare c1 c2) [ (p, prodCost p) | p <- prods ]
                    else do
                         -- Pick a candidate weighted by its cost.
                         -- We want to favor expensive/recursive productions when we have gas.
                         -- Using cost^10 creates a strong bias towards complexity/spending gas.
                         let weightedCandidates = [ (fromIntegral c ^ (10 :: Int) :: Double, p) | (p, c) <- candidates ]
                         weightedSelect weightedCandidates

                let childrenSyms = chosenProd
                let k = length childrenSyms

                if k == 0
                    then return $ Node sym []
                    else do
                        -- 3. Distribute Gas
                        -- We have 'gas - 1' available to distribute.
                        -- Each child 'i' NEEDS 'minCosts[i]'.
                        -- Total required = baseCost - 1.
                        -- Surplus = (gas - 1) - (baseCost - 1) = gas - baseCost.
                        -- If Surplus < 0 (panic mode), we still give minimums but scaled down?
                        -- Actually, if we are in panic mode, we can't guarantee anything.
                        -- But let's try to give at least minCost to everyone if possible.

                        let requiredGas = map (getMinCost minCosts) childrenSyms
                        let totalRequired = sum requiredGas
                        let available = gas - 1

                        allocatedGas <- if available <= totalRequired
                            then
                                -- Not enough gas or just enough.
                                -- If we have deficit, some will fail to terminate, but we can't do better.
                                -- Distribute available proportionally to required?
                                -- Or just give required and let them handle the deficit recursively.
                                -- Giving required is better because the recursion handles the panic logic.
                                return requiredGas
                            else do
                                -- We have surplus. Distribute randomly.
                                let surplus = available - totalRequired
                                extras <- randomPartition k surplus
                                return $ zipWith (+) requiredGas extras

                        children <- mapM (\(s, g) -> expandGas cfg tokens grammar minCosts s g) (zip childrenSyms allocatedGas)
                        return $ Node sym children

getMinCost :: Map Sym Int -> Sym -> Int
getMinCost costs sym = Map.findWithDefault 1 sym costs

calcMinCosts :: Grammar -> Map Text token -> Map Sym Int
calcMinCosts grammar tokens = relax Map.empty (Map.keys grammar)
  where
    -- Initialize with infinite costs (or just rely on default lookup being large?)
    -- Better to start with empty and use a large default during calculation.
    -- Terminals cost 1.

    relax :: Map Sym Int -> [Sym] -> Map Sym Int
    relax currentCosts syms =
        let
            (newCosts, changed) = foldr (\sym (acc, chg) ->
                let
                    oldCost = Map.lookup sym acc
                    -- Calculate min cost from productions

                    getSymCost s =
                        case Map.lookup (symName s) tokens of
                            Just _ -> 1
                            Nothing -> Map.findWithDefault (100000::Int) s acc

                    calcProdCost prod = 1 + sum (map getSymCost prod)

                    newVal = case Map.lookup (symName sym) tokens of
                        Just _ -> 1
                        Nothing -> case Map.lookup sym grammar of
                            Nothing -> 1 -- Unknown/Terminal
                            Just prods ->
                                let costs = map calcProdCost prods
                                in if null costs then 1 else minimum costs

                    cappedVal = min newVal 100000 -- Cap to avoid overflow logic
                in
                    if Just cappedVal /= oldCost
                        then (Map.insert sym cappedVal acc, True)
                        else (acc, chg)
                ) (currentCosts, False) syms
        in
            if changed
                then relax newCosts syms
                else newCosts

symName :: Sym -> Text
symName (Sym n _) = n

randomPartition :: Int -> Int -> Gen [Int]
randomPartition k n
  | k <= 0 = return []
  | k == 1 = return [n]
  | otherwise = do
      splits <- sort <$> replicateM (k-1) (choose (0, n))
      let splits' = 0 : splits ++ [n]
      return $ zipWith (-) (tail splits') splits'

expandFair :: Config token -> Map Text token -> Grammar -> Sym -> Int -> Int -> StateT History Gen (GenTree token)
expandFair cfg tokens grammar sym@(Sym name _) depth maxDepth = do
    -- Update history (global count)
    history <- get
    let count = Map.findWithDefault 0 name history
    put (Map.insert name (count + 1) history)

    case Map.lookup name tokens of
        Just token -> return $ Leaf token
        Nothing -> case Map.lookup sym grammar of
            Nothing -> error $ "Unknown symbol: " ++ show sym
            Just prods -> do

                -- Calculate Fair weights
                -- Weight = 1000.0 / ((1 + usage_of_children) * (1 + depth_penalty))^2
                let calcWeight prod =
                        let
                            -- Look at current history to see how saturated the children are
                            currentHistory = Map.insert name (count + 1) history
                            childCounts = sum [ Map.findWithDefault 0 n currentHistory | s@(Sym n _) <- prod, Map.member s grammar ]
                            usagePenalty = fromIntegral (childCounts + 1) :: Double

                            -- Urgency / Depth Penalty
                            -- As we get deeper, heavily penalize productions that introduce more non-terminals.
                            numNonTerminals = length [ s | s <- prod, Map.member s grammar ]
                            urgency = fromIntegral depth / max 1.0 (fromIntegral maxDepth)
                            -- Penalty factor increases with depth and complexity.
                            depthPenalty = 1.0 + (fromIntegral numNonTerminals * urgency * 20.0)
                        in 1000.0 / (usagePenalty * usagePenalty * depthPenalty * depthPenalty)

                let weightedOptions = [ (calcWeight prod, prod) | prod <- prods ]

                let countNonTerminals prod = length [ s | s <- prod, Map.member s grammar ]

                -- If we are too deep, prefer simpler productions to terminate
                prod <- if depth < maxDepth
                    then lift $ weightedSelect weightedOptions
                    else return $ snd $ minimumBy (\(_, opt1) (_, opt2) -> compare (countNonTerminals opt1) (countNonTerminals opt2)) weightedOptions

                children <- mapM (\s -> expandFair cfg tokens grammar s (depth + 1) maxDepth) prod
                return $ Node sym children

expand :: Double -> Grammar -> Map Text token -> Weights -> Sym -> Int -> Int -> Maybe Text -> Int -> Gen (GenTree token)
expand z grammar tokens weights sym@(Sym name _) depth maxDepth lastSym consecutive = do
    -- 1. Check if terminal (in tokens map)
    case Map.lookup name tokens of
        Just token -> return $ Leaf token
        Nothing -> do
            -- 2. Check if non-terminal (in grammar)
            case Map.lookup sym grammar of
                Just prods -> do
                    if null prods
                        then error $ "No productions for rule: " ++ show sym
                        else do
                            -- Calculate weights for each option
                            let weightedOptions = map (calcOptionWeight z weights) prods

                            let consecutive' = if Just name == lastSym then consecutive + 1 else 0
                            let limit = 2

                            -- Recursive check: check if sym appears in production
                            -- For expanded grammar, we check if exact sym is in production
                            let isRecursive (_, prod) = sym `elem` prod

                            let optionsToUse = if consecutive' >= limit
                                    then let filtered = filter (not . isRecursive) weightedOptions
                                         in if null filtered then weightedOptions else filtered
                                    else weightedOptions

                            -- Heuristic for simplest: fewer symbols?
                            let countNonTerminals prod = length [ s | s@(Sym n _) <- prod, Map.notMember n tokens ]

                            prod <- if depth < maxDepth
                                then weightedSelect optionsToUse
                                else return $ snd $ minimumBy (\(_, opt1) (_, opt2) -> compare (countNonTerminals opt1) (countNonTerminals opt2)) optionsToUse

                            children <- mapM (\s -> expand z grammar tokens weights s (depth + 1) maxDepth (Just name) consecutive') prod
                            return $ Node sym children

                Nothing -> error $ "Unknown symbol: " ++ show sym

calcOptionWeight :: Double -> Weights -> Production -> (Double, Production)
calcOptionWeight z weights prod =
    let
         -- Product of weights of symbols in production
         w = product (map (getSymbolWeight z weights) prod)
    in (w, prod)

getSymbolWeight :: Double -> Weights -> Sym -> Double
getSymbolWeight z weights s =
    case Map.lookup s weights of
        Just w  -> w
        Nothing -> z -- Assume terminal

weightedSelect :: [(Double, a)] -> Gen a
weightedSelect [] = error "weightedSelect: empty list"
weightedSelect options = do
    let total = sum (map fst options)
    if total <= 0
        then error $ "weightedSelect: total weight <= 0: " ++ show (map fst options)
        else do
            r <- Gen.choose (0.0, total)
            return $ pick r options
  where
    pick _ [] = snd (last options) -- Should not happen
    pick x ((w, v):rest)
        | x <= w = v
        | otherwise = pick (x - w) rest
