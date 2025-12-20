{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module Language.Happy.Grammar
    ( Grammar
    , Production
    , Sym (..)
    , fromAst
    , extractTokens
    ) where

import           Control.Monad.State  (State, execState, gets, modify)
import           Data.Fix             (Fix (..))
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import           Language.Happy.Ast   (Node, NodeF (..), RuleDef (..), Sym (..))
import           Language.Happy.Lexer (Lexeme, lexemeText)

-- | A grammar is a map from a non-terminal symbol to a list of productions.
-- Each production is a list of symbols.
type Production = [Sym]
type Grammar = Map Sym [Production]

-- | Converts the AST into an expanded Grammar and a set of token names.
fromAst :: Text -> Node (Lexeme Text) -> (Grammar, Set Text)
fromAst start node =
    let rules = extractRules node
        tokens = extractTokenNames node
        startSym = Sym start []
        grammar = expandGrammar rules tokens startSym
    in (grammar, tokens)

-- Extraction Logic

extractRules :: Node (Lexeme Text) -> Map Text [RuleDef]
extractRules (Fix (Grammar _ _ rules _)) = Map.unionsWith (++) (map extractRule rules)
extractRules _ = mempty

extractRule :: Node (Lexeme Text) -> Map Text [RuleDef]
extractRule (Fix (RuleDefn name params ruleLines)) =
    let paramNames = map lexemeText params
        prods = extractRuleLines ruleLines
    in Map.singleton (lexemeText name) [RuleDef paramNames prods]
extractRule (Fix (RuleType _ _)) = mempty
extractRule node = error $ "Unexpected node in Rules list: " ++ show node

extractRuleLines :: [Node (Lexeme Text)] -> [[Sym]]
extractRuleLines = map extractRuleLine

extractRuleLine :: Node (Lexeme Text) -> [Sym]
extractRuleLine (Fix (RuleLine syms _)) = map extractSymbol syms
extractRuleLine _                       = []

extractSymbol :: Node (Lexeme Text) -> Sym
extractSymbol (Fix (Symbol name args)) = Sym (lexemeText name) (map extractSymbol args)
extractSymbol _ = error "Invalid AST: Expected Symbol"

extractTokenNames :: Node (Lexeme Text) -> Set Text
extractTokenNames (Fix (Grammar _ pragmas _ _)) =
    Set.unions (map extractPragmaTokenNames pragmas)
extractTokenNames _ = mempty

extractPragmaTokenNames :: Node (Lexeme Text) -> Set Text
extractPragmaTokenNames (Fix (PragmaToken tokens)) =
    Set.fromList [ lexemeText name | Fix (Token name _) <- tokens ]
extractPragmaTokenNames _ = mempty

-- | Extracts the actual token values (generic)
extractTokens :: (Text -> token) -> Node (Lexeme Text) -> Map Text token
extractTokens parseFunc (Fix (Grammar _ pragmas _ _)) =
    Map.unions (map (extractPragmaToken parseFunc) pragmas)
extractTokens _ _ = mempty

extractPragmaToken :: (Text -> token) -> Node (Lexeme Text) -> Map Text token
extractPragmaToken parseFunc (Fix (PragmaToken tokens)) =
    Map.fromList [ (lexemeText name, parseFunc (lexemeText code))
                 | Fix (Token name code) <- tokens ]
extractPragmaToken _ _ = mempty

-- Expansion Logic

expandGrammar :: Map Text [RuleDef] -> Set Text -> Sym -> Grammar
expandGrammar rules tokens start =
    execState (process [start]) Map.empty
  where
    process :: [Sym] -> State Grammar ()
    process [] = return ()
    process (sym@(Sym name args) : rest) = do
        visited <- gets (Map.member sym)
        if visited
            then process rest
            else do
                -- Check if it's a token
                if Set.member name tokens
                    then process rest
                    else case Map.lookup name rules of
                        Nothing ->
                             -- It might be a terminal (e.g. literal string)
                             process rest

                        Just defs -> do
                            -- Mark as visited (will be overwritten, but prevents recursion loop during processing if needed,
                            -- though we write result at end of block. The important thing is we check 'visited' at start)
                            -- Actually, we must insert something to prevent infinite loop if A -> A.
                            -- But we compute result first.
                            -- If A -> A, 'sym' is same.
                            -- 'visited' check handles it.
                            -- We insert placeholder to handle recursion?
                            -- No, 'visited' is checked before processing.
                            -- We need to mark it as visited BEFORE processing children?
                            -- Yes.
                            modify (Map.insert sym [])

                            let allProds = [ (params, prod) | RuleDef params prods <- defs, prod <- prods ]

                            -- Instantiate productions
                            let (instantiatedProds, newSymsLists) = unzip $ map (instantiateProd sym args) allProds

                            modify (Map.insert sym instantiatedProds)

                            process (concat newSymsLists ++ rest)

    instantiateProd :: Sym -> [Sym] -> ([Text], [Sym]) -> (Production, [Sym])
    instantiateProd (Sym name _) args (params, prod) =
        if length params /= length args
            then error $ "Arity mismatch for " ++ show (Sym name args)
            else
                let env = Map.fromList $ zip params args
                    prod' = map (substitute env) prod
                in (prod', prod')

    substitute :: Map Text Sym -> Sym -> Sym
    substitute env (Sym n a) =
        case Map.lookup n env of
            Just replacement ->
                 if null a then replacement
                 else error "Higher-order arguments not supported"
            Nothing -> Sym n (map (substitute env) a)
