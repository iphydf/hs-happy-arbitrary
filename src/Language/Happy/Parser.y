{
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Language.Happy.Parser
    ( parseGrammar
    , source
    ) where

import qualified Data.ByteString       as BS
import           Data.FileEmbed        (embedFile)
import           Data.Fix              (Fix (..))
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Language.Happy.Ast    (Node, NodeF (..))
import           Language.Happy.Lexer  (Alex, AlexPosn (..), Lexeme (..),
                                        alexError, alexMonadScan)
import           Language.Happy.Tokens (LexemeClass (..))
}

%expect 0

%name parseGrammar Grammar

%error {parseError}
%errorhandlertype explist
%lexer {lexwrap} {L _ Eof _}
%monad {Alex}
%tokentype {Term}
%token
    ID_NAME			{ L _ IdName			_ }

    '{code}'			{ L _ LitCode			_ }

    '%errorhandlertype'		{ L _ KwErrorhandlertype	_ }
    '%error'			{ L _ KwError			_ }
    '%expect'			{ L _ KwExpect			_ }
    '%left'			{ L _ KwLeft			_ }
    '%lexer'			{ L _ KwLexer			_ }
    '%monad'			{ L _ KwMonad			_ }
    '%name'			{ L _ KwName			_ }
    '%prec'			{ L _ KwPrec			_ }
    '%right'			{ L _ KwRight			_ }
    '%token'			{ L _ KwToken			_ }
    '%tokentype'		{ L _ KwTokentype		_ }

    '%%'			{ L _ PctPercentPercent		_ }
    '::'			{ L _ PctColonColon		_ }
    ':'				{ L _ PctColon			_ }
    ','				{ L _ PctComma			_ }
    '('				{ L _ PctLParen			_ }
    ')'				{ L _ PctRParen			_ }
    '|'				{ L _ PctPipe			_ }

    LIT_STRING			{ L _ LitString			_ }
    LIT_INTEGER			{ L _ LitInteger		_ }

%%

Grammar :: { NonTerm }
Grammar
:	Code Pragmas '%%' Rules Code			{ Fix $ Grammar $1 $2 $4 $5 }

Code :: { [Term] }
Code
:	'{code}'					{ [$1] }
|	Code '{code}'					{ $1 ++ [$2] }

Pragmas :: { [NonTerm] }
Pragmas
:	Pragma						{ [$1] }
|	Pragmas Pragma					{ $1 ++ [$2] }

Pragma :: { NonTerm }
Pragma
:	'%expect' LIT_INTEGER				{ Fix $ PragmaExpect $2 }
|	'%name' ID_NAME ID_NAME				{ Fix $ PragmaName $2 $3 }
|	'%errorhandlertype' ID_NAME			{ Fix $ PragmaErrorHandlerType $2 }
|	'%error' '{code}'				{ Fix $ PragmaError $2 }
|	'%lexer' '{code}' '{code}'			{ Fix $ PragmaLexer $2 $3 }
|	'%monad' '{code}'				{ Fix $ PragmaMonad $2 }
|	'%tokentype' '{code}'				{ Fix $ PragmaTokenType $2 }
|	'%token' Tokens					{ Fix $ PragmaToken $2 }
|	'%left' TokenNames				{ Fix $ PragmaLeft $2 }
|	'%right' TokenNames				{ Fix $ PragmaRight $2 }

TokenNames :: { [Term] }
TokenNames
:	TokenName					{ [$1] }
|	TokenNames TokenName				{ $1 ++ [$2] }

Tokens :: { [NonTerm] }
Tokens
:	Token						{ [$1] }
|	Tokens Token					{ $1 ++ [$2] }

Token :: { NonTerm }
Token
:	TokenName '{code}'				{ Fix $ Token $1 $2 }

TokenName :: { Term }
TokenName
:	LIT_STRING					{ $1 }
|	ID_NAME						{ $1 }

Rules :: { [NonTerm] }
Rules
:	Rule						{ [$1] }
|	Rules Rule					{ $1 ++ [$2] }

Rule :: { NonTerm }
Rule
:	RuleType					{ $1 }
|	RuleDefn					{ $1 }

RuleType :: { NonTerm }
RuleType
:	ID_NAME '::' '{code}'				{ Fix $ RuleType $1 $3 }

RuleDefn :: { NonTerm }
RuleDefn
:	ID_NAME ':' RuleLines				{ Fix $ RuleDefn $1 [] $3 }
|	ID_NAME '(' RuleParams ')' ':' RuleLines	{ Fix $ RuleDefn $1 $3 $6 }

RuleParams :: { [Term] }
RuleParams
:	ID_NAME						{ [$1] }
|	RuleParams ',' ID_NAME				{ $1 ++ [$3] }

RuleLines :: { [NonTerm] }
RuleLines
:	RuleLine					{ [$1] }
|	RuleLines '|' RuleLine				{ $1 ++ [$3] }

RuleLine :: { NonTerm }
RuleLine
:	'{code}'					{ Fix $ RuleLine [] $1 }
|	Symbols '{code}'				{ Fix $ RuleLine $1 $2 }
|	Symbols '%prec' ID_NAME '{code}'		{ Fix $ RuleLine $1 $4 }

Symbols :: { [NonTerm] }
Symbols
:	Symbol						{ [$1] }
|	Symbols Symbol					{ $1 ++ [$2] }

Symbol :: { NonTerm }
Symbol
:	ID_NAME						{ Fix $ Symbol $1 [] }
|	LIT_STRING					{ Fix $ Symbol $1 [] }
|	ID_NAME '(' SymbolArgs ')'			{ Fix $ Symbol $1 $3 }

SymbolArgs :: { [NonTerm] }
SymbolArgs
:	Symbol						{ [$1] }
|	SymbolArgs ',' Symbol				{ $1 ++ [$3] }

{
type Term = Lexeme Text
type NonTerm = Node Term

parseError :: Show text => (Lexeme text, [String]) -> Alex a
parseError (L (AlexPn _ line col) c t, options) =
    alexError $ ":" <> show line <> ":" <> show col <> ": Parse error near " <> show c <> ": "
        <> show t <> "; expected one of " <> show options

lexwrap :: (Lexeme Text -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

source :: BS.ByteString
#ifdef SOURCE
source = $(embedFile SOURCE)
#else
source = BS.empty
#endif
}
