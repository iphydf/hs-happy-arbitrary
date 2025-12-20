{
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData         #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Language.Happy.Lexer
    ( Alex
    , AlexPosn (..)
    , alexError
    , alexScanTokens
    , alexMonadScan
    , Lexeme (..)
    , lexemeClass
    , lexemePosn
    , lexemeText
    , lexemeLine
    , runAlex
    ) where

import           Data.Aeson            (FromJSON, ToJSON)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import           GHC.Generics          (Generic)
import           Language.Happy.Tokens (LexemeClass (..))
}

%wrapper "monad-bytestring"

tokens :-
<0>		"{"					{ begin hs }

<hs>		[^\{\}]+				{ mkL LitCode }
<hs>		"}"					{ begin 0 }
<hs>		"{-"					{ begin hsCmt }
<hsCmt>		"-}"					{ begin hs }
<hs,hsCmt>	(.|\n)					;

<0>		"%errorhandlertype"			{ mkL KwErrorhandlertype }
<0>		"%error"				{ mkL KwError }
<0>		"%expect"				{ mkL KwExpect }
<0>		"%left"					{ mkL KwLeft }
<0>		"%lexer"				{ mkL KwLexer }
<0>		"%monad"				{ mkL KwMonad }
<0>		"%name"					{ mkL KwName }
<0>		"%prec"					{ mkL KwPrec }
<0>		"%right"				{ mkL KwRight }
<0>		"%token"				{ mkL KwToken }
<0>		"%tokentype"				{ mkL KwTokentype }

<0>		"%%"					{ mkL PctPercentPercent }
<0>		"::"					{ mkL PctColonColon }
<0>		":"					{ mkL PctColon }
<0>		","					{ mkL PctComma }
<0>		"("					{ mkL PctLParen }
<0>		")"					{ mkL PctRParen }
<0>		"|"					{ mkL PctPipe }

<0>		"--"\n					;
<0>		"-- ".*					;
<0>		[\ \t\n]+				;
<0>		$white					{ mkE ErrorToken }
<0>		'(\\|[^'])*'				{ mkL LitString }
<0>		[A-Za-z][A-Za-z0-9_]*			{ mkL IdName }
<0>		[0-9]+					{ mkL LitInteger }

-- Error handling.
<0>		.					{ mkL ErrorToken }

{
deriving instance Generic AlexPosn
instance FromJSON AlexPosn
instance ToJSON AlexPosn

data Lexeme text = L AlexPosn LexemeClass text
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

instance FromJSON text => FromJSON (Lexeme text)
instance ToJSON text => ToJSON (Lexeme text)

mkL :: LexemeClass -> AlexInput -> Int64 -> Alex (Lexeme Text)
mkL c (p, _, str, _) len = pure $ L p c (piece str)
  where piece = Text.decodeUtf8 . LBS.toStrict . LBS.take len

mkE :: LexemeClass -> AlexInput -> Int64 -> Alex (Lexeme Text)
mkE c (p, _, str, _) len = alexError $ ": " <> show (L p c (piece str))
  where piece = Text.decodeUtf8 . LBS.toStrict . LBS.take len

lexemePosn :: Lexeme text -> AlexPosn
lexemePosn (L p _ _) = p

lexemeClass :: Lexeme text -> LexemeClass
lexemeClass (L _ c _) = c

lexemeText :: Lexeme text -> text
lexemeText (L _ _ s) = s

lexemeLine :: Lexeme text -> Int
lexemeLine (L (AlexPn _ l _) _ _) = l

alexEOF :: Alex (Lexeme Text)
alexEOF = return (L (AlexPn 0 0 0) Eof Text.empty)

alexScanTokens :: LBS.ByteString -> Either String [Lexeme Text]
alexScanTokens str =
    runAlex str $ loop []
  where
    loop toks = do
        tok@(L _ cl _) <- alexMonadScan
        if cl == Eof
            then return $ reverse toks
            else loop $! (tok:toks)
}
