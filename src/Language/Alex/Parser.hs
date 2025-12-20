{-# LANGUAGE OverloadedStrings #-}
module Language.Alex.Parser
    ( parseAlex
    ) where

import           Control.Applicative  (empty, optional, (<|>))
import           Data.Attoparsec.Text
import           Data.Char            (chr, isAlphaNum, isDigit, isSpace, ord)
import           Data.Functor         (($>))
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Language.Alex.Ast

parseAlex :: Text -> AlexFile
parseAlex input =
    case parse alexFile input of
        Done "" res -> res
        Done rest _ -> error $ "Parsed, but input remains: " ++ show (Text.take 50 rest)
        Fail i ctxs err -> error $ "Failed to parse Alex file: " ++ err ++ " at " ++ show (Text.take 20 i) ++ " context: " ++ show ctxs
        Partial k -> case k "" of
            Done "" res -> res
            Done rest _ -> error $ "Parsed (after partial), but input remains: " ++ show (Text.take 50 rest)
            Fail i _ err -> error $ "Failed (after partial): " ++ err ++ " at " ++ show (Text.take 20 i)
            Partial _ -> error "Failed: Partial result after feeding empty string"

alexFile :: Parser AlexFile
alexFile = do
    whiteSpace
    _ <- optional codeBlock -- Header
    whiteSpace
    _ <- optional wrapper
    whiteSpace
    macs <- many' macro
    whiteSpace
    _ <- string "tokens" >> whiteSpace >> string ":-"
    whiteSpace
    -- rs <- many' rule
    rs <- manyRules
    whiteSpace
    _ <- optional codeBlock -- Footer
    whiteSpace
    endOfInput
    return $ AlexFile macs rs

manyRules :: Parser [Rule]
manyRules = do
    c <- peekChar
    case c of
        Nothing -> return []
        Just _ -> do
            -- Try to parse a rule. If it fails without consuming, return [].
            -- If it fails consuming, the whole parser fails.
            res <- (Just <$> rule) <|> return Nothing
            case res of
                Nothing -> return []
                Just r -> do
                    rs <- manyRules
                    return (r:rs)

wrapper :: Parser ()
wrapper = do
    _ <- string "%wrapper"
    whiteSpace
    _ <- stringLitRaw
    return ()

macro :: Parser (Text, Regex)
macro = do
    name <- char '$' >> takeWhile1 isIdent
    whiteSpace
    _ <- char '='
    whiteSpace
    r <- regex
    whiteSpace
    _ <- optional (char ';')
    whiteSpace
    return (name, r)

rule :: Parser Rule
rule = do
    _ <- peekChar
    startCodes <- option [] (between (char '<') (char '>') (sepBy1 (takeWhile1 isIdent) (char ',') ) <* whiteSpace) <?> "startCodes"
    r <- regex <?> "regex"
    whiteSpace
    code <- codeBlock <?> "codeBlock"
    whiteSpace
    let codeText = Text.pack code
    return $ Rule startCodes r codeText (extractTokenName codeText)

extractTokenName :: Text -> Maybe Text
extractTokenName code =
    let ws = words (Text.unpack code)
    in case dropWhile (/= "mkL") ws of
        (_:token:_) -> Just (Text.pack token)
        _           -> Nothing

-- Helpers

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c == '_' || c == '\''

whiteSpace :: Parser ()
whiteSpace = skipMany (satisfy isSpace <|> comment)

comment :: Parser Char
comment = do
    _ <- string "--"
    _ <- takeWhile1 (/= '\n')
    char '\n'

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

-- Code Blocks

codeBlock :: Parser String
codeBlock = braced <|> semi
  where
    braced = do
        _ <- char '{'
        s <- codeContent
        _ <- char '}'
        return s
    semi = char ';' >> return ";"

codeContent :: Parser String
codeContent = do
    c <- peekChar
    case c of
        Nothing -> return ""
        Just '}' -> return ""
        Just '{' -> do
            _ <- char '{'
            inner <- codeContent
            _ <- char '}'
            rest <- codeContent
            return $ "{" ++ inner ++ "}" ++ rest
        Just x -> do
            _ <- char x
            rest <- codeContent
            return $ x : rest

-- Regex

regex :: Parser Regex
regex = union_

union_ :: Parser Regex
union_ = do
    parts <- sepBy1 seq_ (try (whiteSpace >> char '|' >> whiteSpace))
    case parts of
        [x] -> return x
        xs  -> return $ RAlt xs

seq_ :: Parser Regex
seq_ = do
    parts <- many1 term
    case parts of
        [x] -> return x
        xs  -> return $ RSeq xs

term :: Parser Regex
term = do
    a <- atom
    option a $
            (char '*' $> RStar a)
        <|> (char '+' $> RPlus a)
        <|> (char '?' $> ROpt a)
        <|> (do
                _ <- char '{'
                _ <- takeWhile1 (\c -> isDigit c || c == ',')
                _ <- char '}'
                return (RPlus a) -- Approximation for {n,m}
            )

atom :: Parser Regex
atom =  (char '(' *> regex <* char ')')
    <|> (char '$' *> (RMacro <$> takeWhile1 isIdent))
    <|> stringLit
    <|> charSet
    <|> (RChar <$> escapedChar) -- Handle escapes
    <|> (RChar <$> satisfy (\c -> not (isSpace c) && c `notElem` ['{', '<', '|', ')', '*', '+', '?', '=', ';', ']']))

stringLit :: Parser Regex
stringLit = do
    s <- stringLitRaw
    return $ RString (Text.pack s)

stringLitRaw :: Parser String
stringLitRaw = do
    _ <- char '"'
    s <- many' stringChar
    _ <- char '"' <?> "closing quote"
    return s

stringChar :: Parser Char
stringChar = escapedChar <|> satisfy (/= '"')

escapedChar :: Parser Char
escapedChar = char '\\' *> (
        (char 'x' *> (chr <$> hexadecimal))
    <|> (char 'o' *> octal)
    <|> (do
            c <- peekChar'
            if isDigit c
                then chr <$> decimal
                else empty
        )
    <|> (char 'n' $> '\n')
    <|> (char 't' $> '\t')
    <|> (char 'r' $> '\r')
    <|> (char 'f' $> '\f')
    <|> (char 'v' $> '\v')
    <|> (char 'b' $> '\b')
    <|> (char 'a' $> '\a')
    <|> anyChar
    )

octal :: Parser Char
octal = do
    ds <- takeWhile1 (\c -> c >= '0' && c <= '7')
    let n = Text.foldl' (\acc c -> acc * 8 + (ord c - ord '0')) 0 ds
    return (chr n)

charSet :: Parser Regex
charSet = do
    _ <- char '['
    inv <- option False (char '^' $> True)
    ranges <- many1 range
    _ <- char ']'
    let set = Set.unions ranges
    return $ RSet set inv

range :: Parser (Set Char)
range = do
    start <- setChar
    end <- option Nothing (char '-' >> Just <$> setChar)
    case end of
        Nothing -> return $ Set.singleton start
        Just e  -> return $ Set.fromList [start..e]

setChar :: Parser Char
setChar = escapedChar <|> satisfy (\c -> c /= ']' && c /= '-')
