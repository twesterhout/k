{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Parse where

import Data.Array qualified as A
import Data.Array (Array)
import Data.Char (isDigit, isAlphaNum, isAlpha)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Parsec qualified as P
import Text.Parsec ((<?>))
import Text.Parsec.Pos (SourcePos,initialPos)
import Prelude hiding (lex)
import Language.Lex

data K = KInt !Int
       | KFloat !Double
       | KSym !Text
       | KStr !Text
       | KArray !(Array Int Tree)
       | KVerb !Char !Bool
       | KAdverb !Text
  deriving stock (Show)

data Tree = Const !K | Variable !Text | Apply !K [K]
  deriving stock (Show)

data KExpr = KExpr
  deriving stock (Eq, Show)
--
-- {[args]body}
-- args is [x0;x1;x2;...] where x0, x1, etc are all Names
data KFunc = KFunc (Maybe [Text]) KExpr
  deriving stock (Eq, Show)

satisfy :: (P.Stream s m Token) => (Token -> Bool) -> P.ParsecT s u m Token
{-# INLINABLE satisfy #-}
satisfy f = P.tokenPrim show (\pos _c _cs -> pos) (\c -> if f c then Just c else Nothing)


semi = satisfy (\(Token t _) -> t == TSemicolon) <?> "semicolon"
ws = satisfy f <?> "whitespace" where f (Token TSpace _) = True; f (Token TNewline _) = True; f _ = False
sws = P.optional ws
name = satisfy f <?> "name" where f (Token (TName _) _) = True; f _ = False

pLambda = do
  P.between l r $ do
    args <- P.optionMaybe pFunArgs
    pure $ KFunc args KExpr
  where
    l = satisfy f where f (Token TLBrace _) = True; f _ = False
    r = satisfy f where f (Token TRBrace _) = True; f _ = False



pFunArgs = fmap p <$> P.between l r (P.sepBy (sws *> name <* sws) semi)
  where
    p (Token (TName t) _) = t; p _ = error "bug"
    l = satisfy f where f (Token TLBracket _) = True; f _ = False
    r = satisfy f where f (Token TRBracket _) = True; f _ = False


{-
tkSep TNewline = True; tkSep TSemicolon = True; tkSep TSpace = True; tkSep (TComment _) = True; tkSep _ = False
tkNoun (TInt _) = True; tkNoun (TFloat _) = True; tkNoun (TStr _) = True; tkNoun (TSym _) = True; tkNoun (TName _) = True; tkNoun (TKList _) = True; tkNoun _ = False
tkVerb (TVerb _ _) = True; tkVerb _ = False
tkAdverb (TAdverb _) = True; tkAdverb _ = False

number = flip P.label "Number" $ do
  let d = P.many1 (P.satisfy isDigit)
      o a b = P.optionMaybe $ liftA2 (:) a b
      s = liftA2 (<>) (P.option "" (pure <$> P.char '-'))
  n <- d; f <- o (P.char '.') d; e <- o (P.satisfy (\c -> c == 'e' || c == 'E')) (s d)
  pure $ if isNothing f && isNothing e
    then TInt (T.pack n) else TFloat (T.pack $ n <> fromMaybe "" f <> fromMaybe "" e)

string' = do
  _ <- P.char '"'
  let go s = P.anyToken >>= \c -> case c of
        '\\' -> do { c' <- P.char '\\' <|> P.char '"'; go (c':s) }
        '"' -> pure s
        _ -> go (c:s)
  T.reverse . T.pack <$> go ""
string = fmap TStr string' <?> "String"

name' = T.pack <$> liftA2 (:) (P.satisfy isAlpha) (P.many (P.satisfy isAlphaNum))
name = fmap TName name' <?> "Name"

symbol' = P.char '`' *> P.option T.empty (string' <|> name')
symbol = fmap TSym symbol' <?> "Symbol"

-- <Noun> ::= <Names> | <Ints> | <Floats> | <String> | <Symbols>
noun = string <|> symbol <|> number <|> name <?> "Noun"

-- <Verb>   ::=  <Verb1> | <Verb1> ":"
-- <Verb1>  ::=  ":" | "+" | "-" | "*" | "%" | "!" | "&" | "|" | "<" | ">" | "=" | "~" | "," |
--               "^" | "#" | "_" | "$" | "?" | "@" | "." | <Digit> ":"
verb = liftA2 TVerb (P.oneOf "+-*%&|<>=^!~,#_$?@.") (P.option False (P.char ':' $> True)) <?> "Verb"

-- <Adverb> ::=  "'" | "/" | "\" | "':" | "/:" | "\:"
adverb' = T.pack <$> liftA2 (:) (P.oneOf "'/\\") (P.option "" (pure <$> P.char ':'))
adverb = fmap TAdverb adverb' <?> "Adverb"

newline = P.endOfLine $> TNewline
semicolon = P.char ';' $> TSemicolon
space = P.many1 (P.char ' ' <|> P.char '\t') $> TSpace
space0 = P.many (P.char ' ' <|> P.char '\t') $> ()
klist = P.between (P.char '(' *> space0) (space0 <* P.char ')') (fmap TKList tokens) <?> "(...)"
plist = P.between (P.char '[' *> space0) (space0 <* P.char ']') (fmap TPList tokens) <?> "[...]"
func = P.between (P.char '{' *> space0) (space0 <* P.char '}') (fmap TFunc tokens) <?> "Lambda"

tokens = fmap spanV $ P.many $ space <|> newline <|> semicolon <|> func <|> klist <|> plist <|> verb <|> adverb <|> noun

isI (TInt _) = True; isI _ = False
isF (TFloat _) = True; isF _ = False

spanV = go1
  where
    go1 (a:b:c:xs)
      | TInt _ <- a, TSpace <- b, TInt _ <- c = let (y, xs') = go2 isI [c, a] xs in TKList (reverse y) : go1 xs'
    go1 xs = xs
    go2 f s (a:b:xs) | TSpace <- a, f b = go2 f (b:s) xs
    go2 _ s xs = (s, xs)

-- Skip whitespace (spaces, newlines, and comments)
sw = dropWhile f where f TSpace = True; f TNewline = True; f (TComment _) = True; f _ = False

parse :: [Tk] -> Maybe Tree
parse [] = Nothing
parse [t] = case t of
  TNewline -> Nothing
  TSemicolon -> Nothing
  TSpace -> Nothing
  TComment _ -> Nothing
  TInt s -> Just . Const . KInt . read . T.unpack $ s
  TFloat s -> Just . Const . KFloat . read . T.unpack $ s
  TStr s -> Just . Const . KStr $ s
  TSym s -> Just . Const . KSym $ s
  TName s -> Just . Variable $ s
  -- TKList ts -> Just . Const . KArray $ A.listArray (0, length ts - 1) ts
  _ -> undefined

-- data E = E !Tk
--        | A !Tk !E
--   deriving stock (Show)
-- 
-- pExpr = do
--   t <- noun <|> verb
--   e <- P.spaces *> P.optionMaybe pExpr
--   pure $ maybe (E t) (A t) e
-- pExprs = reverse <$> go []
--   where
--     go s = do
--       e <- P.spaces *> P.optionMaybe pExpr
--       let next = do
--             v <- (== '\n') <$> (P.char ';' <|> P.endOfLine)
--             go $ maybe s (\x -> (x, v):s) e
--           last = P.eof *> pure (maybe s (\x -> (x, True):s) e)
--       next <|> last

-- APL TWO BY TWO -- SYNTAX ANALYSIS BY PAIRWISE REDUCTION, J. D. Bunda and J. A. Gerth
-- https://dl.acm.org/doi/pdf/10.1145/384283.801081
--
-- Applied to K
-- https://nsl.com/papers/kparse.txt
-}

