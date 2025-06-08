{-# LANGUAGE LambdaCase, DerivingStrategies, OverloadedStrings, NoMonomorphismRestriction #-}
module Language.Lex (Adverb(..), Tk(..), Token(..), lex) where

import Data.Char (isDigit, isAlphaNum, isAlpha)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text (Text)
import Text.Parsec qualified as P
import Text.Parsec (SourcePos)
import Prelude hiding (lex)

--            '      /      \      \:         /:
data Adverb = Each | Over | Scan | EachLeft | EachRight
  deriving stock (Eq, Show)

-- Tokens
data Tk = TNewline | TSemicolon | TSpace | TComment !Text -- separators
        | TInt !Int | TFloat !Double | TStr !Text | TSym !Text | TName !Text -- syntactic nouns
        | TVerb !Char !Bool -- syntactic verb
        | TAdverb !Adverb -- adverb
        | TLParen | TRParen
        | TLBrace | TRBrace
        | TLBracket | TRBracket
        | TError !Text
  deriving stock (Eq, Show)

data Token = Token !Tk !SourcePos
  deriving stock (Eq, Show)

col1 p = P.incSourceColumn p 1

lex :: SourcePos -> String -> [Token]
lex _ [] = []
lex p (' ':cs) = let (n, cs') = readSpaces cs in Token TSpace p : lex (P.incSourceColumn p (n + 1)) cs'
lex p ('\t':cs) = let (n, cs') = readSpaces cs in Token TSpace p : lex (P.incSourceColumn p (n + 8)) cs'
lex p ('\n':cs) = let (n, cs') = readNewlines cs in Token TNewline p : lex (P.incSourceLine p (n + 1)) cs'
lex p (';':cs) = Token TSemicolon p : lex (col1 p) cs
lex p ('{':cs) = Token TLBrace p : lex (col1 p) cs
lex p ('}':cs) = Token TRBrace p : lex (col1 p) cs
lex p ('(':cs) = Token TLParen p : lex (col1 p) cs
lex p (')':cs) = Token TRParen p : lex (col1 p) cs
lex p ('[':cs) = Token TLBracket p : lex (col1 p) cs
lex p (']':cs) = Token TRBracket p : lex (col1 p) cs
lex p (c:cs) | c `elem` ("+-*%&|<>=^!~,#_$?@." :: String) =
  case cs of
    (':':cs') -> Token (TVerb c True) p : lex (P.incSourceColumn p 2) cs'
    _ -> Token (TVerb c False) p : lex (col1 p) cs
lex p ('\'':cs) = Token (TAdverb Each) p : lex (col1 p) cs
lex p ('/':':':cs) = Token (TAdverb EachRight) p : lex (P.incSourceColumn p 2) cs
lex p ('/':cs) = Token (TAdverb Over) p : lex (col1 p) cs
lex p ('\\':':':cs) = Token (TAdverb EachLeft) p : lex (P.incSourceColumn p 2) cs
lex p ('\\':cs) = Token (TAdverb Scan) p : lex (col1 p) cs
lex p ('"':cs) = lexStrLit (col1 p) cs
lex p ('`':cs) = lexSymLit p cs
lex p cs@(c:_) | isDigit c = lexNumLit p cs
lex p cs@(c:_) | isAlphaNum c = lexName p cs
lex p (c:_) = [Token (TError . T.pack $ "unexpected character " ++ show c) p]

readSpaces = go (0 :: Int)
  where go !n (' ':cs) = go (n + 1) cs
        go n ('\t':cs) = go (n + 8) cs
        go n cs = (n, cs)

readNewlines = go (0 :: Int)
  where go !n ('\n':cs) = go (n + 1) cs
        go n cs = (n, cs)

lexStrLit p = loop p "" 
  where
    loop l _ [] = [Token (TError "unterminated string literal") l]
    loop l rs ('"':cs) = Token (TStr (T.reverse (T.pack rs))) p : lex l cs
    loop l rs ('\\':'"':cs) = loop (P.incSourceColumn l 2) ('"':rs) cs
    loop l rs ('\\':'\\':cs) = loop (P.incSourceColumn l 2) ('\\':rs) cs
    loop l _ ('\\':c:_) = [Token (TError . T.pack $ "unrecognized escape sequence: " ++ show ['\\', c]) l]
    loop l rs ('\n':cs) = loop (P.incSourceLine l 1) ('\n':rs) cs
    loop l rs ('\t':cs) = loop (P.incSourceColumn l 8) ('\t':rs) cs
    loop l rs ('\r':cs) = loop l rs cs
    loop l rs (c:cs) = loop (P.incSourceColumn l 1) (c:rs) cs

readName = loop (0 :: Int) ""
  where
    loop n [] (c:cs) | isAlpha c = loop (n + 1) [c] cs
    loop n rs (c:cs) | isAlphaNum c = loop (n + 1) (c:rs) cs
    loop n rs cs = (T.reverse (T.pack rs), n, cs)

lexName p cs = let (x, n, rest) = readName cs in Token (TName x) p : lex (P.incSourceColumn p n) rest
-- Currently only recognizes symbols such as `a `hell123 etc., but not `"hello world"
lexSymLit p cs = let (x, n, rest) = readName cs in Token (TSym x) p : lex (P.incSourceColumn p (1 + n)) rest

-- Based on https://github.com/augustss/MicroHs/blob/785038ce0a24e60825152b5ec0eeb829f779e02b/src/MicroHs/Lex.hs#L139C1-L152C55
lexDecLit :: String -> Maybe (String, Int, String)
lexDecLit = loop "" 0
  where
    loop acc len (d : cs) | isDigit d = loop (d:acc) (len + 1) cs
    loop acc len rest = if len > 0 then Just (acc, len, rest) else Nothing

lexNumLit p cs = Token t p : lex (P.incSourceColumn p n) r
  where
    (t, n, r) = s0 cs
    s0 rest = case lexDecLit rest of
      Just (b, nb, rest') -> s1 b nb rest'
      Nothing -> error "lexNumLit should only be called if the next character is a digit"
    s1 acc nacc ('.':rest) = case lexDecLit rest of
      Just (f, nf, rest') -> s2 TFloat (f ++ "." ++ acc) (nf + 1 + nacc) rest'
      Nothing -> s2 TFloat ("." ++ acc) (1 + nacc) rest
    s1 acc nacc rest = s2 TInt acc nacc rest
    s2 _ acc nacc (e:pm:rest@(d:_)) | (pm == '+' || pm == '-') && cond e d = s3 (pm:e:acc) (2 + nacc) rest
    s2 _ acc nacc (e:rest@(d:_)) | cond e d = s3 (e:acc) (1 + nacc) rest
    s2 f acc nacc rest = (f . read . reverse $ acc, nacc, rest)
    s3 acc nacc rest = (TFloat . read . reverse $ e ++ acc, ne + nacc, rest')
      where (e, ne, rest') = fromJust (lexDecLit rest)
    cond e d = (e == 'e' || e == 'E') && isDigit d

