{-# LANGUAGE LambdaCase, DerivingStrategies, OverloadedStrings, NoMonomorphismRestriction #-}
module Language.Lex (Adverb(..), Tk(..), Token(..), Number(..), lex) where

import Data.Char (isDigit, isAlphaNum, isAlpha)
import Data.Sequence qualified as S
import Data.Sequence (Seq(..))
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text (Text)
import Text.Parsec qualified as P
import Text.Parsec (SourcePos)
import Prelude hiding (lex)

--            '      /      \      \:         /:
data Adverb = Each | Over | Scan | EachLeft | EachRight deriving stock (Eq, Show)
-- "+-*%&|<>=^!~,#_$?@."
-- data Verb = 

data Number = TInt !Int | TFloat !Double deriving stock (Eq, Show)

-- Tokens
data Tk = TNewline | TSemicolon | TSpace | TComment !Text -- separators
        | TNumber !Number | TStr !Text | TSym !Text | TName !Text -- syntactic nouns
        | TNumbers !(Seq Number) | TSyms !(Seq Text)
        | TVerb !Char !Bool -- syntactic verb
        | TAdverb !Adverb -- adverb
        | TLParen | TRParen
        | TLBrace | TRBrace
        | TLBracket | TRBracket
        | TError !Text
  deriving stock (Eq, Show)

data Token = Token !Tk !SourcePos
  deriving stock (Eq, Show)

col = P.incSourceColumn
col1 p = col p 1

lex :: SourcePos -> String -> [Token]
lex _ [] = []
lex p cs@(c:_) | c == ' ' || c == '\t' = let (n, cs') = readSpaces cs in Token TSpace p : lex (col p n) cs'
lex p ('\n':cs) = let (n, cs') = readNewlines cs in Token TNewline p : lex (P.incSourceLine p (n + 1)) cs'
lex p (';':cs) = Token TSemicolon p : lex (col1 p) cs
lex p ('{':cs) = Token TLBrace p : lex (col1 p) cs
lex p ('}':cs) = Token TRBrace p : lex (col1 p) cs
lex p ('(':cs) = Token TLParen p : lex (col1 p) cs
lex p (')':cs) = Token TRParen p : lex (col1 p) cs
lex p ('[':cs) = Token TLBracket p : lex (col1 p) cs
lex p (']':cs) = Token TRBracket p : lex (col1 p) cs
lex p (c:cs) | c `elem` (":+-*%&|<>=^!~,#_$?@." :: String) =
  case cs of
    (':':cs') -> Token (TVerb c True) p : lex (P.incSourceColumn p 2) cs'
    _ -> Token (TVerb c False) p : lex (col1 p) cs
lex p ('\'':cs) = Token (TAdverb Each) p : lex (col1 p) cs
lex p ('/':':':cs) = Token (TAdverb EachRight) p : lex (P.incSourceColumn p 2) cs
lex p ('/':cs) = Token (TAdverb Over) p : lex (col1 p) cs
lex p ('\\':':':cs) = Token (TAdverb EachLeft) p : lex (P.incSourceColumn p 2) cs
lex p ('\\':cs) = Token (TAdverb Scan) p : lex (col1 p) cs
lex p ('"':cs) = lexStrLit (col1 p) cs
lex p cs@('`':_) = lexSymLits p cs
lex p cs@(c:_) | isDigit c = lexNumLits p cs
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
readSym ('`':cs) = let (x, n, rest) = readName cs in (x, n + 1, rest)
readSym _ = error "readSym should only be called if the next character is `"

lexName p cs = let (x, n, rest) = readName cs in Token (TName x) p : lex (P.incSourceColumn p n) rest
-- Currently only recognizes symbols such as `a `hell123 etc., but not `"hello world"
lexSymLits p0 = go Empty (0 :: Int)
  where
    go !acc !n cs@('`':_) = let (t, n', rest) = readSym cs in go (acc :|> t) (n + n') rest
    go acc n cs = Token (acc2token acc) p0 : lex (col p0 n) cs
    acc2token (t :<| Empty) = TSym t; acc2token ts = TSyms ts

-- Based on https://github.com/augustss/MicroHs/blob/785038ce0a24e60825152b5ec0eeb829f779e02b/src/MicroHs/Lex.hs#L139C1-L152C55
readDecLit :: String -> Maybe (String, Int, String)
readDecLit = loop "" 0
  where
    loop acc len (d : cs) | isDigit d = loop (d:acc) (len + 1) cs
    loop acc len rest = if len > 0 then Just (acc, len, rest) else Nothing

readNumLit = s0
  where
    s0 rest = case readDecLit rest of
      Just (b, nb, rest') -> s1 b nb rest'
      Nothing -> error "readNumLit should only be called if the next character is a digit"
    s1 acc nacc ('.':rest) = case readDecLit rest of
      Just (f, nf, rest') -> s2 TFloat (f ++ "." ++ acc) (nf + 1 + nacc) rest'
      Nothing -> s2 TFloat ("." ++ acc) (1 + nacc) rest
    s1 acc nacc rest = s2 TInt acc nacc rest
    s2 _ acc nacc (e:pm:rest@(d:_)) | (pm == '+' || pm == '-') && cond e d = s3 (pm:e:acc) (2 + nacc) rest
    s2 _ acc nacc (e:rest@(d:_)) | cond e d = s3 (e:acc) (1 + nacc) rest
    s2 f acc nacc rest = (f . read . reverse $ acc, nacc, rest)
    s3 acc nacc rest = (TFloat . read . reverse $ e ++ acc, ne + nacc, rest')
      where (e, ne, rest') = fromJust (readDecLit rest)
    cond e d = (e == 'e' || e == 'E') && isDigit d

-- lexNumLit p cs = Token (TNumber t) p : lex (col p n) r where (t, n, r) = readNumLit cs
lexNumLits p0 cs0 = go0 Empty p0 cs0
  where
    -- two states: S0 (parse digit) <-> S1 (parse space)
    go0 !acc !p cs = let (t, n, rest) = readNumLit cs in go1 (acc :|> t) (col p n) rest
    go1 !acc !p cs@(c:_) | c == ' ' || c == '\t' = 
      let (n, rest) = readSpaces cs
       in case rest of
            (c':_) | isDigit c' -> go0 acc (col p n) rest
            _ -> Token (acc2token acc) p0 : Token TSpace p : lex (col p n) rest
    go1 acc p cs = Token (acc2token acc) p0 : lex p cs
    acc2token (t :<| Empty) = TNumber t; acc2token ts = TNumbers ts
