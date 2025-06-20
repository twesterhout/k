{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Parse where

import Data.Sequence qualified as S
import Data.Sequence (Seq)
import Data.Text (Text)
import Text.Parsec qualified as P
import Text.Parsec ((<?>),(<|>))
import Text.Parsec.Pos (initialPos)
import Data.Functor ((<&>))
import Prelude hiding (lex)
import Language.Lex (Adverb(..), Tk(..), Token(..), Number(..), lex)

data K = KNumber !Number
       | KSym !Text
       | KStr !Text
       | KArray !(Seq Tree)
       | KVerb !Char !Bool
       | KAdverb !Adverb
       | KFunc !(Maybe (Seq Text)) !(Seq Tree)
       | KBlock !(Seq Tree)
  deriving stock (Show)

data Tree = Lit !K | Id !Text | At !Tree !(Seq Tree) | Ap !Tree !Tree | Ap2 !Tree !Tree !Tree
  deriving stock (Show)

token = P.tokenPrim show (\pos _c _cs -> pos) 
satisfy f = token (\c -> if f c then Just c else Nothing)
satisfy' f = satisfy (\(Token t _) -> f t)
semi = satisfy' (== TSemicolon) <?> "semicolon"
semi' = sws *> semi <* sws
sp = satisfy' (== TSpace) <?> "space"
ws = satisfy' (\t -> t == TSpace || t == TNewline) <?> "whitespace"
sws = P.optional ws
name = satisfy f <?> "name" where f (Token (TName _) _) = True; f _ = False
lparen = satisfy' (== TLParen) <?> "("; rparen = satisfy' (== TRParen) <?> ")"
lbrace = satisfy' (== TLBrace) <?> "{"; rbrace = satisfy' (== TRBrace) <?> "}"
lbracket = satisfy' (== TLBracket) <?> "["; rbracket = satisfy' (== TRBracket) <?> "]"
pUnary = token f <?> "unary verb" where f (Token (TVerb c True) _) = Just . Lit $ KVerb c True; f _ = Nothing
pBinary = token f <?> "binary verb" where f (Token (TVerb c False) _) = Just . Lit $ KVerb c False; f _ = Nothing
pAdverb = token f <?> "adverb" where f (Token (TAdverb a) _) = Just . Lit $ KAdverb a; f _ = Nothing
pStmts = S.fromList <$> P.sepBy pTree semi'


cont f = P.option f $ Ap f <$> (P.optional sp *> pTree)
cont2 f x = P.option (Ap f x) (Ap2 f x <$> pTree)

pBlock = P.between (lbracket <* sws) (sws *> rbracket) (S.fromList <$> P.sepBy pTree semi')
pAt f = At f <$> pBlock

pNoun = do n <- c1 <|> c2 <|> c3 <|> c4; (At n <$> pBlock) <|> pure n
  where
    -- literals
    c1 = token $ \case
      Token (TNumber x) _ -> Just $ Lit (KNumber x)
      Token (TNumbers xs) _ -> Just . Lit . KArray $ Lit . KNumber <$> xs
      Token (TSym s) _ -> Just . Lit . KSym $ s
      Token (TSyms ss) _ -> Just . Lit . KArray $ Lit . KSym <$> ss
      Token (TStr s) _ -> Just $ Lit (KStr s)
      Token (TName n) _ -> Just $ Id n
      _ -> Nothing
    -- (...)
    c2 = P.between (lparen <* sws) rparen $ do
      P.sepBy pTree semi' >>= \case
        [] -> pure . Lit . KArray $ S.Empty
        [t] -> pure t
        ts -> pure . Lit . KArray $ S.fromList ts
    -- {...}
    -- {[args]body}
    c3 = P.between lbrace rbrace $ do
      let getName (Token (TName t) _) = t; getName _ = error "bug"
          args = P.between (lbracket <* sws) (sws *> rbracket) (S.fromList . fmap getName <$> P.sepBy name semi')
      fmap Lit $ KFunc <$> P.optionMaybe args <*> pStmts
    -- [...]
    c4 = Lit . KBlock <$> pBlock

--     x y z ... 
-- c1  u        -> Apply x [parse y z ...]
-- c2  b a      -> Apply (Apply y [x]) [parse z ...]
--     b        -> Apply x [parse y z ...]
-- c31 n a      -> Apply (Apply y [x]) [parse z ...]
-- c32 n b a    -> Apply (Apply z [y]) [x, parse ...]
--     n b      -> Apply y [x, parse z ...]
-- c33 n n a    -> Apply (Apply z [y]) [x, parse ...]
--     n n      -> Apply x [parse y z ...]
-- c34 n
pTree :: P.Stream s m Token => P.ParsecT s u m Tree
pTree = c1 <|> c2 <|> c3
  where
    c1 = pUnary >>= cont
    c2 = do b <- pBinary; (cont . (`Ap` b) =<< pAdverb) <|> cont b
    c3 = do
      n <- pNoun <* P.optional sp
      let -- n a
          c31 = (Ap <$> pAdverb <*> pure n) >>= cont
          -- n b a | n b
          c32 = do b <- pBinary; k b n <|> cont2 b n
          -- n n a | n n
          c33 = do n' <- pNoun; k n' n <|> (Ap n <$> cont n')
      c31 <|> c32 <|> c33 <|> cont n
    k b x = do a <- pAdverb; cont2 (Ap a b) x
