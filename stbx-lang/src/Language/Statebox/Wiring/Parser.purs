module Language.Statebox.Wiring.Parser where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.List (List(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String

import Language.Statebox.Hypergraph (NodeF(..), HyperEdgeF(..), GElemF(..))
import Language.Statebox.Wiring.AST (Node(..), HyperEdge(..), LabelWithSpan, GElem(..))
import Language.Statebox.Parser.Util (getPosition, hspaces, inside, isAlphaNum, someOf)

diagram :: Parser String (List GElem)
diagram = (gElem `inside` hspaces) `sepEndBy` (semicolon <|> newlines)
  where
    newlines = skipMany1 (char '\n')
    semicolon = const unit <$> char ';'

gElem :: Parser String GElem
gElem = GHyperEdge <$> try hyperEdge
    <|> GNode      <$>     node

node :: Parser String Node
node = Node <$> label

hyperEdge :: Parser String HyperEdge
hyperEdge = do
  src  <- labels
  _    <- hspaces
  _    <- string "->"
  _    <- hspaces
  targ <- labels
  pure $ HyperEdge unit src targ

label :: Parser String LabelWithSpan
label = do
  start  <- getPosition
  ident' <- ident
  end    <- getPosition
  pure $ ident' /\ { start, end }

labels :: Parser String (List LabelWithSpan)
labels = (label `inside` hspaces) `sepEndBy1` char ','

ident :: Parser String String
ident = someOf $ isAlphaNum || (_ == '_')
