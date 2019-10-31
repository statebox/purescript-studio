module Language.Statebox.Net.Parser where

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

import Language.Statebox.AST (NodeF(..), HyperEdgeF(..), GElemF(..))
import Language.Statebox.Net.AST (Node(..), HyperEdge(..), LabelWithSpan, LabelWithSpanWithType, GElem(..))
import Language.Statebox.Parser.Util (getPosition, hspaces, inside, isAlphaNum, someOf)

graph1 :: Parser String (List GElem)
graph1 = (gElem `inside` hspaces) `sepEndBy` (semicolon <|> newlines)
  where
    newlines = skipMany1 (char '\n')
    semicolon = const unit <$> char ';'

gElem :: Parser String GElem
gElem = GHyperEdge <$> try hyperEdge
    <|> GNode      <$>     node

node :: Parser String Node
node = Node <$> labelWithoutType

nodes :: Parser String (List Node)
nodes = (node `inside` hspaces) `sepEndBy1` char ','

hyperEdge :: Parser String HyperEdge
hyperEdge = do
  lbl  <- labelWithoutType
  _    <- hspaces
  _    <- string ":"
  _    <- hspaces
  src  <- nodes
  _    <- hspaces
  _    <- string "->"
  _    <- hspaces
  targ <- nodes
  pure $ HyperEdge lbl src targ

labelWithoutType :: Parser String LabelWithSpanWithType
labelWithoutType = Tuple <$> label <*> pure Nothing

label :: Parser String LabelWithSpan
label = do
  start  <- getPosition
  ident' <- ident
  end    <- getPosition
  pure $ ident' /\ { start, end }

ident = someOf $ isAlphaNum || (_ == '_')
