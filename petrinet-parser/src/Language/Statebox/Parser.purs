module Language.Statebox.Parser where

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

import Language.Statebox.AST (Node(..), NodeF(..), MultiEdge(..), MultiEdgeF(..), Label, Type, LabelAndType, GElem1(..), GElem1F(..))
import Language.Statebox.Parser.Util (getPosition, hspaces, inside, isAlphaNum, someOf)

graph1 :: Parser String (List GElem1)
graph1 = (gElem1 `inside` hspaces) `sepEndBy` (semicolon <|> newlines)
  where
    newlines = skipMany1 (char '\n')
    semicolon = const unit <$> char ';'

gElem1 :: Parser String GElem1
gElem1 = GMultiEdge1 <$> try multiEdge
     <|> GNode1      <$>     node

node :: Parser String Node
node = Node <$> labelWithoutType

nodes :: Parser String (List Node)
nodes = (node `inside` hspaces) `sepEndBy1` char ','

multiEdge :: Parser String MultiEdge
multiEdge = do
  lbl  <- pure <$> labelWithoutType
  _    <- hspaces
  _    <- string ":"
  _    <- hspaces
  src  <- nodes
  _    <- hspaces
  _    <- string "->"
  _    <- hspaces
  dest <- nodes
  pure $ MultiEdge lbl src dest

labelWithoutType :: Parser String LabelAndType
labelWithoutType = Tuple <$> label <*> pure Nothing

label :: Parser String Label
label = do
  start  <- getPosition
  ident' <- ident
  end    <- getPosition
  pure $ ident' /\ { start, end }

ident = someOf $ isAlphaNum || (_ == '_')
