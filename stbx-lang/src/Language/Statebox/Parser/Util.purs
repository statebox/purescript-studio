module Language.Statebox.Parser.Util where

import Prelude
import Control.Monad.State (gets)
import Data.Array (some, many)
import Data.String.CodeUnits (fromCharArray, singleton)
import Data.String as String
import Data.String.Regex as R
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Text.Parsing.Parser (Parser, ParserT, ParseState(ParseState), fail)
import Text.Parsing.Parser.Combinators (skipMany, optional)
import Text.Parsing.Parser.Pos (Position)
import Text.Parsing.Parser.String (satisfy)

-- adapted from https://github.com/slamdata/purescript-markdown/blob/master/src/Text/Markdown/SlamDown/Parser/Inline.purs

isAlphaNum :: Char -> Boolean
isAlphaNum = isAlpha || isDigit

isAlpha :: Char -> Boolean
isAlpha = isAlphaLower || isAlphaUpper

isAlphaLower :: Char -> Boolean
isAlphaLower c = c >= 'a' && c <= 'z'

isAlphaUpper :: Char -> Boolean
isAlphaUpper c = c >= 'A' && c <= 'Z'

isDigit :: Char -> Boolean
isDigit c = c >= '0' && c <= '9'

isWhitespace :: Char -> Boolean
isWhitespace = R.test wsRegex <<< singleton
  where
    wsRegex = unsafeRegex "^\\s$" noFlags
    flags = { unicode: false
            , sticky: false
            , multiline: false
            , ignoreCase: false
            , global: false
            }

someOf :: (Char -> Boolean) -> Parser String String
someOf p = fromCharArray <$> some (satisfy p)

manyOf :: (Char -> Boolean) -> Parser String String
manyOf p = fromCharArray <$> many (satisfy p)

inside :: forall a b. Parser String a -> Parser String b -> Parser String a
inside x ignore = ignore *> x <* ignore

spaces :: Parser String Unit
spaces = skipMany (satisfy isWhitespace)

hspaces :: Parser String Unit
hspaces = skipMany (satisfy (isWhitespace && (_ /= '\n')))

--------------------------------------------------------------------------------

-- | @ getPosition @ returns current position
-- | should probably be added to Text.Parsing.Parser.Pos
-- |
-- | Taken from module @ Text.Parsing.Indent @:
-- | https://github.com/purescript-contrib/purescript-parsing/blob/d917ef2125c8ac8dea54fb24f40098ae1ab92eb4/src/Text/Parsing/Parser/Indent.purs#L71-L74.
getPosition :: forall m s. (Monad m) => ParserT s m Position
getPosition = gets \(ParseState _ pos _) -> pos

