{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Redundant return" #-}
module Assignment (markdownParser, convertADTHTML, getTime) where
--  paragraphParser, convertADTHTML

import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..))
import           Parser
import           Control.Applicative
import Data.Char (isDigit)
import Control.Monad
data ADT = 
  -- Your ADT **must** derive Show.
    Body [BodyEle]

  deriving (Show, Eq)

data BodyEle =
  FreeText [Text]
  | Image String String String
  | FootnoteRef String String
  | Quote [Text]
  | Blockquote [BodyEle]
  | Heading Int [Text]
  | Code String String
  | OrderedList [OrderedListItem]
  | EmptyList
  | Table TableHead [TableRow]
  deriving (Show, Eq)

data TableHead = TableHead [TableCell]
  deriving (Show, Eq)

data TableRow = TableRow [TableCell]
  deriving (Show, Eq)

data TableCell = TableCell [Text]
  deriving (Show, Eq)


data OrderedListItem =
  OrderedListItem [Text] BodyEle
  deriving (Show, Eq)

data Text = Bold String
            | Italic String
            | Strikethrough String
            | Link String String
            | Inline String
            | Footnote String
            | PlainText String
            deriving (Show, Eq)

-- Takes in a string and a function to create Text, returns a parser for that Text
textmodParser :: String -> (String -> Text) -> Parser Text
textmodParser d s = do
  _ <- string d
  x <- someTill (noneof "\n") (string d)
  return $ s x

-- Parses a string until a newline character
someTillNL :: Char -> Parser String
someTillNL y = someTill (isNot '\n') $ is y

-- Parses a newline character or end of input
eofOrnewline :: Parser ()
eofOrnewline = eof <|> void (is '\n')

-- Parses Italics
italicParser :: Parser Text
italicParser = textmodParser "_" Italic

-- Parses Bold
boldParser :: Parser Text
boldParser = textmodParser "**" Bold

-- Parses Strikethrough
strikethroughParser :: Parser Text
strikethroughParser = textmodParser "~~" Strikethrough

-- Parses a link, parsing spaces between ] and ()
linkParser :: Parser Text
linkParser = do
  _ <- is '['
  x <- someTillNL ']' <* inlineSpace
  _ <- is '('
  -- getting the string between the brackets
  y <- someTill char $ is ')'
  return $ Link x y

-- Parses Inline 
inlineParser :: Parser Text
inlineParser = textmodParser "`" Inline

-- Parses a footnote
footnoteParser :: Parser Text
footnoteParser = do
  _ <- string "[^"
  x <- posNum
  _ <- string "]"
  return $ Footnote x

-- Parses positive integers
posNum :: Parser String
posNum = (:) <$> isPosInt <*> many digit

-- Parses image
imageParser :: Parser BodyEle
imageParser = do
  -- removing non-newline spaces
  _ <- inlineSpace *> string "!["
  -- the string between the braces
  x <- someTillNL ']' <* inlineSpace
  _ <- is '('
  -- the string between the brackets and next non-newline space
  y <- some (noneof "\t\r\f\v ") <* inlineSpace
  _ <- is '"'
  -- the string between the quotes
  z <- someTillNL '"' <* inlineSpace
  _ <- is ')'
  return $ Image x y z

footnoteRefParser :: Parser BodyEle
footnoteRefParser = do
  _ <- inlineSpace
  _ <- string "[^"
  -- string between the brackets
  x <- posNum
  _ <- stringTok "]:"
  -- string until newline
  y <- some (isNot '\n')
  _ <- eofOrnewline
  return $ FootnoteRef x y

plainParser :: Parser Text
plainParser = do
    -- until a newline, space, or end of input
    x <- someTill (isNot '\n') (eof <|> lookAhead (void modParsers)
       <|> lookAhead (void (is '\n')))
    return $ PlainText x
-- parsers for text modifiers
modParsers :: Parser Text
modParsers = italicParser <|> boldParser <|> strikethroughParser 
  <|> footnoteParser <|> inlineParser <|> linkParser

-- parses a text, either a text with modifiers or plain text
textParsers :: Parser Text
textParsers = modParsers <|> plainParser

freeTextParser :: Parser BodyEle
freeTextParser = do
  -- make sure its not a blockquote
  _ <- lookAhead $ noneof ">"
  -- parse until a newline
  x <- many textParsers
  _ <- eofOrnewline
  return $ FreeText x

-- counts the occurances of a character
count :: Int -> Parser a -> Parser [a]
count n p = (:) <$> p <*> count (n + 1) p <|> pure []

headingParser :: Parser BodyEle
headingParser = do
  -- checks if its a valid heading
  x <- length <$> count 0 (is '#')
  guard (x /= 0 && x < 7)
  -- removes non-newline spaces and gets the string until a newline or end
  _ <- someInlineSpace
  y <- someTill textParsers eofOrnewline
  return $ Heading x y

-- Function that parses alternative headings, takes in a character and an int for heading level
altHeadingParser :: Char -> Int -> Parser BodyEle
altHeadingParser c i = do
  -- trim leading non-newline spaces
  _ <- inlineSpace
  -- parses texts until a newline
  x <- some (textParsers)
  _ <- is '\n'<* inlineSpace
  -- looks ahead for two characters in a row
  _ <- lookAhead (string [c, c])
  -- parses the same character until a newline or end
  _ <- someTill (is c) (is '\n') <|> someTill (is c) eof
  return $ Heading i x

heading1Parser :: Parser BodyEle
heading1Parser = altHeadingParser '=' 1

heading2Parser :: Parser BodyEle
heading2Parser = altHeadingParser '-' 2

-- combines all heading parsers
allHeadingParser :: Parser BodyEle
allHeadingParser = headingParser <|> heading1Parser <|> heading2Parser 

-- Parses a blockquote
blockquoteParser :: Parser BodyEle
blockquoteParser = do
  x <- some quoteParser
  return $ Blockquote x

-- Parses a quote
quoteParser :: Parser BodyEle
quoteParser = do
  -- trim leading spaces
  _ <- inlineSpace
  -- parse > and 0 or more texts
  _ <- is '>' <* inlineSpace
  x <- many textParsers
  _ <- eofOrnewline
  return $ Quote x

codeParser :: Parser BodyEle
codeParser = do
  -- removes non-newline spaces
  _ <- inlineSpace
  _ <- string "```"
  -- parses the language
  x <- manyTill char ( is '\n')
  -- makes sure its an accepted ending
  y <- manyTill char (string "\n```")
  _ <- eofOrnewline
  return $ Code x y

orderedListParser :: Int -> Parser BodyEle
orderedListParser i = do
  x <- orderedListItemParser i (string "1")
  y <- many (orderedListItemParser i posNum)
  -- combines all the ordered list items
  return $ OrderedList (x : y)

-- Takes in indentation required and parser for 1 or a positive number, parses the list
orderedListItemParser :: Int -> Parser String -> Parser OrderedListItem
orderedListItemParser i p = do
  -- makes sure the indentation is correct
    l <- length <$> count 0 (is ' ')
    guard (l == i)
    -- parses p
    _ <- p
    _ <- is '.'
    -- trim leading non-newline spaces and parse texts until a newline or end of input
    _ <- someInlineSpace
    y <- some textParsers <* eofOrnewline
    -- checks for sublists
    x <- orderedListHelper i
    -- returns y and x, x is EmptyList if there is no sublists
    return $ OrderedListItem y x

-- Helper function for orderedList, parses sublists
orderedListHelper :: Int -> Parser BodyEle
orderedListHelper i = orderedListParser (i + 4) <|> pure EmptyList

tableParser :: Parser BodyEle
tableParser = do
  -- removes non-newline spaces, parses head, count the num of cols in head, makes sure table bodies have same length
  _ <- inlineSpace
  x <- tableHeadParser 
  let TableHead headerCount = x
  y <- many (tableRowParser (length headerCount))
  return $ Table x y

-- Parses table head and table seperator
tableHeadParser :: Parser TableHead
tableHeadParser = do
  -- parse heading row
  _ <- is '|'
  x <- many (tableCellParser)
  _ <- inlineSpace
  _ <- is '\n'
  _ <- inlineSpace *> is '|'
  -- and it's seperator
  y <- many (tableHeadSepParser)
  guard (length x == length y)
  _ <- inlineSpace
  _ <- eofOrnewline
  return $ TableHead x

-- Parses the seperator between head and body
tableHeadSepParser :: Parser String
tableHeadSepParser = do
  _ <- inlineSpace
  -- checks if there is at least 3 - chars
  _ <- lookAhead (string "---")
  x <- many (is '-')
  _ <- inlineSpace
  _ <- is '|'
  return $ x

tableRowParser :: Int -> Parser TableRow
tableRowParser i = do
  _ <- inlineSpace
  _ <- is '|'
  -- parses cells, checking if valid length
  x <- many (tableCellParser)
  guard (length x == i)
  _ <- inlineSpace
  _ <- eofOrnewline
  return $ TableRow x

tableCellParser :: Parser TableCell
tableCellParser = do
  _ <- inlineSpace
  -- parses text until '|', then parses the '|' character
  content <- manyTill (modParsers <|> tablePlainParser) (is '|')
  return $ TableCell $ content

-- plain text parser for table, not parsing in '|'
tablePlainParser :: Parser Text
tablePlainParser = do
  x <- someTill (noneof "\n|") (eof <|> lookAhead (void modParsers) 
    <|> lookAhead (void (is '\n')) <|> lookAhead (void (is '|')))
  return $ PlainText x

-- trim leading and trailing whitespaces, and build the Body ADT
markdownParser :: Parser ADT
markdownParser = do
  _ <- spaces
  _ <- endSpaces
  x <- many (tableParser <|> orderedListParser 0 <|> 
    imageParser <|> footnoteRefParser <|> allHeadingParser <|> 
    codeParser <|> blockquoteParser <|> freeTextParser)
  return $ Body x


getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: ADT -> String
convertADTHTML (Body x) = htmlHead ++ concatMap (`convertBodyEleHTML` 4) x ++ htmlTail
  where
    htmlHead = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n"
    htmlTail = "</body>\n\n</html>\n"

-- parses body elements, with indents
convertBodyEleHTML :: BodyEle -> Integer -> String
convertBodyEleHTML (FreeText x) i = addIndent i ++ "<p>" ++ concatMap convertTextHTML x ++ "</p>\n"
convertBodyEleHTML (Image x y z) i = addIndent i ++  "<img src=\"" ++ y ++ "\" alt=\"" ++ x ++ "\" title=\"" ++ z ++ "\">\n"
convertBodyEleHTML (FootnoteRef x y) i = addIndent i ++ "<p id=\"fn" ++ x ++ "\">" ++ y ++ "</p>\n"
convertBodyEleHTML (Heading i x) s = addIndent s ++ "<h" ++ show i ++ ">" ++ concatMap convertTextHTML x ++ "</h" ++ show i ++ ">\n"
convertBodyEleHTML (Blockquote x) i = addIndent i ++ "<blockquote>\n" ++ concatMap (\n -> convertBodyEleHTML n (i + 4)) x ++ addIndent i ++ "</blockquote>\n"
convertBodyEleHTML (Code "" y) i = addIndent i ++ "<pre><code>"++y ++ "\n"++addIndent i ++ "</code></pre>\n"
convertBodyEleHTML (Code x y) i = addIndent i ++ "<pre><code class=\"language-" ++ x ++ "\">"++y ++ "\n"++addIndent i ++ "</code></pre>\n"
convertBodyEleHTML (OrderedList x) i = addIndent i ++ "<ol>\n" ++ concatMap (\n -> convertOrderedListHTML n (i+4)) x ++ addIndent i ++ "</ol>\n"
convertBodyEleHTML (Table x y) i = addIndent i ++ "<table>\n" ++ convertTableHeadHTML x (i+4) ++ concatMap (\n -> convertTableBodyHTML n (i+4)) y++addIndent i++ "</table>\n"
convertBodyEleHTML EmptyList _ = ""
convertBodyEleHTML (Quote x) i = addIndent i ++ "<p>" ++ concatMap convertTextHTML x ++ "</p>\n"

-- texts have no indents
convertTextHTML :: Text -> String
convertTextHTML (Bold x) = "<strong>" ++ x ++ "</strong>"
convertTextHTML (Italic x) = "<em>" ++ x ++ "</em>"
convertTextHTML (Strikethrough x) = "<del>" ++ x ++ "</del>"
convertTextHTML (Link x y) = "<a href=\"" ++ y ++ "\">" ++ x ++ "</a>"
convertTextHTML (Inline x) = "<code>" ++ x ++ "</code>"
convertTextHTML (Footnote x) = "<sup><a id=\"fn" ++ x ++ "ref\" href=\"#fn" ++ x ++ "\">" ++ x ++ "</a></sup>"
convertTextHTML (PlainText x) = x

-- builds the ordered list
convertOrderedListHTML :: OrderedListItem -> Integer -> String
convertOrderedListHTML (OrderedListItem x EmptyList) i = addIndent i ++ "<li>" ++ concatMap convertTextHTML x ++ "</li>\n"
convertOrderedListHTML (OrderedListItem x y) i = addIndent i ++ "<li>" ++ concatMap convertTextHTML x ++ "\n"++convertBodyEleHTML y (i+4) ++ addIndent i++"</li>\n"

-- builds the table head
convertTableHeadHTML :: TableHead -> Integer -> String
convertTableHeadHTML (TableHead x) i = addIndent i ++ "<tr>\n" ++ concatMap (\n -> convertTableHeadCellHTML n (i+4)) x ++ addIndent i ++ "</tr>\n"

-- builds the table body
convertTableBodyHTML :: TableRow -> Integer -> String
convertTableBodyHTML (TableRow x) i = addIndent i ++ "<tr>\n" ++ concatMap (\n -> convertTableBodyCellHTML n (i+4)) x ++ addIndent i ++ "</tr>\n"

-- cells for table body
convertTableBodyCellHTML :: TableCell -> Integer -> String
convertTableBodyCellHTML (TableCell x) i = addIndent i ++ "<td>" ++ concatMap convertTextHTML x ++ "</td>\n"

-- cells for head body
convertTableHeadCellHTML :: TableCell -> Integer -> String
convertTableHeadCellHTML (TableCell x) i = addIndent i ++ "<th>" ++ concatMap convertTextHTML x ++ "</th>\n"


-- helper function to add indents
addIndent :: Integer -> String
addIndent i = replicate (fromInteger i) ' '