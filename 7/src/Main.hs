module Main where

import Data.Graph.DGraph
import Data.Graph.Traversal
import Data.Graph.Types
import Text.Parsec

type Bag = String
type Rule = (Bag, [(Bag, Int)])
type RuleGraph = DGraph Bag Int

main :: IO ()
main = do
  input <- getContents
  case parseRuleGraph input of
    Left error -> putStrLn $ "parse error: " ++ show error
    Right g -> putStrLn $ show $ [partA g, partB g]

parseRuleGraph :: String -> Either ParseError RuleGraph
parseRuleGraph input = fromList <$> parse (many rule) "(input)" input
  where
    rule = do
      container <- manyTill anyChar (try (string " bags contain "))
      contents <- (string "no other bags" >> return []) <|> sepBy1 item (string ", ")
      char '.'
      newline
      return (container, contents)
    item = do
      count <- read <$> many1 digit
      char ' '
      let suffix = if count == 1 then " bag" else " bags"
      bag <- manyTill anyChar (try (string suffix))
      return (bag, count)

partA :: RuleGraph -> Int
partA g = length (bfsVertices (transpose g) "shiny gold") - 1

partB :: RuleGraph -> Int
partB g = containedBags "shiny gold"
  where
    containedBags v = sum $ map (\(_, v', count) -> count * totalBags v') (reachableAdjacentVertices' g v)
    totalBags v = 1 + containedBags v
