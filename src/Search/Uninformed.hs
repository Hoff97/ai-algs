module Search.Uninformed where

import Search.Queue

data Node a = Node {value :: a, parent :: Maybe (Node a)}

path :: Node a -> [a]
path (Node a Nothing) = [a]
path (Node a (Just as)) = a:path as

bfs :: Fifo (Node a) -> (a -> Bool) -> (a -> [a]) -> Maybe [a]
bfs as g n
  | empty as = Nothing
  | g $ value a = Just $ path a
  | otherwise = bfs (append rs . fmap (\v -> Node v (Just a)) . n . value $ a) g n
  where
    Just (a,rs) = pop as

dfs :: [Node a] -> (a -> Bool) -> (a -> [a]) -> Maybe [a]
dfs as g n
  | empty as = Nothing
  | g $ value a = Just $ path a
  | otherwise = dfs ((++rs) . fmap (\v -> Node v (Just a)) . n . value $ a) g n
  where
    Just (a,rs) = pop as
