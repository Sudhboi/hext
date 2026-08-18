module List where

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index val list = start ++ (val : drop 1 end)
 where
  (start, end) = splitAt index list

insertAt :: Int -> a -> [a] -> [a]
insertAt index val list = start ++ (val : end)
 where
  (start, end) = splitAt index list
