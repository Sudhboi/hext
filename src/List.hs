module List where

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index val list = start ++ (val : drop 1 end)
 where
  (start, end) = splitAt index list

insertAt :: Int -> a -> [a] -> [a]
insertAt index val list = start ++ (val : end)
 where
  (start, end) = splitAt index list

removeAt :: Int -> [a] -> [a]
removeAt index list = start ++ drop 1 end
 where
  (start, end) = splitAt (index - 1) list

mergeAndFlatten :: Int -> [[a]] -> [[a]]
mergeAndFlatten index list = start ++ [new1 ++ new2] ++ drop 2 end
 where
  (start, end) = splitAt (index - 1) list
  new1 = list !! (index - 1)
  new2 = list !! index

splitAndFlatten :: Int -> Int -> [[a]] -> [[a]]
splitAndFlatten outerindex innerindex outerlist = start ++ [new1, new2] ++ drop 1 end
 where
  oldStr = outerlist !! outerindex
  (new1, new2) = splitAt innerindex oldStr
  (start, end) = splitAt outerindex outerlist
