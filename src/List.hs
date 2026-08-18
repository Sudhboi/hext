module List where

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index val list = start ++ (val : drop 1 end)
 where
  (start, end) = splitAt index list

insertAt :: Int -> a -> [a] -> [a]
insertAt index val list = start ++ (val : end)
 where
  (start, end) = splitAt index list

splitAndFlatten :: Int -> Int -> [[a]] -> [[a]]
splitAndFlatten outerindex innerindex outerlist = start ++ [new1, new2] ++ drop 1 end
 where
  oldStr = outerlist !! outerindex
  (new1, new2) = splitAt innerindex oldStr
  (start, end) = splitAt outerindex outerlist
