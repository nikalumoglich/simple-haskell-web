module Web where

import Data.List

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parseGet :: String -> [(String, String)]
parseGet i = map (parseSingleEntry) (wordsWhen (=='&') i) where
        parseSingleEntry j = ((wordsWhen (=='=') j)!!0, (wordsWhen (=='=') j)!!1                              )

valueByKey key xs = snd ((filter (\x -> fst x == key) xs)!!0)
