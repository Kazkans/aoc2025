import Data.Char (digitToInt)

maxWithIndex :: Ord a => [a] -> (a, Int)
maxWithIndex xs =
    foldl1 select (zip xs [1..])
  where
    select acc@(v1, _) cur@(v2, _)
        | v2 > v1  = cur
        | otherwise = acc

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

solve1 bank =
    let (a, i) = maxWithIndex (init bank)
        b = maximum $ drop i bank
    in
        a * 10 + b

loop l 1 acc = 10*acc + maximum l
loop l n acc =
    let (a, i) = maxWithIndex (take (length l - n) l)
        l' = drop i l
    in
    (loop l' (n-1) (acc*10+a))

solve2 bank = loop bank 12 0

run lst =
    sum $
    map solve2 $
    map (map digitToInt) $
    wordsWhen (=='\n') lst


main = do
    contents <- readFile "anc3.txt"
    putStrLn (show (run contents))
