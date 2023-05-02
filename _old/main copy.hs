-- operator defin charset: !#$%&*+./<=>?@\^|-~:

-- wtf dis dog goin on about
dog = 5

-- dog a = 0 + 5

add a b = do
  --   putStrLn "doing int version"
  a + b

-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

-- onlyoverthree :: Int -> Maybe v
onlyoverthree x =
  if x > 3
    then Just True
    else Nothing

main :: IO ()
main = do
  print $ add 5 6

-- onlyoverthree x
--     | x > 3 = True
--     | otherwise = False

-- the $ operator is used to avoid parentheses, it is defined as:
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x
-- so, putStrLn $ show $ dog is the same as putStrLn (show dog)
