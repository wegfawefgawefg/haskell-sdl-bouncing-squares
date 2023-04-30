-- operator defin charset: !#$%&*+./<=>?@\^|-~:

-- wtf dis dog goin on about
dog = 5
-- dog a = 0 + 5

add :: Int -> Int -> Int
add a b = do
    -- putStrLn "doing int version"
    a + b

-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

-- onlyoverthree :: Int -> Maybe v
onlyoverthree x = if x > 3
    then Just True
    else Nothing

-- print out the type of onlyoverthree:
-- :t onlyoverthree

main :: IO ()
main = do
    putStrLn "hello world"
    putStrLn $ show $ dog
    putStrLn $ show $ add 5 6
    putStrLn $ show $ onlyoverthree 2
    putStrLn $ show $ onlyoverthree 5
    putStrLn $ show $ :t onlyoverthree



-- onlyoverthree x
--     | x > 3 = True
--     | otherwise = False

-- the $ operator is used to avoid parentheses, it is defined as:
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x
-- so, putStrLn $ show $ dog is the same as putStrLn (show dog)

