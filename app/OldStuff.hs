module OldStuff where

import System.Random (randomRIO)

a = 4

add :: Int -> Int -> Int
add x y = x + y

-- recursively count to ten
countToTen :: Int -> IO ()
countToTen n = do
    if n <= 10
        then do
            putStrLn $ show n
            countToTen (n + 1)
        else return ()

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Function to pick a random element from a list
pickRandom :: [a] -> IO a
pickRandom list = do
    index <- randomRIO (0, length list - 1)
    return $ list !! index

-- lets make a color enum
data Color = Red | Green | Blue | Yellow | Orange | Purple | Black | White deriving (Show, Enum, Bounded)

allColors :: [Color]
allColors = [minBound..maxBound]

-- lets make a type that is a person with a name, age, and favorite color
data Person = Person { name :: String, age :: Int, favoriteColor :: Color } deriving (Show)

-- consonants
consonants = "bcdfghjklmnpqrstvwxyz"
vowels = "aeiou"

-- function that generates a cons-vowel phoneme via picking a random consonant and a random vowel, combines into string
genPhoneme :: IO String
genPhoneme = do
    consonant <- pickRandom consonants
    vowel <- pickRandom vowels
    return [consonant, vowel]

-- func to generate a name given number of phonemes
genName :: Int -> IO String
genName 0 = return ""
genName n = do
    phoneme <- genPhoneme
    rest <- genName (n - 1)
    return $ phoneme ++ rest

-- function to gen a random person
-- makes a name between 1 and 5 phonemes long, age between 1 and 100, and a random color
genRandomPerson :: IO Person
genRandomPerson = do
    name_length <- pickRandom [1..5]
    name <- genName name_length
    age <- pickRandom [1..100]
    color <- pickRandom allColors
    return $ Person { name = name, age = age, favoriteColor = color }

-- function which determines number of syllables in name
numSyllables :: String -> Int
numSyllables [] = 0
numSyllables (c:cs) = if elem c vowels then 1 + numSyllables cs else numSyllables cs
-- how does the above line work
-- if the first character is a vowel, then we add 1 to the number of syllables in the rest of the string

-- function that does a sequence of things and printing
someShit :: IO ()   
someShit = do
    putStrLn $ show $ add 3 4
    putStrLn $ show a
    countToTen 1

    print $ factorial 5
    print $ fib 10

    -- lets make a list of nums from 1 to 10
    let nums = [1..10]
    print nums

    -- fib of all of them
    let fibs = map fib nums
    print fibs

    -- filter out the even ones
    let evens = filter even fibs
    print evens

    
    -- lets make a person
    let person = Person { name = "Bob", age = 25, favoriteColor = Blue }
    print person

    -- lets make a list of people
    let people = [Person { name = "Bob", age = 25, favoriteColor = Blue }, Person { name = "Alice", age = 30, favoriteColor = Red }]
    print people

    -- lets make a list of people and their favorite colors
    let peopleAndColors = map (\p -> (name p, favoriteColor p)) people

    -- make 100 random people
    randomPeople <- sequence $ replicate 100 genRandomPerson
    print randomPeople

    -- print them nicely
    mapM_ print randomPeople
    
    
    -- print if "e" is a vowel
    print $ elem 'e' vowels

