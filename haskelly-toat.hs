import System.Random
import System.Environment

main = do
    args <- getArgs
    handleArgs args

handleArgs :: [String] -> IO ()
handleArgs [] = showHelp
handleArgs args = do
    gen <- getStdGen
    let vowelList = parseInput $ args !! 0
        newWord = getRandomWord vowelList gen
    putStrLn newWord

parseInput :: String -> [Bool]
parseInput(x:[]) = [(read [x]) == 0]
parseInput (x:xs) = b:parseInput xs
    where b = (read [x]) == 0


getRandomLetter :: RandomGen g => Bool -> g -> (Char, g)
getRandomLetter isVowel gen  = (letterSet !! idx, newGen)
    where vowels = ['a', 'e', 'i', 'o', 'u'] 
          letterSet = if isVowel then vowels else [c | c <- ['a'..'z'], not $ c `elem` vowels]
          (idx, newGen) = randomR (0, (length letterSet)-1) gen

getRandomWord :: RandomGen g => [Bool] -> g -> String
getRandomWord (v:[]) gen = [fst $ getRandomLetter v gen]
getRandomWord (v:vs) gen = randomLetter:(getRandomWord vs newGen)
    where (randomLetter, newGen) = getRandomLetter v gen

showHelp = do
    putStrLn ""
    putStrLn "Usage: Takes a string of 1s and 0s, returns a brand new word."
    putStrLn "0s are vowels. 1s are consonants."
    putStrLn ""
    putStrLn "Example:" 
    putStrLn "$ haskelly-toat \"10101010\""
    -- putStrLn "zupuceli"
    gen <- getStdGen
    let vowelList = parseInput "10101010"
        newWord = getRandomWord vowelList gen
    putStrLn newWord
    putStrLn ""

