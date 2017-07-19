import System.Random

getRandomLetter :: RandomGen g => Bool -> g -> (Char, g)
getRandomLetter isVowel gen  = (letterSet !! idx, newGen)
    where vowels = ['a', 'e', 'i', 'o', 'u'] 
          letterSet = if isVowel then vowels else [c | c <- ['a'..'z'], not $ c `elem` vowels]
          (idx, newGen) = randomR (0, (length letterSet)-1) gen
