import Safe

main :: IO ()
main = print $ headMay ([] :: [Int])
