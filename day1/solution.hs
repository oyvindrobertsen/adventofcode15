parenToInt :: Char -> Int
parenToInt c = if c == '(' then 1 else if c == ')' then -1 else 0

findBasementIndex :: [Int] -> Int -> Int -> Int
findBasementIndex [] acc index =
    if acc == -1 then
        index
    else
        -1
findBasementIndex (x:xs) acc index = 
    if acc == -1 then
        index 
    else 
        findBasementIndex xs (acc + x) (index + 1)

oneline = readFile "input.txt" >>= return . sum . map parenToInt >>= print

main = do
    contents <- readFile "input.txt"
    let num = map parenToInt contents
    print $ sum num
    print $ findBasementIndex num 0 0
