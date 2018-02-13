type Board = [Pos]
type Pos = (Int, Int)

width :: Int
width = 50

height :: Int
height = 50

glider :: Board
glider = [(1, 1), (1, 2), (2, 2), (2, 1), (2, 3)]

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbors :: Pos -> [Pos]
neighbors (x, y) = map wrap [(x-1, y-1), (x, y-1),
                             (x+1, y-1), (x-1, y),
                             (x+1, y), (x-1, y+1),
                             (x, y+1), (x+1, y+1)]

wrap :: Pos -> Pos
wrap (x, y) = (((x-1) `mod` width) + 1,
               ((y-1) `mod` height) + 1)

liveneighbors :: Board -> Pos -> Int
liveneighbors b = length . filter (isAlive b) . neighbors

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbors b p) [2, 3]]

births :: Board -> [Pos]
births b = [(x, y) | x <- [1..width],
                     y <- [1..height],
                     isEmpty b (x, y),
                     liveneighbors b (x, y) == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do
  cls
  showcells b
  wait 500000
  life (nextgen b)

cls :: IO ()
cls = putStr "\ESC[2J"

writeat :: Pos -> String -> IO ()
writeat p xs = do
  goto p
  putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

