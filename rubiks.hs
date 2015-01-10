data Color = White|Green|Orange|Yellow|Red|Blue
  deriving (Eq)

colorOff :: String
colorOff = "\x1b[0m"

instance Show Color where
  show White  = "W"
  show Green  = "\x1b[32mG" ++ colorOff
  show Orange = "\x1b[35mO" ++ colorOff
  show Yellow = "\x1b[33mY" ++ colorOff
  show Red    = "\x1b[31mR" ++ colorOff
  show Blue   = "\x1b[34mB" ++ colorOff

data Cube = Cube { front :: [Color]
                 , back :: [Color]
                 , left :: [Color]
                 , right :: [Color]
                 , top :: [Color]
                 , bottom :: [Color]
                 }
  deriving (Show, Eq)

data Move = Move (Cube -> Cube) String

instance Eq Move where
  (Move f s) == (Move f' s') = (s == s')

instance Show Move where
  show (Move f s) = show s

cubeListOrder :: [String]
cubeListOrder = ["front", "back", "left", "right", "top", "bottom"]

cubeToList :: Cube -> [[Color]]
cubeToList c = [front c, back c, left c, right c, top c, bottom c]

isSolved :: Cube -> Bool
isSolved c = isSolved' $ cubeToList c
  where isSolved' xs = foldr (&&) True (map solvedSide xs)

solvedSide :: [Color] -> Bool
solvedSide [] = True
solvedSide (x:xs) = foldr (&&) True (map (== x) xs)

cubeString :: Cube -> String
cubeString c = foldr (++) "" (map entitle $ zip cubeListOrder (cubeToList c))
  where entitle (a,b) = "[" ++ a ++ "]\n" ++ (sideAsString b)

sideAsString :: [Color] -> String
sideAsString s = asString s 1
  where asString [] n = ""
        asString (x:[]) n = show x ++ "\n"
        asString (x:xs) n = 
         if n `mod` 3 == 0
           then show x ++ "\n" ++ asString xs (n + 1)
           else show x ++ " " ++  asString xs (n + 1)

applyMove :: Move -> Cube -> Cube
applyMove (Move m _) c = m c

applyMoves :: [Move] -> Cube -> Cube
applyMoves [] c = c
applyMoves (x:xs) c = applyMoves xs (applyMove x c)

moves :: [Move]
moves = [rotateFrontRight, rotateZAxisRight, rotateLeftBack, rotateYAxisBack,
         rotateTopCounterC, rotateYAxisForward, rotateTopClockwise, rotateLeftForward,
         rotateXAxisRight, rotateFrontLeft, rotateXAxisLeft, rotateZAxisLeft]

base :: [[[Move]]]
base = [addMoves []]

addMovesOfNPlusOne :: [[[Move]]] -> [[[Move]]]
addMovesOfNPlusOne (x:xs) = (buildMoves x):(x:xs)
addMovesOfNPlusOne [] = base

addMoves :: [Move] -> [[Move]]
addMoves m = filter noMoveCycle $ filter noMoveUndo $ map (\x -> m ++ [x]) moves

buildMoves :: [[Move]] -> [[Move]]
buildMoves m = concatMap addMoves m

buildAll :: [[[Move]]]
buildAll = reverse $ buildAll' base 1
  where buildAll' xs 25 = xs
        buildAll' xs n = buildAll' (addMovesOfNPlusOne xs) (n + 1)

noMoveCycle :: [Move] -> Bool
noMoveCycle xs = noMoveCycle' (reverse xs)
  where noMoveCycle' (a:b:c:d:_) = 
          if (a == b) && (b == c) && (c == d)
            then False
            else True
        noMoveCycle' _ = True

noMoveUndo :: [Move] -> Bool
noMoveUndo xs = noMoveUndo' (reverse xs)
  where noMoveUndo' (x:y:_) = if x == (opposite y) then False else True
        noMoveUndo' _ = True

opposite :: Move -> Move
opposite (Move m s) = case s of
  "Rotate front face to the right" -> rotateFrontLeft
  "Rotate front face to the left" -> rotateFrontRight
  "Rotate left side backward"   -> rotateLeftForward
  "Rotate left side forward" -> rotateLeftBack
  "Rotate top clockwise" -> rotateTopCounterC
  "Rotate top counter clockwise" -> rotateTopClockwise
  "Rotate horizontal middle right" -> rotateXAxisLeft
  "Rotate horizontal middle left" -> rotateXAxisRight
  "Rotate vertical middle back" -> rotateYAxisForward
  "Rotate vertical middle forward" -> rotateYAxisBack
  "Rotate z axis right" -> rotateZAxisLeft
  "Rotate z axis left" -> rotateZAxisRight

-----------------------------

rotateFrontRight = Move frontRight "Rotate front face to the right"
rotateFrontLeft = Move frontLeft "Rotate front face to the left"

frontRight :: Cube -> Cube
frontRight c = Cube { front = permuteFaceRight (front c)
                    , back = back c
                    , left = [(l!!0),(l!!1),(b!!0),(l!!3),(l!!4),(b!!1),(l!!6),(l!!7),(b!!2)]
                    , right = [(t!!6),(r!!1),(r!!2),(t!!7),(r!!4),(r!!5),(t!!8),(r!!7),(r!!8)]
                    , top = (take 6 t) ++ [(l!!8),(l!!5),(l!!2)]
                    , bottom = [(r!!6),(r!!3),(r!!0)] ++ drop 3 b 
                    }
  where b = bottom c
        t = top c
        l = left c
        r = right c

frontLeft :: Cube -> Cube
frontLeft = frontRight . frontRight . frontRight

permuteFaceRight (a:b:c:d:e:f:g:h:i:[]) = [g, d, a, h, e, b, i, f, c]

-----------------------------

rotateLeftBack = Move leftBackward "Rotate left side backward"
rotateLeftForward = Move leftForward "Rotate left side forward"

leftForward :: Cube -> Cube
leftForward c = Cube { front = [(t!!0),(f!!1),(f!!2),(t!!3),(f!!4),(f!!5),(t!!6),(f!!7),(f!!8)]
                     , back = [(ba!!0),(ba!!1),(b!!6),(ba!!3),(ba!!4),(b!!3),(ba!!6),(ba!!7),(b!!0)] 
                     , left = permuteFaceRight (left c)
                     , right = right c
                     , top = [(ba!!8),(t!!1),(t!!2),(ba!!5),(t!!4),(t!!5),(ba!!2),(t!!7),(t!!8)] 
                     , bottom = [(f!!0),(b!!1),(b!!2),(f!!3),(b!!4),(b!!5),(f!!6),(b!!7),(b!!8)]
                     }
  where f = front c
        b = bottom c
        t = top c
        ba = back c

leftBackward = leftForward . leftForward . leftForward

-----------------------------

rotateTopClockwise = Move topC "Rotate top clockwise"
rotateTopCounterC = Move topCC "Rotate top counter clockwise"

topC :: Cube -> Cube
topC c = Cube { front = [(r!!0),(r!!1),(r!!2)] ++ drop 3 f
              , back = [(l!!0),(l!!1),(l!!2)] ++ drop 3 ba
              , left = [(f!!0),(f!!1),(f!!2)] ++ drop 3 l
              , right = [(ba!!0),(ba!!1),(ba!!2)] ++ drop 3 r
              , top = permuteFaceRight (top c)
              , bottom = bottom c
              }
  where f = front c
        l = left c
        r = right c
        ba = back c

topCC = topC . topC . topC

-----------------------------

rotateYAxisBack = Move yAxisBackward "Rotate vertical middle back"
rotateYAxisForward = Move yAxisForward "Rotate vertical middle forward"

yAxisBackward :: Cube -> Cube
yAxisBackward c = Cube { front = [(f!!0),(b!!1),(f!!2),(f!!3),(b!!4),(f!!5),(f!!6),(b!!7),(f!!8)]
                       , back = [(ba!!0),(t!!7),(ba!!2),(ba!!3),(t!!4),(ba!!5),(ba!!6),(t!!1),(ba!!8)] 
                       , left = left c
                       , right = right c
                       , top = [(t!!0),(f!!1),(t!!2),(t!!3),(f!!4),(t!!5),(t!!6),(f!!7),(t!!8)]
                       , bottom = [(b!!0),(ba!!7),(b!!2),(b!!3),(ba!!4),(b!!5),(b!!6),(ba!!1),(b!!8)]
                       }
  where f = front c
        t = top c
        b = bottom c
        ba = back c

yAxisForward = yAxisBackward . yAxisBackward . yAxisBackward

-----------------------------

rotateXAxisRight = Move xAxisRight "Rotate horizontal middle right"
rotateXAxisLeft = Move xAxisLeft "Rotate horizontal middle left"

xAxisRight :: Cube -> Cube
xAxisRight c = Cube { front = take 3 f ++ [(l!!3),(l!!4),(l!!5)] ++ drop 6 f
                    , back = take 3 ba ++ [(r!!3),(r!!4),(r!!5)] ++ drop 6 ba
                    , left = take 3 l ++ [(ba!!3),(ba!!4),(ba!!5)] ++ drop 6 l
                    , right = take 3 r ++ [(f!!3),(f!!4),(f!!5)] ++ drop 6 r
                    , top = top c
                    , bottom = bottom c
                    }
  where f = front c
        l = left c
        r = right c
        ba = back c

xAxisLeft = xAxisRight . xAxisRight . xAxisRight

-----------------------------

rotateZAxisRight = Move zAxisRight "Rotate z axis right"
rotateZAxisLeft = Move zAxisLeft "Rotate z axis left"

zAxisRight :: Cube -> Cube
zAxisRight c = Cube { front = front c
                    , back = back c
                    , left = [(l!!0),(b!!3),(l!!2),(l!!3),(b!!4),(l!!5),(l!!6),(b!!5),(l!!8)] 
                    , right = [(r!!0),(t!!3),(r!!2),(r!!3),(t!!4),(r!!5),(r!!6),(t!!5),(r!!8)]
                    , top = take 3 t ++ [(l!!7),(l!!4),(l!!1)] ++ drop 6 t
                    , bottom = take 3 b ++ [(r!!7),(r!!4),(r!!1)] ++ drop 6 b
                    }
  where t = top c
        l = left c
        r = right c
        b = bottom c
   
zAxisLeft = zAxisRight . zAxisRight . zAxisRight

-----------------------------

-- Front has green center
fronts  = [Green, Green, Blue, Orange, Green, Red, Yellow, White, Yellow]
backs   = [Red, Orange, Red, Blue, Blue, Red, Orange, Blue, Green]
lefts   = [Yellow, Red, Orange, Blue, Orange, Green, Red, Green, Green]
rights  = [White, Yellow, White, Yellow, Red, Yellow, Blue, Blue, White]
bottoms = [Orange, Green, Orange, Red, Yellow, Orange, Yellow, White, Blue]
tops    = [Blue, White, White, White, White, Orange, White, Yellow, Red]

cube = Cube { front = fronts
            , back = backs
            , left = lefts
            , right = rights
            , bottom = bottoms
            , top = tops
            }

solutions :: [[[Move]]]
solutions = map checkForSolutions $ drop 15 buildAll

checkForSolutions :: [[Move]] -> [[Move]]
checkForSolutions xs =  filter (\x -> isSolved (applyMoves x cube)) xs

main = do
  putStrLn "Starting up"
  putStrLn $ show solutions
