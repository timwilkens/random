import qualified Data.Sequence as S
import Data.Foldable (toList)
import System.Environment

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

data Cube = Cube { front :: S.Seq Color
                 , back :: S.Seq Color
                 , left :: S.Seq Color
                 , right :: S.Seq Color
                 , top :: S.Seq Color
                 , bottom :: S.Seq Color
                 }
  deriving (Show, Eq)

data Move = Move (Cube -> Cube) String

instance Eq Move where
  (Move f s) == (Move f' s') = (s == s')

instance Show Move where
  show (Move f s) = show s

cubeListOrder :: [String]
cubeListOrder = ["front", "back", "left", "right", "top", "bottom"]

cubeToSeq :: Cube -> [S.Seq Color]
cubeToSeq c = [front c, back c, left c, right c, top c, bottom c]

isSolved :: Cube -> Bool
isSolved c = isSolved' $ cubeToSeq c
  where isSolved' xs = all (==True) (map solvedSide xs)

solvedSide :: S.Seq Color -> Bool
solvedSide xs = all (==True) (map (== h) end)
  where h = S.index xs 0
        end = toList $ S.drop 1 xs

cubeString :: Cube -> String
cubeString c = foldr (++) "" (map entitle $ zip cubeListOrder (cubeToSeq c))
  where entitle (a,b) = "[" ++ a ++ "]\n" ++ (sideAsString b)

sideAsString :: S.Seq Color -> String
sideAsString s = asString (toList s) 1
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
moves = [rotateFrontRight, rotateFrontLeft, rotateZAxisRight, rotateZAxisLeft,
         rotateYAxisForward, rotateYAxisBack, rotateTopClockwise, rotateTopCounterC,
         rotateLeftForward, rotateLeftBack, rotateXAxisRight, rotateXAxisLeft]

base :: [[Move]]
base = addMoves []

buildMovesOfNPlusOne :: [[Move]] -> [[Move]]
buildMovesOfNPlusOne (x:xs) = buildMoves (x:xs)
buildMovesOfNPlusOne [] = base

addMoves :: [Move] -> [[Move]]
addMoves m = filter noRepeats $ map (\x -> m ++ [x]) moves

buildMoves :: [[Move]] -> [[Move]]
buildMoves m = concatMap addMoves m

solve :: Cube -> Int -> [[Move]]
solve c maxDepth = case isSolved c of
  -- Return early for already solved cube.
  True ->  []
  False -> solve' c 1 maxDepth base

solve' :: Cube -> Int -> Int -> [[Move]] -> [[Move]]
solve' c n max moves
  | n >= max = []
  | otherwise = if length solutions > 0
                   then solutions
                   else solve' c (n + 1) max (buildMovesOfNPlusOne moves)
                     where solutions = filter (\x -> isSolved (applyMoves x c)) moves

noRepeats :: [Move] -> Bool
noRepeats x = noMoveUndo x' && noMoveCycle x'
  where x' = reverse x

noMoveCycle :: [Move] -> Bool
noMoveCycle (a:b:c:d:_) = 
  if (a == b) && (b == c) && (c == d)
    then False
    else True
noMoveCycle _ = True

noMoveUndo :: [Move] -> Bool
noMoveUndo (x:y:_) = 
  if x == (opposite y)
    then False
    else True
noMoveUndo _ = True

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
                    , left = S.update 8 (S.index b 2) (S.update 5 (S.index b 1) (S.update 2 (S.index b 0) l))
                    , right = S.update 6 (S.index t 8) (S.update 3 (S.index t 7) (S.update 0 (S.index t 6) r))
                    , top = S.update 8 (S.index l 2) (S.update 7 (S.index l 5) (S.update 6 (S.index l 8) t))
                    , bottom = S.update 2 (S.index r 0) (S.update 1 (S.index r 3) (S.update 0 (S.index r 6) b))
                    }
  where b = bottom c
        t = top c
        l = left c
        r = right c

frontLeft :: Cube -> Cube
frontLeft = frontRight . frontRight . frontRight


-- Expensive
permuteFaceRight xs = permuteFaceRight' (toList xs)
  where permuteFaceRight' (a:b:c:d:e:f:g:h:i:[]) = S.fromList [g, d, a, h, e, b, i, f, c]

-----------------------------

rotateLeftBack = Move leftBackward "Rotate left side backward"
rotateLeftForward = Move leftForward "Rotate left side forward"

leftForward :: Cube -> Cube
leftForward c = Cube { front = S.update 6 (S.index t 6) (S.update 3 (S.index t 3) (S.update 0 (S.index t 0) f))
                     , back = S.update 8 (S.index b 0) (S.update 5 (S.index b 3) (S.update 2 (S.index b 6) ba))
                     , left = permuteFaceRight (left c)
                     , right = right c
                     , top = S.update 6 (S.index ba 2) (S.update 3 (S.index ba 5) (S.update 0 (S.index ba 8) t))
                     , bottom = S.update 6 (S.index f 6) (S.update 3 (S.index f 3) (S.update 0 (S.index f 0) b))
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
topC c = Cube { front = (S.take 3 r) S.>< (S.drop 3 f)
              , back = (S.take 3 l) S.>< (S.drop 3 ba)
              , left = (S.take 3 f) S.>< (S.drop 3 l)
              , right = (S.take 3 ba) S.>< (S.drop 3 r)
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
yAxisBackward c = Cube { front = S.update 7 (S.index b 7) (S.update 4 (S.index b 4) (S.update 1 (S.index b 1) f))
                       , back = S.update 7 (S.index t 1) (S.update 4 (S.index t 4) (S.update 1 (S.index t 7) ba))
                       , left = left c
                       , right = right c
                       , top = S.update 7 (S.index f 7) (S.update 4 (S.index f 4) (S.update 1 (S.index f 1) t))
                       , bottom = S.update 7 (S.index ba 1) (S.update 4 (S.index ba 4) (S.update 1 (S.index ba 7) b))
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
xAxisRight c = Cube { front = S.update 5 (S.index l 5) (S.update 4 (S.index l 4) (S.update 3 (S.index l 3) f))
                    , back = S.update 5 (S.index r 5) (S.update 4 (S.index r 4) (S.update 3 (S.index r 3) ba))
                    , left = S.update 5 (S.index ba 5) (S.update 4 (S.index ba 4) (S.update 3 (S.index ba 3) l))
                    , right = S.update 5 (S.index f 5) (S.update 4 (S.index f 4) (S.update 3 (S.index f 3) r))
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
                    , left = S.update 7 (S.index b 5) (S.update 4 (S.index b 4) (S.update 1 (S.index b 3) l))
                    , right = S.update 7 (S.index t 5) (S.update 4 (S.index t 4) (S.update 1 (S.index t 3) r))
                    , top = S.update 5 (S.index l 1) (S.update 4 (S.index l 4) (S.update 3 (S.index l 7) t))
                    , bottom = S.update 5 (S.index r 1) (S.update 4 (S.index r 4) (S.update 3 (S.index r 7) b))
                    }
  where t = top c
        l = left c
        r = right c
        b = bottom c
   
zAxisLeft = zAxisRight . zAxisRight . zAxisRight

-----------------------------

-- Front has green center
fronts  = S.fromList [Green, Green, Blue, Orange, Green, Red, Yellow, White, Yellow]
backs   = S.fromList [Red, Orange, Red, Blue, Blue, Red, Orange, Blue, Green]
lefts   = S.fromList [Yellow, Red, Orange, Blue, Orange, Green, Red, Green, Green]
rights  = S.fromList [White, Yellow, White, Yellow, Red, Yellow, Blue, Blue, White]
bottoms = S.fromList [Orange, Green, Orange, Red, Yellow, Orange, Yellow, White, Blue]
tops    = S.fromList [Blue, White, White, White, White, Orange, White, Yellow, Red]

cube = Cube { front = fronts
            , back = backs
            , left = lefts
            , right = rights
            , bottom = bottoms
            , top = tops
            }

solvedCube = Cube { front = S.fromList $ replicate 9 Green
            , back = S.fromList $ replicate 9 Blue
            , left = S.fromList $ replicate 9 Orange
            , right = S.fromList $ replicate 9 Red
            , bottom = S.fromList $ replicate 9 Yellow
            , top = S.fromList $ replicate 9 White
            }

slightlyScrambled :: Cube
slightlyScrambled = yAxisBackward $ frontLeft $ leftBackward $ xAxisRight $ topCC $ zAxisLeft solvedCube

main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Must provide max depth as argument"
    xs -> do let maxDepth = read (head xs)
             putStrLn $ "Setting max depth to " ++ show maxDepth
             putStrLn $ show $ solve slightlyScrambled maxDepth
