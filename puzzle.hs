--install cabal https://www.haskell.org/cabal/download.html
--cabal install matrix
import System.IO
import Data.Matrix

--zapytanie o nazwę pliku
getFileName = do
    putStrLn ("Type file name")
    input <- getLine
    return input

--wczytanie pliku (dowolny rozmiar planszy)
{-loadFile = do
    fileName <- getFileName
-}

sampleRows = [1, 0, 2, 1, 2, 1]
sampleColumns = [1, 1, 2, 1, 1, 1]
sampleHouses = [(0, 1), (3, 2), (3, 4), (4, 0), (4, 4), (5, 2), (5, 5)]--row, col

sampleData = initPuzzleData sampleRows sampleColumns sampleHouses

{-
Reprezentacja danych na planszy
H - domek
X - puste pole
L - zbiornik przyłączony do domku po lewej
R, U, D - analogicznie
-}

--znalezienie rozwiązania
--findSolution :: PuzzleData -> PuzzleData

--findSolution puzzleData =

{-
wykreśl pola, które nie sąsiadują z żadnym domkiem

domek styka się bokiem z jednym pustym polem -> postaw tam zbiornik z gazem i powykreślaj pola dookoła zbiornika
jeśli w rzędzie jest tyle zbiorników ile w opisie rzędu - poskreślaj puste pola
-}

--findSolution inRows inColumns houses =

--numElementsInCol elem col

crossOutColumn :: Int -> PuzzleData -> PuzzleData
crossOutColumn col (PuzzleData rows cols pdata) = crossOutColumn' (length cols -1) col (PuzzleData rows cols pdata)--Length od dobrej wartosci?

crossOutColumn' :: Int -> Int -> PuzzleData -> PuzzleData
crossOutColumn' row col pdata
    | row == -1 = pdata
    | getElement (row, col) pdata == Just ' ' = placeElement (row, col) 'X' (crossOutColumn' (row-1) col pdata)
    | otherwise = crossOutColumn' (row-1) col pdata

crossOutRow :: Int -> PuzzleData -> PuzzleData
crossOutRow row (PuzzleData rows cols pdata) = crossOutRow' row (length rows -1) (PuzzleData rows cols pdata)--Length od dobrej wartosci?

crossOutRow' :: Int -> Int -> PuzzleData -> PuzzleData
crossOutRow' row col pdata
    | col == -1 = pdata
    | getElement (row, col) pdata == Just ' ' = placeElement (row, col) 'X' (crossOutRow' row (col-1) pdata)
    | otherwise = crossOutRow' row (col-1) pdata

crossOutNoNeighbour :: PuzzleData -> PuzzleData
crossOutNoNeighbour (PuzzleData rows cols pdata) = crossOutNoNeighbour' (length cols-1) (length rows-1) (PuzzleData rows cols pdata)--Length od dobrej wartosci?

crossOutNoNeighbour' :: Int -> Int -> PuzzleData -> PuzzleData
crossOutNoNeighbour' row col (PuzzleData rows cols pdata)
    | col == -1 = PuzzleData rows cols pdata
    | row == -1 = crossOutNoNeighbour' (length cols-1) (col-1) (PuzzleData rows cols pdata)--Length od dobrej wartosci?
    | checkCrossOut row col (PuzzleData rows cols pdata) = placeElement (row, col) 'X' (crossOutNoNeighbour' (row -1) col (PuzzleData rows cols pdata))
    | otherwise = crossOutNoNeighbour' (row - 1) col (PuzzleData rows cols pdata)

checkCrossOut  :: Int -> Int -> PuzzleData -> Bool
checkCrossOut row col pdata
    | Just ' ' /= getElement (row, col) pdata = False
    | Just 'H' == getElement (row, (col - 1)) pdata = False
    | Just 'H' == getElement (row, (col + 1)) pdata = False
    | Just 'H' == getElement ((row - 1), col) pdata = False
    | Just 'H' == getElement ((row + 1), col) pdata = False
    | otherwise = True

--wyświetlenie rozwiązania
displaySolution :: PuzzleData -> IO()
displaySolution (PuzzleData _ _ pdata) = do putStr $ prettyMatrix pdata

--możliwość zapisania rozwiązania do pliku o podanej nazwie
--saveToFile

data PuzzleData = PuzzleData [Int][Int](Matrix Char)
initPuzzleData :: [Int] -> [Int] -> [(Int, Int)] -> PuzzleData
initPuzzleData a b h =
    PuzzleData a b (fromList (length b) (length a) [' ', ' '..])

placeHouses :: [(Int, Int)] -> PuzzleData -> PuzzleData
placeHouses [] pdata = pdata
placeHouses (x:xs) pdata =
    placeHouses xs (placeElement x 'H' pdata)

placeElement :: (Int, Int) -> Char -> PuzzleData -> PuzzleData
placeElement (row, col) elem (PuzzleData a b pdata) =
    PuzzleData a b (setElem elem (row+1, col+1) pdata)

getElement :: (Int, Int) -> PuzzleData -> Maybe Char
getElement (row, col) (PuzzleData _ _ pdata) = safeGet (row+1) (col+1) pdata